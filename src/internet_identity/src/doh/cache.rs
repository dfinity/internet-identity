//! In-memory DoH cache with concurrent-fetch deduplication.
//!
//! Two responsibilities:
//!
//! 1. **Cache** — keep a fetched TXT record around for as long as the
//!    deploy-arg `max_cache_age_secs` (default 1 h, capped at 24 h) so
//!    a flurry of mail from the same domain doesn't re-fetch on every
//!    message. The cache is heap-only — losing it on upgrade is fine,
//!    we just refetch the next time mail arrives.
//!
//! 2. **Dedup** — when several concurrent `smtp_request` calls arrive
//!    for the same domain and the cache is cold, only the first fans
//!    out to the providers. The rest observe an in-flight marker and
//!    re-check the cache once it publishes, rather than each firing
//!    their own five-provider fan-out.
//!
//! The waiters do **not** block on a shared future woken by the
//! fetcher. The canister runs a single wasm thread, and waking another
//! call's task runs that task to completion *inside the fetcher's call
//! context* — so the woken waiter's reply is routed to the wrong call
//! (`ic0.msg_reply ... already replied`) and the real owner is left
//! unreplied. Instead each waiter polls this cache from its OWN call
//! context, yielding a round via a cheap downstream call between checks
//! (see `super::fetch_txt`). This module is therefore just a value map
//! plus an in-flight marker set — no `Waker`s, no shared `RefCell`
//! futures, nothing that drives one call's task from another's.

use std::collections::HashMap;

use super::types::DohError;

/// How long an in-flight marker may sit before a new arrival treats the
/// fetch as abandoned and takes it over.
///
/// The IC commits state changes made *before* an `.await`, so if a
/// fetcher's post-outcall continuation traps, its in-flight marker
/// survives and later arrivals would otherwise `Wait` on it forever.
///
/// 120 s is comfortably longer than any plausible HTTP-outcall round
/// trip (the IC caps individual outcalls at ~5 minutes, but successful
/// ones complete in seconds; a 2-minute floor on "looks abandoned" is
/// safe for live fetches and quick enough that genuinely stuck entries
/// clear within a couple of email retries).
pub const PENDING_STALE_AFTER_SECS: u64 = 120;

/// One cache entry — bytes and the wall-clock time at which they go
/// stale.
#[derive(Clone, Debug)]
pub struct CacheEntry {
    pub bytes: Vec<u8>,
    pub expires_at_secs: u64,
}

/// Marker for an in-flight fetch. `fetch_id` identifies *this* attempt
/// so [`DohCache::publish`] can tell whether a stale-takeover has since
/// replaced it; `started_at_secs` drives the staleness check.
struct InFlight {
    fetch_id: u64,
    started_at_secs: u64,
}

/// In-memory cache, keyed by FQDN (e.g. `selector1._domainkey.gmail.com`).
#[derive(Default)]
pub struct DohCache {
    entries: HashMap<String, CacheEntry>,
    in_flight: HashMap<String, InFlight>,
    next_fetch_id: u64,
}

impl DohCache {
    /// Look up a fresh entry. Returns `None` if missing or stale.
    pub fn get_fresh(&self, name: &str, now_secs: u64) -> Option<Vec<u8>> {
        let entry = self.entries.get(name)?;
        if entry.expires_at_secs > now_secs {
            Some(entry.bytes.clone())
        } else {
            None
        }
    }

    /// Insert an entry that expires at the given wall-clock time.
    pub fn insert(&mut self, name: &str, bytes: Vec<u8>, expires_at_secs: u64) {
        self.entries.insert(
            name.to_string(),
            CacheEntry {
                bytes,
                expires_at_secs,
            },
        );
    }

    /// Remove a value entry (by name).
    pub fn invalidate(&mut self, name: &str) {
        self.entries.remove(name);
    }

    /// Decide what an incoming fetch request should do:
    /// - `Hit(bytes)` — cache fresh, use immediately.
    /// - `Wait` — another caller is fetching this name; yield a round
    ///   and re-check (the caller polls from its own context — see
    ///   `super::fetch_txt`). Returned only for a *fresh* in-flight
    ///   marker.
    /// - `Fetch(token)` — you own the fetch; fan out, then call
    ///   [`Self::publish`] with the token. Returned when there's no
    ///   in-flight marker, or the existing one is older than
    ///   [`PENDING_STALE_AFTER_SECS`] (presumed abandoned — you take it
    ///   over). Marks a fresh in-flight entry for you.
    ///
    /// Also opportunistically evicts the looked-up value entry if it's
    /// expired — keeps the cache from accumulating dead entries for
    /// FQDNs queried once and then gone silent.
    pub fn lookup(&mut self, name: &str, now_secs: u64) -> CacheLookup {
        if let Some(entry) = self.entries.get(name) {
            if entry.expires_at_secs > now_secs {
                return CacheLookup::Hit(entry.bytes.clone());
            }
            // Expired: drop it now rather than waiting for a publish to
            // overwrite — most expired entries never see a republish.
            self.entries.remove(name);
        }
        if let Some(inflight) = self.in_flight.get(name) {
            let age = now_secs.saturating_sub(inflight.started_at_secs);
            if age < PENDING_STALE_AFTER_SECS {
                return CacheLookup::Wait;
            }
            // Stale: the owner likely trapped before publishing. Fall
            // through and take the fetch over — the insert below
            // overwrites the abandoned marker with a fresh `fetch_id`,
            // so the old owner's late `publish` becomes a no-op.
        }
        let fetch_id = self.next_fetch_id;
        self.next_fetch_id = self.next_fetch_id.wrapping_add(1);
        self.in_flight.insert(
            name.to_string(),
            InFlight {
                fetch_id,
                started_at_secs: now_secs,
            },
        );
        CacheLookup::Fetch(FetchToken { fetch_id })
    }

    /// Publish the result of the fetch you owned. On success the value
    /// entry is written; either way your in-flight marker is cleared —
    /// but only if it's still *yours* (`token.fetch_id` matches the
    /// current marker). If a stale-takeover replaced you, we leave the
    /// new owner's marker and any value entry untouched, so a late
    /// publish from a superseded fetch can't clobber fresher state.
    ///
    /// Also opportunistically sweeps expired value entries (publish is
    /// the rare write path, so this keeps reads cheap while bounding
    /// cache size — every successful fetch drops the dead weight
    /// accumulated since the last write).
    pub fn publish(
        &mut self,
        name: &str,
        token: FetchToken,
        result: Result<Vec<u8>, DohError>,
        expires_at_secs: u64,
        now_secs: u64,
    ) {
        self.sweep_expired(now_secs);

        let still_ours = self
            .in_flight
            .get(name)
            .is_some_and(|f| f.fetch_id == token.fetch_id);
        if still_ours {
            if let Ok(bytes) = result {
                self.insert(name, bytes, expires_at_secs);
            }
            self.in_flight.remove(name);
        }
    }

    /// Drop every value entry whose `expires_at_secs` is at or before
    /// `now_secs`. Linear in `entries.len()`, which is bounded by
    /// "unique FQDNs queried in the last `max_cache_age_secs`" — small
    /// in practice.
    fn sweep_expired(&mut self, now_secs: u64) {
        self.entries.retain(|_, e| e.expires_at_secs > now_secs);
    }
}

/// What [`DohCache::lookup`] returned. The caller acts on the variant.
pub enum CacheLookup {
    Hit(Vec<u8>),
    Wait,
    Fetch(FetchToken),
}

/// Opaque handle a `Fetch` caller passes back to [`DohCache::publish`].
/// Identifies this particular in-flight attempt, so `publish` can tell
/// whether the caller is still the owner or was superseded by a
/// stale-takeover.
pub struct FetchToken {
    fetch_id: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: take the `Fetch` arm or fail the test.
    fn expect_fetch(lookup: CacheLookup) -> FetchToken {
        match lookup {
            CacheLookup::Fetch(t) => t,
            _ => panic!("expected Fetch"),
        }
    }

    #[test]
    fn cache_hit_inside_window() {
        let mut cache = DohCache::default();
        cache.insert("a.com", b"hello".to_vec(), 1000);
        assert_eq!(cache.get_fresh("a.com", 999), Some(b"hello".to_vec()));
    }

    #[test]
    fn cache_miss_when_expired() {
        let mut cache = DohCache::default();
        cache.insert("a.com", b"hello".to_vec(), 1000);
        assert!(cache.get_fresh("a.com", 1001).is_none());
    }

    #[test]
    fn lookup_returns_hit_on_fresh_entry() {
        let mut cache = DohCache::default();
        cache.insert("a.com", b"hello".to_vec(), 1000);
        match cache.lookup("a.com", 999) {
            CacheLookup::Hit(b) => assert_eq!(b, b"hello"),
            _ => panic!("expected Hit"),
        }
    }

    #[test]
    fn first_lookup_returns_fetch_and_marks_in_flight() {
        let mut cache = DohCache::default();
        let _token = expect_fetch(cache.lookup("a.com", 0));
        assert!(cache.in_flight.contains_key("a.com"));
    }

    #[test]
    fn second_concurrent_lookup_returns_wait() {
        let mut cache = DohCache::default();
        let _token = expect_fetch(cache.lookup("a.com", 0));
        assert!(matches!(cache.lookup("a.com", 0), CacheLookup::Wait));
    }

    #[test]
    fn publish_success_writes_entry_and_clears_in_flight() {
        let mut cache = DohCache::default();
        let token = expect_fetch(cache.lookup("a.com", 0));
        cache.publish("a.com", token, Ok(b"hello".to_vec()), 1000, 0);

        assert!(!cache.in_flight.contains_key("a.com"));
        assert_eq!(cache.get_fresh("a.com", 999), Some(b"hello".to_vec()));
        // A later arrival is a straight hit — no second fetch.
        assert!(matches!(cache.lookup("a.com", 999), CacheLookup::Hit(_)));
    }

    #[test]
    fn publish_failure_does_not_cache_but_clears_in_flight() {
        let mut cache = DohCache::default();
        let token = expect_fetch(cache.lookup("a.com", 0));
        cache.publish("a.com", token, Err(DohError::AllProvidersFailed), 1000, 0);

        assert!(cache.get_fresh("a.com", 0).is_none());
        assert!(!cache.in_flight.contains_key("a.com"));
        // The next arrival starts a fresh fetch rather than waiting.
        assert!(matches!(cache.lookup("a.com", 0), CacheLookup::Fetch(_)));
    }

    // ---- Stale in-flight takeover ----

    #[test]
    fn fresh_in_flight_within_threshold_returns_wait() {
        let mut cache = DohCache::default();
        let _orig = expect_fetch(cache.lookup("a.com", 100));
        // Just under the threshold: still fresh.
        assert!(matches!(
            cache.lookup("a.com", 100 + PENDING_STALE_AFTER_SECS - 1),
            CacheLookup::Wait
        ));
    }

    #[test]
    fn stale_in_flight_is_taken_over_with_fetch() {
        let mut cache = DohCache::default();
        let _abandoned = expect_fetch(cache.lookup("a.com", 100));
        // Past the staleness threshold — the prior fetch is presumed
        // dead, so the new arrival takes over with a fresh Fetch.
        let _new = expect_fetch(cache.lookup("a.com", 100 + PENDING_STALE_AFTER_SECS));
    }

    #[test]
    fn superseded_owner_publish_does_not_overwrite_new_fetch() {
        // A starts a fetch; it's presumed abandoned and C takes over. A
        // then returns late and publishes. The cache must NOT write A's
        // data or clear C's in-flight marker.
        let mut cache = DohCache::default();
        let a_token = expect_fetch(cache.lookup("a.com", 100));
        let c_token = expect_fetch(cache.lookup("a.com", 100 + PENDING_STALE_AFTER_SECS + 1));

        cache.publish("a.com", a_token, Ok(b"A's stale data".to_vec()), 9999, 0);
        assert!(
            cache.get_fresh("a.com", 0).is_none(),
            "A's late publish must not write to the cache after takeover"
        );
        assert!(
            cache.in_flight.contains_key("a.com"),
            "C's in-flight marker must still be in place"
        );

        // C publishes its real result and that DOES land.
        cache.publish("a.com", c_token, Ok(b"C's fresh data".to_vec()), 9999, 0);
        assert_eq!(
            cache.get_fresh("a.com", 0),
            Some(b"C's fresh data".to_vec())
        );
        assert!(!cache.in_flight.contains_key("a.com"));
    }

    // ---- Eviction / sweeping of expired value entries ----

    #[test]
    fn lookup_evicts_expired_entry() {
        let mut cache = DohCache::default();
        cache.insert("a.com", b"x".to_vec(), 100);
        // Lookup at a time past the expiry — entry should be removed.
        let _ = cache.lookup("a.com", 200);
        assert!(
            !cache.entries.contains_key("a.com"),
            "expired entry should be evicted on lookup"
        );
    }

    #[test]
    fn publish_sweeps_other_expired_entries() {
        let mut cache = DohCache::default();
        // Three pre-existing entries, two expired and one fresh.
        cache.insert("expired1.com", b"x".to_vec(), 50);
        cache.insert("expired2.com", b"y".to_vec(), 60);
        cache.insert("fresh.com", b"z".to_vec(), 1_000);

        // Publishing for a fourth name at now=100 should drop both
        // expired entries while leaving the fresh one alone.
        let token = expect_fetch(cache.lookup("new.com", 100));
        cache.publish("new.com", token, Ok(b"v".to_vec()), 2_000, 100);

        assert!(!cache.entries.contains_key("expired1.com"));
        assert!(!cache.entries.contains_key("expired2.com"));
        assert!(cache.entries.contains_key("fresh.com"));
        assert!(cache.entries.contains_key("new.com"));
    }
}
