//! Single-flight TTL cache: a value map plus an in-flight marker set that
//! deduplicates concurrent fills of the same key.
//!
//! Two responsibilities:
//!
//! 1. **Cache** — keep a filled value around until it expires, so a
//!    flurry of requests for the same key doesn't re-run the expensive
//!    fill on every one. Entries carry an absolute `expires_at_secs`
//!    wall-clock deadline; the caller chooses the TTL when it publishes.
//!
//! 2. **Single-flight dedup** — when several concurrent callers ask for
//!    the same cold key, only the first is told to fill it. The rest
//!    observe an in-flight marker and can re-check the cache once it
//!    publishes, rather than each launching its own expensive fill.
//!
//! Every [`lookup`](SingleFlightCache::lookup) resolves to one of three
//! states — the "temperature" of the key:
//!
//! - **Warm** — a fresh value is cached; use it.
//! - **Pending** — another caller is already filling this key; wait and
//!   re-check.
//! - **Cold** — nothing fresh and no live fill; *you* own the fill.
//!
//! This type is deliberately just data: a value map plus a marker set. It
//! holds no `Waker`s, no shared futures, nothing that drives one caller's
//! task from another's. How a `Pending` caller waits — poll on a timer,
//! yield a round, block on a channel — is the consumer's decision, made
//! in the consumer's own execution context. (On the single-threaded IC
//! executor that separation is load-bearing: waking another call's task
//! runs it to completion *inside the waker's call context* and mis-routes
//! its reply, so consumers there re-check from their own context instead
//! of being woken. See `crate::doh::fetch_txt` for that pattern.)

use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

/// Default staleness window for an in-flight marker: how long it may sit
/// before a new arrival treats the fill as abandoned and takes it over.
///
/// Pick a value comfortably longer than a healthy fill takes but short
/// enough that a genuinely wedged fill clears within a few retries.
/// 120 s suits a fill bounded by a handful of HTTP outcalls; tune it per
/// use case with [`SingleFlightCache::new`].
pub const DEFAULT_PENDING_STALE_AFTER_SECS: u64 = 120;

/// One cache entry — the value and the wall-clock time at which it goes
/// stale.
struct Entry<V> {
    value: V,
    expires_at_secs: u64,
}

/// Marker for an in-flight fill. `fill_id` identifies *this* attempt so
/// [`SingleFlightCache::publish`] can tell whether a stale-takeover has
/// since replaced it; `started_at_secs` drives the staleness check.
struct InFlight {
    fill_id: u64,
    started_at_secs: u64,
}

/// In-memory single-flight cache keyed by `K`, holding values of type `V`.
pub struct SingleFlightCache<K, V> {
    entries: HashMap<K, Entry<V>>,
    in_flight: HashMap<K, InFlight>,
    next_fill_id: u64,
    /// How long an in-flight marker may sit before a new arrival presumes
    /// it abandoned and takes the fill over. See
    /// [`DEFAULT_PENDING_STALE_AFTER_SECS`].
    pending_stale_after_secs: u64,
}

impl<K, V> SingleFlightCache<K, V> {
    /// Create a cache whose in-flight markers are presumed abandoned after
    /// `pending_stale_after_secs` (see [`DEFAULT_PENDING_STALE_AFTER_SECS`]
    /// for guidance on the value).
    pub fn new(pending_stale_after_secs: u64) -> Self {
        Self {
            entries: HashMap::new(),
            in_flight: HashMap::new(),
            next_fill_id: 0,
            pending_stale_after_secs,
        }
    }
}

impl<K, V> Default for SingleFlightCache<K, V> {
    /// A cache with the default staleness window
    /// ([`DEFAULT_PENDING_STALE_AFTER_SECS`]).
    fn default() -> Self {
        Self::new(DEFAULT_PENDING_STALE_AFTER_SECS)
    }
}

impl<K: Eq + Hash, V> SingleFlightCache<K, V> {
    /// Borrow a fresh value. Returns `None` if missing or stale.
    ///
    /// Part of the reusable cache surface; the first consumer (DoH) drives
    /// reads through [`lookup`](Self::lookup), so this has no non-test
    /// caller yet.
    #[allow(dead_code)]
    pub fn get_fresh<Q>(&self, key: &Q, now_secs: u64) -> Option<&V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        let entry = self.entries.get(key)?;
        if entry.expires_at_secs > now_secs {
            Some(&entry.value)
        } else {
            None
        }
    }

    /// Insert a value that expires at the given wall-clock time.
    pub fn insert(&mut self, key: K, value: V, expires_at_secs: u64) {
        self.entries.insert(
            key,
            Entry {
                value,
                expires_at_secs,
            },
        );
    }

    /// Remove a value entry (by key). Leaves any in-flight marker alone.
    ///
    /// Part of the reusable cache surface; no non-test caller yet (see
    /// [`get_fresh`](Self::get_fresh)).
    #[allow(dead_code)]
    pub fn invalidate<Q>(&mut self, key: &Q)
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        self.entries.remove(key);
    }

    /// Decide what an incoming request for `key` should do:
    ///
    /// - [`CacheState::Warm`]`(value)` — cache fresh, use immediately.
    /// - [`CacheState::Pending`] — another caller is filling this key;
    ///   wait and re-check. The consumer chooses how to wait (this type
    ///   does not drive the waiter — see the module docs). Returned only
    ///   for a *fresh* in-flight marker.
    /// - [`CacheState::Cold`]`(token)` — you own the fill; do the work,
    ///   then call [`Self::publish`] with the token. Returned when there's
    ///   no in-flight marker, or the existing one is older than the
    ///   configured staleness window (presumed abandoned — you take it
    ///   over). Marks a fresh in-flight entry for you.
    ///
    /// Also opportunistically evicts the looked-up value entry if it's
    /// expired — keeps the cache from accumulating dead entries for keys
    /// queried once and then gone silent.
    pub fn lookup<Q>(&mut self, key: &Q, now_secs: u64) -> CacheState<V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
        V: Clone,
    {
        if let Some(entry) = self.entries.get(key) {
            if entry.expires_at_secs > now_secs {
                return CacheState::Warm(entry.value.clone());
            }
            // Expired: drop it now rather than waiting for a publish to
            // overwrite — most expired entries never see a republish.
            self.entries.remove(key);
        }
        if let Some(inflight) = self.in_flight.get(key) {
            let age = now_secs.saturating_sub(inflight.started_at_secs);
            if age < self.pending_stale_after_secs {
                return CacheState::Pending;
            }
            // Stale: the owner likely died before publishing. Fall through
            // and take the fill over — the insert below overwrites the
            // abandoned marker with a fresh `fill_id`, so the old owner's
            // late `publish` becomes a no-op.
        }
        let fill_id = self.next_fill_id;
        self.next_fill_id = self.next_fill_id.wrapping_add(1);
        self.in_flight.insert(
            key.to_owned(),
            InFlight {
                fill_id,
                started_at_secs: now_secs,
            },
        );
        CacheState::Cold(FillToken { fill_id })
    }

    /// Publish the outcome of the fill you owned. `value` is `Some` on
    /// success (the value entry is written) and `None` on failure (nothing
    /// is cached). Either way your in-flight marker is cleared — but only
    /// if it's still *yours* (`token.fill_id` matches the current marker).
    /// If a stale-takeover replaced you, the new owner's marker and any
    /// value entry are left untouched, so a late publish from a superseded
    /// fill can't clobber fresher state.
    ///
    /// Also opportunistically sweeps expired value entries (publish is the
    /// rare write path, so this keeps reads cheap while bounding cache
    /// size — every successful fill drops the dead weight accumulated
    /// since the last write).
    pub fn publish<Q>(
        &mut self,
        key: &Q,
        token: FillToken,
        value: Option<V>,
        expires_at_secs: u64,
        now_secs: u64,
    ) where
        K: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
    {
        self.sweep_expired(now_secs);

        let still_ours = self
            .in_flight
            .get(key)
            .is_some_and(|f| f.fill_id == token.fill_id);
        if still_ours {
            if let Some(value) = value {
                self.insert(key.to_owned(), value, expires_at_secs);
            }
            self.in_flight.remove(key);
        }
    }

    /// Drop every value entry whose `expires_at_secs` is at or before
    /// `now_secs`. Linear in `entries.len()`, which is bounded by the
    /// number of distinct keys filled within the TTL window — small in
    /// practice.
    fn sweep_expired(&mut self, now_secs: u64) {
        self.entries.retain(|_, e| e.expires_at_secs > now_secs);
    }
}

/// What [`SingleFlightCache::lookup`] returned — the temperature of the
/// key. The caller acts on the variant.
pub enum CacheState<V> {
    /// A fresh value is cached.
    Warm(V),
    /// Another caller is filling this key; wait and re-check.
    Pending,
    /// Nothing fresh and no live fill; the holder owns the fill.
    Cold(FillToken),
}

/// Opaque handle a [`CacheState::Cold`] caller passes back to
/// [`SingleFlightCache::publish`]. Identifies this particular in-flight
/// attempt, so `publish` can tell whether the caller is still the owner or
/// was superseded by a stale-takeover.
pub struct FillToken {
    fill_id: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: take the `Cold` arm or fail the test.
    fn expect_cold<V>(state: CacheState<V>) -> FillToken {
        match state {
            CacheState::Cold(t) => t,
            _ => panic!("expected Cold"),
        }
    }

    #[test]
    fn cache_hit_inside_window() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        cache.insert("a.com".to_string(), "hello", 1000);
        assert_eq!(cache.get_fresh("a.com", 999), Some(&"hello"));
    }

    #[test]
    fn cache_miss_when_expired() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        cache.insert("a.com".to_string(), "hello", 1000);
        assert!(cache.get_fresh("a.com", 1001).is_none());
    }

    #[test]
    fn invalidate_removes_entry() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        cache.insert("a.com".to_string(), "hello", 1000);
        cache.invalidate("a.com");
        assert!(cache.get_fresh("a.com", 0).is_none());
    }

    #[test]
    fn lookup_returns_warm_on_fresh_entry() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        cache.insert("a.com".to_string(), "hello", 1000);
        match cache.lookup("a.com", 999) {
            CacheState::Warm(v) => assert_eq!(v, "hello"),
            _ => panic!("expected Warm"),
        }
    }

    #[test]
    fn first_lookup_returns_cold_and_marks_in_flight() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        let _token = expect_cold(cache.lookup("a.com", 0));
        assert!(cache.in_flight.contains_key("a.com"));
    }

    #[test]
    fn second_concurrent_lookup_returns_pending() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        let _token = expect_cold(cache.lookup("a.com", 0));
        assert!(matches!(cache.lookup("a.com", 0), CacheState::Pending));
    }

    #[test]
    fn publish_success_writes_entry_and_clears_in_flight() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        let token = expect_cold(cache.lookup("a.com", 0));
        cache.publish("a.com", token, Some("hello"), 1000, 0);

        assert!(!cache.in_flight.contains_key("a.com"));
        assert_eq!(cache.get_fresh("a.com", 999), Some(&"hello"));
        // A later arrival is a straight hit — no second fill.
        assert!(matches!(cache.lookup("a.com", 999), CacheState::Warm(_)));
    }

    #[test]
    fn publish_failure_does_not_cache_but_clears_in_flight() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        let token = expect_cold(cache.lookup("a.com", 0));
        cache.publish("a.com", token, None, 1000, 0);

        assert!(cache.get_fresh("a.com", 0).is_none());
        assert!(!cache.in_flight.contains_key("a.com"));
        // The next arrival starts a fresh fill rather than waiting.
        assert!(matches!(cache.lookup("a.com", 0), CacheState::Cold(_)));
    }

    // ---- Stale in-flight takeover ----

    #[test]
    fn fresh_in_flight_within_threshold_returns_pending() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        let _orig = expect_cold(cache.lookup("a.com", 100));
        // Just under the threshold: still fresh.
        assert!(matches!(
            cache.lookup("a.com", 100 + DEFAULT_PENDING_STALE_AFTER_SECS - 1),
            CacheState::Pending
        ));
    }

    #[test]
    fn stale_in_flight_is_taken_over_with_cold() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        let _abandoned = expect_cold(cache.lookup("a.com", 100));
        // Past the staleness threshold — the prior fill is presumed dead,
        // so the new arrival takes over with a fresh Cold.
        let _new = expect_cold(cache.lookup("a.com", 100 + DEFAULT_PENDING_STALE_AFTER_SECS));
    }

    #[test]
    fn configured_staleness_window_is_respected() {
        // A short window means a fill is presumed abandoned sooner.
        let mut cache = SingleFlightCache::<String, &str>::new(10);
        let _abandoned = expect_cold(cache.lookup("a.com", 100));
        assert!(matches!(cache.lookup("a.com", 109), CacheState::Pending));
        let _new = expect_cold(cache.lookup("a.com", 110));
    }

    #[test]
    fn superseded_owner_publish_does_not_overwrite_new_fill() {
        // A starts a fill; it's presumed abandoned and C takes over. A then
        // returns late and publishes. The cache must NOT write A's value or
        // clear C's in-flight marker.
        let mut cache = SingleFlightCache::<String, &str>::default();
        let a_token = expect_cold(cache.lookup("a.com", 100));
        let c_token =
            expect_cold(cache.lookup("a.com", 100 + DEFAULT_PENDING_STALE_AFTER_SECS + 1));

        cache.publish("a.com", a_token, Some("A's stale data"), 9999, 0);
        assert!(
            cache.get_fresh("a.com", 0).is_none(),
            "A's late publish must not write to the cache after takeover"
        );
        assert!(
            cache.in_flight.contains_key("a.com"),
            "C's in-flight marker must still be in place"
        );

        // C publishes its real result and that DOES land.
        cache.publish("a.com", c_token, Some("C's fresh data"), 9999, 0);
        assert_eq!(cache.get_fresh("a.com", 0), Some(&"C's fresh data"));
        assert!(!cache.in_flight.contains_key("a.com"));
    }

    // ---- Eviction / sweeping of expired value entries ----

    #[test]
    fn lookup_evicts_expired_entry() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        cache.insert("a.com".to_string(), "x", 100);
        // Lookup at a time past the expiry — entry should be removed.
        let _ = cache.lookup("a.com", 200);
        assert!(
            !cache.entries.contains_key("a.com"),
            "expired entry should be evicted on lookup"
        );
    }

    #[test]
    fn publish_sweeps_other_expired_entries() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        // Three pre-existing entries, two expired and one fresh.
        cache.insert("expired1.com".to_string(), "x", 50);
        cache.insert("expired2.com".to_string(), "y", 60);
        cache.insert("fresh.com".to_string(), "z", 1_000);

        // Publishing for a fourth key at now=100 should drop both expired
        // entries while leaving the fresh one alone.
        let token = expect_cold(cache.lookup("new.com", 100));
        cache.publish("new.com", token, Some("v"), 2_000, 100);

        assert!(!cache.entries.contains_key("expired1.com"));
        assert!(!cache.entries.contains_key("expired2.com"));
        assert!(cache.entries.contains_key("fresh.com"));
        assert!(cache.entries.contains_key("new.com"));
    }
}
