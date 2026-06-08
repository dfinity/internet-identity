//! Single-flight TTL cache for IC canister code: a value map plus an
//! in-flight marker set that deduplicates concurrent fills of the same
//! key, and drives the waiting itself.
//!
//! Two responsibilities:
//!
//! 1. **Cache** — keep a filled value around until it expires, so a
//!    flurry of requests for the same key doesn't re-run the expensive
//!    fill on every one. Entries carry an absolute `expires_at_secs`
//!    wall-clock deadline; the caller chooses the TTL.
//!
//! 2. **Single-flight dedup** — when several concurrent callers ask for
//!    the same cold key, only the first runs the fill. The rest observe
//!    an in-flight marker and wait for it to publish, rather than each
//!    launching its own expensive fill.
//!
//! [`get_or_fill`] is the entry point: hand it the cache, a key, a clock,
//! a TTL, and a fill future, and it returns the cached or freshly-filled
//! value. Internally each lookup resolves to one of three "temperatures":
//!
//! - **Warm** — a fresh value is cached; return it.
//! - **Pending** — another caller is filling this key; yield a round and
//!   re-check, bounded by [`MAX_WAIT_POLLS`].
//! - **Cold** — nothing fresh and no live fill; run the fill and publish.
//!
//! ## Why the waiter polls instead of being woken
//!
//! A `Pending` waiter does **not** block on a shared future that the
//! filler wakes. The canister runs a single wasm thread, and waking
//! another call's task runs that task to completion *inside the filler's
//! call context* — so the woken waiter's reply is routed to the wrong
//! call (`ic0.msg_reply ... already replied`) and the real owner is left
//! unreplied. Instead each waiter polls the cache from its OWN call
//! context, yielding a round via a cheap management-canister round-trip
//! ([`yield_round`]) between checks. The cache holds no `Waker`s and no
//! shared futures precisely so that nothing drives one call's task from
//! another's, and the [`get_or_fill`] loop releases the cache borrow
//! before every `.await` (a borrow held across a yield would trap on the
//! single-threaded executor).

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::hash::Hash;
use std::thread::LocalKey;

/// Default staleness window for an in-flight marker: how long it may sit
/// before a new arrival treats the fill as abandoned and takes it over.
///
/// The IC commits state changes made *before* an `.await`, so if a
/// filler's post-yield continuation traps its in-flight marker survives,
/// and later arrivals would otherwise wait on it forever; the staleness
/// window lets one take the fill over. Pick a value comfortably longer
/// than a healthy fill takes but short enough that a genuinely wedged
/// fill clears within a few retries. 120 s suits a fill bounded by a
/// handful of HTTP outcalls; tune it per use case with
/// [`SingleFlightCache::new`].
pub const DEFAULT_PENDING_STALE_AFTER_SECS: u64 = 120;

/// Upper bound on how many times a waiter polls before giving up with
/// [`CacheFillError::WaitTimedOut`]. Each poll yields one round via
/// [`yield_round`], so this is ~100 rounds of patience — far more than a
/// healthy fill needs — and bounds the wait so a wedged fill can't pin a
/// waiter indefinitely. The post-staleness takeover (see
/// [`DEFAULT_PENDING_STALE_AFTER_SECS`]) is the real recovery path; this
/// is just a backstop against an unbounded poll loop.
const MAX_WAIT_POLLS: u32 = 100;

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
    /// Insert a value that expires at the given wall-clock time.
    fn insert(&mut self, key: K, value: V, expires_at_secs: u64) {
        self.entries.insert(
            key,
            Entry {
                value,
                expires_at_secs,
            },
        );
    }

    /// Decide what an incoming request for `key` should do:
    ///
    /// - [`CacheState::Warm`]`(value)` — cache fresh, use immediately.
    /// - [`CacheState::Pending`] — another caller is filling this key.
    ///   Returned only for a *fresh* in-flight marker.
    /// - [`CacheState::Cold`]`(token)` — you own the fill; do the work,
    ///   then call [`Self::publish`] with the token. Returned when there's
    ///   no in-flight marker, or the existing one is older than the
    ///   configured staleness window (presumed abandoned — you take it
    ///   over). Marks a fresh in-flight entry for you.
    ///
    /// Also opportunistically evicts the looked-up value entry if it's
    /// expired — keeps the cache from accumulating dead entries for keys
    /// queried once and then gone silent.
    fn lookup<Q>(&mut self, key: &Q, now_secs: u64) -> CacheState<V>
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
            // late `publish` can't clobber the new owner's marker or value
            // (its only remaining effect is the harmless expired-entry
            // sweep).
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
    fn publish<Q>(
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
/// key.
enum CacheState<V> {
    /// A fresh value is cached.
    Warm(V),
    /// Another caller is filling this key.
    Pending,
    /// Nothing fresh and no live fill; the holder owns the fill.
    Cold(FillToken),
}

/// Opaque handle a [`CacheState::Cold`] caller passes back to
/// [`SingleFlightCache::publish`]. Identifies this particular in-flight
/// attempt, so `publish` can tell whether the caller is still the owner or
/// was superseded by a stale-takeover.
struct FillToken {
    fill_id: u64,
}

/// Why [`get_or_fill`] returned without a value.
pub enum CacheFillError<E> {
    /// The fill we owned failed; nothing was cached. Carries the fill's
    /// own error so the caller can react to it.
    Fill(E),
    /// Another caller's in-flight fill didn't publish within
    /// [`MAX_WAIT_POLLS`] yields. Transient — a retry, or the
    /// post-staleness takeover, resolves it.
    WaitTimedOut,
}

/// Get `key` from `cache`, or fill it once under single-flight dedup.
///
/// - **Warm:** returns the cached value without calling `fill`.
/// - **Cold:** runs `fill`, caches the value on success (TTL `ttl_secs`
///   from the post-fill clock reading), and returns it. On a `fill`
///   error nothing is cached and the error is returned as
///   [`CacheFillError::Fill`].
/// - **Pending:** another call owns the fill; yields a round and
///   re-checks, up to [`MAX_WAIT_POLLS`] times, then returns
///   [`CacheFillError::WaitTimedOut`].
///
/// `now` is read fresh on each loop turn (so the staleness check sees real
/// time advance across yields) and again after the fill to set the expiry.
/// The cache borrow is released before every `.await`, as the
/// single-threaded executor requires.
pub async fn get_or_fill<K, V, Q, E, Fut>(
    cache: &'static LocalKey<RefCell<SingleFlightCache<K, V>>>,
    key: &Q,
    now: impl Fn() -> u64,
    ttl_secs: u64,
    fill: impl FnOnce() -> Fut,
) -> Result<V, CacheFillError<E>>
where
    K: Eq + Hash + Borrow<Q>,
    Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
    V: Clone,
    Fut: Future<Output = Result<V, E>>,
{
    let mut waits = 0u32;
    let token = loop {
        let now_secs = now();
        match cache.with(|c| c.borrow_mut().lookup(key, now_secs)) {
            CacheState::Warm(value) => return Ok(value),
            CacheState::Cold(token) => break token,
            CacheState::Pending => {
                if waits >= MAX_WAIT_POLLS {
                    return Err(CacheFillError::WaitTimedOut);
                }
                waits += 1;
                yield_round().await;
            }
        }
    };

    // We own the fill. Run it, then publish (writes the value on success,
    // clears our in-flight marker either way).
    let result = fill().await;
    let now_secs = now();
    let expires_at = now_secs.saturating_add(ttl_secs);
    let to_cache = result.as_ref().ok().cloned();
    cache.with(|c| {
        c.borrow_mut()
            .publish(key, token, to_cache, expires_at, now_secs)
    });
    result.map_err(CacheFillError::Fill)
}

/// Yield one round, resuming in the caller's OWN call context.
///
/// A waiter uses this to let the in-flight fill make progress and then
/// re-check the cache, *without* being driven by the filler's wake (see
/// the module docs for why that would mis-route the reply). Awaiting a
/// cheap management-canister round-trip is the idiomatic way to give up a
/// round and come back in our own context; the random bytes are unused.
#[cfg(not(test))]
async fn yield_round() {
    let _ =
        ic_cdk::call::<(), (Vec<u8>,)>(candid::Principal::management_canister(), "raw_rand", ())
            .await;
}

/// Host-test stand-in: there is no executor to yield to, so a poll is a
/// no-op. The [`MAX_WAIT_POLLS`] counter still bounds the loop, so the
/// `Pending` timeout path is exercisable without an IC.
#[cfg(test)]
async fn yield_round() {}

#[cfg(test)]
mod tests {
    use super::*;
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::Arc;
    use std::task::{Context, Poll, Wake, Waker};

    /// Minimal block-on. Under `cfg(test)` [`yield_round`] is a no-op, so
    /// these futures complete in a single poll — no executor needed.
    struct NoopWaker;
    impl Wake for NoopWaker {
        fn wake(self: Arc<Self>) {}
    }
    fn block_on<F: Future>(mut f: F) -> F::Output {
        // SAFETY: `f` is on the stack and we don't move it out.
        let mut f = unsafe { Pin::new_unchecked(&mut f) };
        let waker = Waker::from(Arc::new(NoopWaker));
        let mut cx = Context::from_waker(&waker);
        match f.as_mut().poll(&mut cx) {
            Poll::Ready(out) => out,
            Poll::Pending => panic!("test future returned Pending — test setup is incorrect"),
        }
    }

    /// Read a fresh value out of the cache (test-only assertion helper).
    fn get_fresh<'a, K: Eq + Hash + Borrow<str>, V>(
        cache: &'a SingleFlightCache<K, V>,
        key: &str,
        now_secs: u64,
    ) -> Option<&'a V> {
        cache
            .entries
            .get(key)
            .filter(|e| e.expires_at_secs > now_secs)
            .map(|e| &e.value)
    }

    /// Helper: take the `Cold` arm or fail the test.
    fn expect_cold<V>(state: CacheState<V>) -> FillToken {
        match state {
            CacheState::Cold(t) => t,
            _ => panic!("expected Cold"),
        }
    }

    // ---- Low-level lookup/publish state machine ----

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
    fn lookup_misses_when_expired() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        cache.insert("a.com".to_string(), "hello", 1000);
        assert!(matches!(cache.lookup("a.com", 1001), CacheState::Cold(_)));
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
        assert_eq!(get_fresh(&cache, "a.com", 999), Some(&"hello"));
        // A later arrival is a straight hit — no second fill.
        assert!(matches!(cache.lookup("a.com", 999), CacheState::Warm(_)));
    }

    #[test]
    fn publish_failure_does_not_cache_but_clears_in_flight() {
        let mut cache = SingleFlightCache::<String, &str>::default();
        let token = expect_cold(cache.lookup("a.com", 0));
        cache.publish("a.com", token, None, 1000, 0);

        assert!(get_fresh(&cache, "a.com", 0).is_none());
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
            get_fresh(&cache, "a.com", 0).is_none(),
            "A's late publish must not write to the cache after takeover"
        );
        assert!(
            cache.in_flight.contains_key("a.com"),
            "C's in-flight marker must still be in place"
        );

        // C publishes its real result and that DOES land.
        cache.publish("a.com", c_token, Some("C's fresh data"), 9999, 0);
        assert_eq!(get_fresh(&cache, "a.com", 0), Some(&"C's fresh data"));
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

    // ---- get_or_fill: the full lookup/wait/publish flow ----

    thread_local! {
        static TEST_CACHE: RefCell<SingleFlightCache<String, i32>> =
            RefCell::new(SingleFlightCache::default());
    }

    fn reset_test_cache() {
        TEST_CACHE.with(|c| *c.borrow_mut() = SingleFlightCache::default());
    }

    #[test]
    fn get_or_fill_warm_returns_cached_without_filling() {
        reset_test_cache();
        TEST_CACHE.with(|c| c.borrow_mut().insert("k".to_string(), 7, 1000));

        let r: Result<i32, CacheFillError<()>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 999,
            100,
            || async { panic!("fill must not run on a warm hit") },
        ));
        assert!(matches!(r, Ok(7)));
    }

    #[test]
    fn get_or_fill_cold_runs_fill_and_caches() {
        reset_test_cache();

        let r: Result<i32, CacheFillError<()>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 100,
            50,
            || async { Ok(42) },
        ));
        assert!(matches!(r, Ok(42)));

        // Second call is a warm hit — fill must not run again.
        let r2: Result<i32, CacheFillError<()>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 120,
            50,
            || async { panic!("fill must not run on a warm hit") },
        ));
        assert!(matches!(r2, Ok(42)));
    }

    #[test]
    fn get_or_fill_propagates_fill_error_and_caches_nothing() {
        reset_test_cache();

        let r: Result<i32, CacheFillError<&str>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 100,
            50,
            || async { Err("boom") },
        ));
        assert!(matches!(r, Err(CacheFillError::Fill("boom"))));
        // Nothing cached and no marker left behind: a retry fills afresh.
        let r2: Result<i32, CacheFillError<()>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 100,
            50,
            || async { Ok(1) },
        ));
        assert!(matches!(r2, Ok(1)));
    }

    #[test]
    fn get_or_fill_times_out_waiting_on_a_foreign_in_flight() {
        reset_test_cache();
        // Claim an in-flight marker we never publish, so every lookup for
        // "k" sees a fresh Pending (the clock is constant, so the marker
        // never goes stale and is never taken over).
        let _foreign = TEST_CACHE.with(|c| match c.borrow_mut().lookup("k", 100) {
            CacheState::Cold(t) => t,
            _ => panic!("expected Cold"),
        });

        let r: Result<i32, CacheFillError<()>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 100,
            50,
            || async { panic!("fill must not run while another fill is in flight") },
        ));
        assert!(matches!(r, Err(CacheFillError::WaitTimedOut)));
    }
}
