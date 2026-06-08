//! Single-flight cache for IC canister code: stale-while-revalidate +
//! stale-if-error, with concurrent-fill dedup, exponential-backoff retry,
//! and an LRU size bound.
//!
//! [`get_or_fill`] is the entry point. Each key moves through these
//! "temperatures" (see the SSO readiness doc §5.2.2 for the full picture):
//!
//! - **Fresh** — a value cached within its `ttl`; returned with no fill.
//! - **Stale** — past `ttl` but still within the stale window; the old
//!   value is served while a refresh is attempted on demand (throttled by
//!   the retry backoff).
//! - **Cold** — nothing servable; the caller owns a (blocking) fill.
//! - A fill/refresh **in flight** makes every concurrent caller wait and
//!   converge on the same result.
//!
//! Two properties matter most:
//!
//! 1. **Single-flight, uniform value.** Once a fill or refresh is in
//!    flight, concurrent callers wait on it (polling from their own call
//!    context — see "Why the waiter polls" below) and all receive the same
//!    outcome: the fresh value on success, or the *same* stale value on
//!    failure. An error only surfaces for a key with no servable value.
//! 2. **Stale-while-revalidate / stale-if-error.** An expired value is not
//!    evicted at `ttl`; it lingers through the stale window as a fallback,
//!    replaced only by a successful refresh. A failed refresh keeps serving
//!    it. The value is hard-evicted at `evict_at = last_success + ttl +
//!    stale`, or earlier under LRU pressure.
//!
//! ## Failure backoff (debounce)
//!
//! A failed fill does not retry on the next call; the key parks until
//! `next_attempt = now + base * multiplier^(failures-1)` (reset on
//! success). The entry lifetime is the bound and the reset: once the delay
//! would push `next_attempt` past `evict_at`, the entry is evicted and the
//! next call is a fresh cold fill. Cold-failure markers (no value) carry an
//! `evict_at` too, so a never-succeeding key's backoff is bounded the same
//! way and can't leak. With no backoff configured (the default) a failed
//! fill is immediately retryable.
//!
//! ## Why the waiter polls instead of being woken
//!
//! A waiter does not block on a shared future the filler wakes. The
//! canister runs a single wasm thread, and waking another call's task runs
//! it to completion *inside the filler's* call context — mis-routing the
//! reply (`ic0.msg_reply ... already replied`). Instead each waiter polls
//! from its OWN call context, yielding a cheap management-canister round
//! ([`yield_round`]) between checks. The cache holds no `Waker`s and
//! [`get_or_fill`] releases the cache borrow before every `.await`.

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
/// and later arrivals would otherwise wait on it forever; this window lets
/// one take the fill over. 120 s is comfortably longer than any plausible
/// HTTP-outcall round trip yet quick enough that a genuinely stuck entry
/// clears within a couple of retries.
pub const DEFAULT_PENDING_STALE_AFTER_SECS: u64 = 120;

/// Upper bound on how many times a waiter polls before giving up. Each poll
/// yields one round via [`yield_round`]; on giving up the waiter falls back
/// to a stale value if one exists, else [`CacheFillError::WaitTimedOut`].
const MAX_WAIT_POLLS: u32 = 100;

/// Exponential backoff for the failure path: `delay = base_secs *
/// multiplier^(consecutive_failures - 1)`, reset on success. No explicit
/// cap — the entry lifetime (`evict_at`) bounds and resets it.
#[derive(Clone, Copy)]
pub struct RetryBackoff {
    base_secs: u64,
    multiplier: u64,
}

impl RetryBackoff {
    /// Build a backoff schedule. The first failure waits `base_secs`; each
    /// further consecutive failure multiplies the wait by `multiplier`.
    pub fn new(base_secs: u64, multiplier: u64) -> Self {
        Self {
            base_secs,
            multiplier,
        }
    }

    /// Debounce delay for the `failures`-th consecutive failure (1-based).
    /// Saturating throughout — a large failure count clamps rather than
    /// overflowing; the entry's `evict_at` caps the effective wait anyway.
    fn delay_secs(&self, failures: u32) -> u64 {
        let factor = self
            .multiplier
            .checked_pow(failures.saturating_sub(1))
            .unwrap_or(u64::MAX);
        self.base_secs.saturating_mul(factor)
    }
}

/// One cache record. `value` is `Some` after a success (fresh or stale) and
/// `None` for a cold-failure marker. Timing is absolute wall-clock seconds.
struct Entry<V> {
    value: Option<V>,
    /// Fresh while `now < fresh_until_secs` (only meaningful with a value).
    fresh_until_secs: u64,
    /// Hard-evict at/after this (`last_success + ttl + stale`, or for a
    /// cold-failure marker `failure_time + ttl + stale`).
    evict_at_secs: u64,
    /// Earliest a (re)fill may run — the failure-backoff throttle.
    next_attempt_secs: u64,
    /// Consecutive failures, for the backoff. Reset to 0 on success.
    failures: u32,
    /// Monotonic access stamp for LRU eviction.
    last_access: u64,
}

/// Marker for an in-flight fill. `fill_id` identifies *this* attempt so
/// [`SingleFlightCache::publish`] can tell whether a stale-takeover
/// replaced it; `started_at_secs` drives the staleness check.
struct InFlight {
    fill_id: u64,
    started_at_secs: u64,
}

/// In-memory single-flight cache keyed by `K`, holding values of type `V`.
pub struct SingleFlightCache<K, V> {
    entries: HashMap<K, Entry<V>>,
    in_flight: HashMap<K, InFlight>,
    next_fill_id: u64,
    /// Monotonic counter stamped onto entries on access, for LRU.
    access_clock: u64,
    /// Extra time a value is served past `ttl` (stale-while-revalidate).
    stale_secs: u64,
    /// Hard cap on entries; over it, the least-recently-used is evicted.
    max_entries: usize,
    /// Failure backoff; `None` means immediately retryable.
    backoff: Option<RetryBackoff>,
    /// See [`DEFAULT_PENDING_STALE_AFTER_SECS`].
    pending_stale_after_secs: u64,
}

impl<K, V> SingleFlightCache<K, V> {
    /// A cache with no stale window, no size bound, and no failure backoff
    /// — a failed fill caches nothing and is immediately retryable. Use the
    /// `with_*` builders to opt into stale-serving, an LRU cap, and backoff.
    pub fn new() -> Self {
        Self {
            entries: HashMap::new(),
            in_flight: HashMap::new(),
            next_fill_id: 0,
            access_clock: 0,
            stale_secs: 0,
            max_entries: usize::MAX,
            backoff: None,
            pending_stale_after_secs: DEFAULT_PENDING_STALE_AFTER_SECS,
        }
    }

    /// Serve an expired value for `stale_secs` past its `ttl` while a
    /// refresh is attempted (stale-while-revalidate / stale-if-error).
    pub fn with_stale_secs(mut self, stale_secs: u64) -> Self {
        self.stale_secs = stale_secs;
        self
    }

    /// Cap the cache at `max_entries`; over it, evict the least-recently-
    /// used entry. Protects a hot key from a churn of one-off keys.
    pub fn with_max_entries(mut self, max_entries: usize) -> Self {
        self.max_entries = max_entries.max(1);
        self
    }

    /// Debounce a failed fill with exponential backoff (see [`RetryBackoff`])
    /// instead of allowing an immediate retry.
    pub fn with_retry_backoff(mut self, backoff: RetryBackoff) -> Self {
        self.backoff = Some(backoff);
        self
    }

    /// Override the in-flight staleness window
    /// ([`DEFAULT_PENDING_STALE_AFTER_SECS`]).
    #[allow(dead_code)]
    pub fn with_pending_stale_after_secs(mut self, secs: u64) -> Self {
        self.pending_stale_after_secs = secs;
        self
    }
}

impl<K, V> Default for SingleFlightCache<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash + Clone, V> SingleFlightCache<K, V> {
    /// Decide what an incoming request for `key` should do. See the module
    /// docs for the state semantics. Drops a fully-dead entry, honours the
    /// in-flight dedup (uniform wait), and otherwise classifies the entry
    /// as fresh / stale-serve / stale-refresh-due / cooling-down / cold.
    fn lookup<Q>(&mut self, key: &Q, now_secs: u64) -> CacheState<V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
        V: Clone,
    {
        // Drop a fully-dead entry so the rest reasons over a live one.
        if self
            .entries
            .get(key)
            .is_some_and(|e| now_secs >= e.evict_at_secs)
        {
            self.entries.remove(key);
        }

        let inflight_fresh = self.in_flight.get(key).is_some_and(|f| {
            now_secs.saturating_sub(f.started_at_secs) < self.pending_stale_after_secs
        });

        // Classify from an immutable borrow, then act (touch/claim need
        // `&mut self`, so the borrow must end first).
        let decision = if let Some(e) = self.entries.get(key) {
            let fresh = now_secs < e.fresh_until_secs;
            let throttled = now_secs < e.next_attempt_secs;
            match e.value.as_ref() {
                Some(v) if fresh => Decision::Warm(v.clone()),
                _ if inflight_fresh => Decision::Pending,
                Some(v) if throttled => Decision::Stale(v.clone()),
                None if throttled => Decision::Cooling,
                _ => Decision::Claim,
            }
        } else if inflight_fresh {
            Decision::Pending
        } else {
            Decision::Claim
        };

        match decision {
            Decision::Warm(v) => {
                self.touch(key);
                CacheState::Warm(v)
            }
            Decision::Stale(v) => {
                self.touch(key);
                CacheState::Stale(v)
            }
            Decision::Pending => CacheState::Pending,
            Decision::Cooling => CacheState::CoolingDown,
            Decision::Claim => CacheState::Cold(self.claim(key, now_secs)),
        }
    }

    /// Publish the outcome of the fill you owned. `Some` on success (stored
    /// fresh for `ttl_secs`, backoff reset), `None` on failure (any prior
    /// value is kept for stale-serving; a backoff cooldown is parked).
    /// Cleared/written only if the in-flight marker is still *yours*, so a
    /// superseded fill can't clobber fresher state. Sweeps expired entries.
    fn publish<Q>(
        &mut self,
        key: &Q,
        token: FillToken,
        value: Option<V>,
        ttl_secs: u64,
        now_secs: u64,
    ) where
        K: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
    {
        let still_ours = self
            .in_flight
            .get(key)
            .is_some_and(|f| f.fill_id == token.fill_id);
        if !still_ours {
            self.sweep_expired(now_secs);
            return;
        }
        self.in_flight.remove(key);

        match value {
            Some(v) => {
                let fresh_until = now_secs.saturating_add(ttl_secs);
                let evict_at = fresh_until.saturating_add(self.stale_secs);
                // Success: store fresh, reset the backoff. `next_attempt`
                // points at stale onset so the first stale lookup refreshes.
                self.upsert(
                    key,
                    Entry {
                        value: Some(v),
                        fresh_until_secs: fresh_until,
                        evict_at_secs: evict_at,
                        next_attempt_secs: fresh_until,
                        failures: 0,
                        last_access: 0,
                    },
                );
            }
            None => {
                let failures = self
                    .entries
                    .get(key)
                    .map_or(0, |e| e.failures)
                    .saturating_add(1);
                let delay = self.backoff.as_ref().map_or(0, |b| b.delay_secs(failures));
                let next_attempt = now_secs.saturating_add(delay);

                if let Some(e) = self.entries.get_mut(key) {
                    // Stale-if-error: keep the existing value and its
                    // `evict_at` (the value still dies relative to the last
                    // success); just bump the throttle and failure count.
                    // For a cold-failure marker, also refresh its lifetime.
                    e.next_attempt_secs = next_attempt;
                    e.failures = failures;
                    if e.value.is_none() {
                        e.evict_at_secs = now_secs
                            .saturating_add(ttl_secs)
                            .saturating_add(self.stale_secs);
                    }
                } else {
                    // New cold-failure marker: no value to serve, but bounded
                    // by its own `evict_at` so the backoff can't grow forever.
                    let evict_at = now_secs
                        .saturating_add(ttl_secs)
                        .saturating_add(self.stale_secs);
                    self.upsert(
                        key,
                        Entry {
                            value: None,
                            fresh_until_secs: now_secs,
                            evict_at_secs: evict_at,
                            next_attempt_secs: next_attempt,
                            failures,
                            last_access: 0,
                        },
                    );
                }
            }
        }

        self.sweep_expired(now_secs);
    }

    /// The currently servable value for `key` (fresh or stale), if any —
    /// used for stale-if-error and the poll-timeout fallback.
    fn servable_value<Q>(&self, key: &Q, now_secs: u64) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
        V: Clone,
    {
        self.entries.get(key).and_then(|e| {
            if now_secs < e.evict_at_secs {
                e.value.clone()
            } else {
                None
            }
        })
    }

    /// Mark a fresh in-flight attempt for `key`, returning its token.
    fn claim<Q>(&mut self, key: &Q, now_secs: u64) -> FillToken
    where
        K: Borrow<Q>,
        Q: ToOwned<Owned = K> + ?Sized,
    {
        let fill_id = self.next_fill_id;
        self.next_fill_id = self.next_fill_id.wrapping_add(1);
        self.in_flight.insert(
            key.to_owned(),
            InFlight {
                fill_id,
                started_at_secs: now_secs,
            },
        );
        FillToken { fill_id }
    }

    /// Bump the access stamp on an existing entry (LRU recency).
    fn touch<Q>(&mut self, key: &Q)
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
    {
        self.access_clock = self.access_clock.wrapping_add(1);
        let stamp = self.access_clock;
        if let Some(e) = self.entries.get_mut(key) {
            e.last_access = stamp;
        }
    }

    /// Insert/overwrite an entry, stamping it most-recently-used and
    /// evicting the least-recently-used entry first if a *new* key would
    /// exceed `max_entries`.
    fn upsert<Q>(&mut self, key: &Q, mut entry: Entry<V>)
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
    {
        self.access_clock = self.access_clock.wrapping_add(1);
        entry.last_access = self.access_clock;
        if !self.entries.contains_key(key) && self.entries.len() >= self.max_entries {
            self.evict_lru();
        }
        self.entries.insert(key.to_owned(), entry);
    }

    /// Evict the least-recently-used entry (smallest `last_access`).
    fn evict_lru(&mut self) {
        if let Some(victim) = self
            .entries
            .iter()
            .min_by_key(|(_, e)| e.last_access)
            .map(|(k, _)| k.clone())
        {
            self.entries.remove(&victim);
        }
    }

    /// Drop every entry at/past its `evict_at` — bounds memory for an
    /// unbounded key space and clears dead cold-failure markers.
    fn sweep_expired(&mut self, now_secs: u64) {
        self.entries.retain(|_, e| now_secs < e.evict_at_secs);
    }
}

/// What [`SingleFlightCache::lookup`] resolved to.
enum CacheState<V> {
    /// A fresh value — return it, no fill.
    Warm(V),
    /// A stale value served while the refresh is throttled — return it.
    Stale(V),
    /// A fill/refresh is in flight; wait and re-check.
    Pending,
    /// A recent fill failed and its backoff hasn't elapsed; no value.
    CoolingDown,
    /// Nothing servable and no live fill; the holder owns the fill.
    Cold(FillToken),
}

/// Internal lookup decision, computed before mutating the cache.
enum Decision<V> {
    Warm(V),
    Stale(V),
    Pending,
    Cooling,
    Claim,
}

/// Opaque handle a [`CacheState::Cold`] caller passes back to
/// [`SingleFlightCache::publish`].
struct FillToken {
    fill_id: u64,
}

/// Why [`get_or_fill`] returned without a value.
pub enum CacheFillError<E> {
    /// The fill we owned failed and there was no value to fall back on.
    Fill(E),
    /// A foreign in-flight fill didn't publish within [`MAX_WAIT_POLLS`]
    /// and there was no stale value to serve. Transient.
    WaitTimedOut,
    /// A recent fill failed and the backoff cooldown hasn't elapsed; the
    /// fill was not run and there is no servable value.
    CoolingDown,
}

/// Get `key` from `cache`, or fill it once under single-flight dedup, with
/// stale-while-revalidate + stale-if-error and exponential-backoff retry.
/// See the module docs for the full semantics.
///
/// `now` is read fresh on each loop turn and again after the fill. The
/// cache borrow is released before every `.await`, as the single-threaded
/// executor requires.
pub async fn get_or_fill<K, V, Q, E, Fut>(
    cache: &'static LocalKey<RefCell<SingleFlightCache<K, V>>>,
    key: &Q,
    now: impl Fn() -> u64,
    ttl_secs: u64,
    fill: impl FnOnce() -> Fut,
) -> Result<V, CacheFillError<E>>
where
    K: Eq + Hash + Clone + Borrow<Q>,
    Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
    V: Clone,
    Fut: Future<Output = Result<V, E>>,
{
    let mut waits = 0u32;
    let token = loop {
        let now_secs = now();
        match cache.with(|c| c.borrow_mut().lookup(key, now_secs)) {
            CacheState::Warm(value) => return Ok(value),
            CacheState::Stale(value) => return Ok(value),
            CacheState::Cold(token) => break token,
            CacheState::CoolingDown => return Err(CacheFillError::CoolingDown),
            CacheState::Pending => {
                if waits >= MAX_WAIT_POLLS {
                    // A wedged owner: serve a stale value if one exists,
                    // else report the transient timeout.
                    if let Some(v) = cache.with(|c| c.borrow().servable_value(key, now())) {
                        return Ok(v);
                    }
                    return Err(CacheFillError::WaitTimedOut);
                }
                waits += 1;
                yield_round().await;
            }
        }
    };

    // We own the fill.
    let result = fill().await;
    let now_secs = now();
    let to_cache = result.as_ref().ok().cloned();
    cache.with(|c| {
        c.borrow_mut()
            .publish(key, token, to_cache, ttl_secs, now_secs)
    });
    match result {
        Ok(value) => Ok(value),
        // Stale-if-error: a kept value beats surfacing the failure.
        Err(e) => match cache.with(|c| c.borrow().servable_value(key, now_secs)) {
            Some(value) => Ok(value),
            None => Err(CacheFillError::Fill(e)),
        },
    }
}

/// Yield one round, resuming in the caller's OWN call context (see the
/// module docs for why a waiter must not be woken cross-call). Awaiting a
/// cheap management-canister round-trip gives up a round and comes back in
/// our own context; the random bytes are unused.
#[cfg(not(test))]
async fn yield_round() {
    let _ =
        ic_cdk::call::<(), (Vec<u8>,)>(candid::Principal::management_canister(), "raw_rand", ())
            .await;
}

/// Host-test stand-in: there is no executor to yield to, so a poll is a
/// no-op. The [`MAX_WAIT_POLLS`] counter still bounds the loop.
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

    fn expect_cold<V>(state: CacheState<V>) -> FillToken {
        match state {
            CacheState::Cold(t) => t,
            _ => panic!("expected Cold"),
        }
    }

    // ---- lookup / publish state machine ----

    #[test]
    fn fresh_then_stale_then_dead() {
        // ttl 100, stale 50 → fresh [0,100), stale [100,150), dead ≥150.
        let mut c = SingleFlightCache::<String, &str>::new().with_stale_secs(50);
        let t = expect_cold(c.lookup("k", 0));
        c.publish("k", t, Some("v"), 100, 0);
        assert!(matches!(c.lookup("k", 50), CacheState::Warm("v")));

        let mut c = SingleFlightCache::<String, &str>::new().with_stale_secs(50);
        let t = expect_cold(c.lookup("k", 0));
        c.publish("k", t, Some("v"), 100, 0);
        // Stale: refresh due → Cold, value retained.
        assert!(matches!(c.lookup("k", 120), CacheState::Cold(_)));
        assert!(c.entries.get("k").and_then(|e| e.value).is_some());

        let mut c = SingleFlightCache::<String, &str>::new().with_stale_secs(50);
        let t = expect_cold(c.lookup("k", 0));
        c.publish("k", t, Some("v"), 100, 0);
        // Dead: evicted, cold path.
        assert!(matches!(c.lookup("k", 150), CacheState::Cold(_)));
    }

    #[test]
    fn concurrent_lookup_waits_when_fill_in_flight() {
        let mut c = SingleFlightCache::<String, &str>::new();
        let _t = expect_cold(c.lookup("k", 0));
        assert!(matches!(c.lookup("k", 0), CacheState::Pending));
    }

    #[test]
    fn failed_refresh_keeps_serving_stale() {
        let mut c = SingleFlightCache::<String, &str>::new()
            .with_stale_secs(1000)
            .with_retry_backoff(RetryBackoff::new(60, 2));
        let t = expect_cold(c.lookup("k", 0));
        c.publish("k", t, Some("good"), 100, 0);
        let t2 = expect_cold(c.lookup("k", 100)); // refresh due, owns refill
        c.publish("k", t2, None, 100, 100); // refresh fails
                                            // Within the backoff window: serve the OLD value, no fill.
        assert!(matches!(c.lookup("k", 130), CacheState::Stale("good")));
        // After the backoff (base 60s): refresh due again.
        assert!(matches!(c.lookup("k", 160), CacheState::Cold(_)));
    }

    #[test]
    fn cold_failure_cools_down_then_retries() {
        let mut c = SingleFlightCache::<String, &str>::new()
            .with_stale_secs(1000)
            .with_retry_backoff(RetryBackoff::new(60, 2));
        let t = expect_cold(c.lookup("k", 0));
        c.publish("k", t, None, 100, 0); // cold fill fails → marker, no value
        assert!(matches!(c.lookup("k", 30), CacheState::CoolingDown));
        // After base 60s the cold key may be refilled.
        assert!(matches!(c.lookup("k", 61), CacheState::Cold(_)));
    }

    #[test]
    fn backoff_grows_then_resets_on_success() {
        let mut c = SingleFlightCache::<String, &str>::new()
            .with_stale_secs(100_000)
            .with_retry_backoff(RetryBackoff::new(60, 2));
        // fail 1 @0 → next at 60.
        let t = expect_cold(c.lookup("k", 0));
        c.publish("k", t, None, 1000, 0);
        assert_eq!(c.entries.get("k").map(|e| e.next_attempt_secs), Some(60));
        // fail 2 @60 → next at 60 + 120 = 180.
        let t = expect_cold(c.lookup("k", 60));
        c.publish("k", t, None, 1000, 60);
        assert_eq!(c.entries.get("k").map(|e| e.next_attempt_secs), Some(180));
        // success resets the failure count.
        let t = expect_cold(c.lookup("k", 180));
        c.publish("k", t, Some("v"), 1000, 180);
        assert_eq!(c.entries.get("k").map(|e| e.failures), Some(0));
    }

    #[test]
    fn no_backoff_is_immediately_retryable() {
        let mut c = SingleFlightCache::<String, &str>::new();
        let t = expect_cold(c.lookup("k", 0));
        c.publish("k", t, None, 100, 0);
        // No value cached, no cooldown: next lookup is a fresh Cold.
        assert!(matches!(c.lookup("k", 0), CacheState::Cold(_)));
    }

    #[test]
    fn superseded_owner_publish_does_not_overwrite() {
        let mut c = SingleFlightCache::<String, &str>::new().with_pending_stale_after_secs(100);
        let a = expect_cold(c.lookup("k", 0));
        let cc = expect_cold(c.lookup("k", 101)); // A presumed abandoned, C takes over
        c.publish("k", a, Some("A"), 1000, 0); // A's late publish
        assert!(
            c.servable_value("k", 0).is_none(),
            "A's stale publish must not land after takeover"
        );
        c.publish("k", cc, Some("C"), 1000, 101);
        assert_eq!(c.servable_value("k", 200), Some("C"));
    }

    #[test]
    fn lru_evicts_least_recently_used_when_full() {
        let mut c = SingleFlightCache::<String, &str>::new().with_max_entries(2);
        for (k, t) in [("a", 0u64), ("b", 1)] {
            let tok = expect_cold(c.lookup(k, t));
            c.publish(k, tok, Some("v"), 10_000, t);
        }
        // Touch "a" so "b" is the LRU.
        assert!(matches!(c.lookup("a", 2), CacheState::Warm(_)));
        // Insert "c": over capacity → evict the LRU ("b").
        let tok = expect_cold(c.lookup("c", 3));
        c.publish("c", tok, Some("v"), 10_000, 3);
        assert!(c.entries.contains_key("a"));
        assert!(c.entries.contains_key("c"));
        assert!(!c.entries.contains_key("b"), "b was least-recently-used");
        assert_eq!(c.entries.len(), 2);
    }

    #[test]
    fn sweep_drops_dead_entries() {
        let mut c = SingleFlightCache::<String, &str>::new();
        c.entries.insert(
            "old".into(),
            Entry {
                value: Some("x"),
                fresh_until_secs: 10,
                evict_at_secs: 10,
                next_attempt_secs: 10,
                failures: 0,
                last_access: 0,
            },
        );
        // A publish at t=100 sweeps the dead "old" entry.
        let t = expect_cold(c.lookup("new", 100));
        c.publish("new", t, Some("v"), 1000, 100);
        assert!(!c.entries.contains_key("old"));
        assert!(c.entries.contains_key("new"));
    }

    // ---- get_or_fill end to end ----

    thread_local! {
        static TEST_CACHE: RefCell<SingleFlightCache<String, i32>> = RefCell::new(
            SingleFlightCache::new()
                .with_stale_secs(50)
                .with_retry_backoff(RetryBackoff::new(60, 2)),
        );
    }
    fn reset() {
        TEST_CACHE.with(|c| {
            *c.borrow_mut() = SingleFlightCache::new()
                .with_stale_secs(50)
                .with_retry_backoff(RetryBackoff::new(60, 2))
        });
    }

    #[test]
    fn get_or_fill_warm_skips_fill() {
        reset();
        let r: Result<i32, CacheFillError<()>> =
            block_on(get_or_fill(&TEST_CACHE, "k", || 0, 100, || async { Ok(7) }));
        assert!(matches!(r, Ok(7)));
        let r2: Result<i32, CacheFillError<()>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 50,
            100,
            || async { panic!("must not fill on warm hit") },
        ));
        assert!(matches!(r2, Ok(7)));
    }

    #[test]
    fn get_or_fill_serves_stale_on_failed_refresh() {
        reset();
        let _: Result<i32, CacheFillError<()>> =
            block_on(get_or_fill(&TEST_CACHE, "k", || 0, 100, || async { Ok(7) }));
        // now=110 → stale, refresh due; this caller owns it and it fails,
        // so stale-if-error returns the old value.
        let r: Result<i32, CacheFillError<&str>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 110,
            100,
            || async { Err("boom") },
        ));
        assert!(matches!(r, Ok(7)), "stale value should be served on error");
        // Within backoff: still stale, fill not even called.
        let r2: Result<i32, CacheFillError<()>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 120,
            100,
            || async { panic!("must not fill during backoff") },
        ));
        assert!(matches!(r2, Ok(7)));
    }

    #[test]
    fn get_or_fill_cold_failure_errors_then_cools_down() {
        reset();
        let r: Result<i32, CacheFillError<&str>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 0,
            100,
            || async { Err("boom") },
        ));
        assert!(matches!(r, Err(CacheFillError::Fill("boom"))));
        // Within backoff a cold key short-circuits without filling.
        let r2: Result<i32, CacheFillError<()>> = block_on(get_or_fill(
            &TEST_CACHE,
            "k",
            || 30,
            100,
            || async { panic!("must not fill during cooldown") },
        ));
        assert!(matches!(r2, Err(CacheFillError::CoolingDown)));
    }
}
