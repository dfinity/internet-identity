//! Single-flight cache for IC canister code: a memoised async fill with
//! concurrent-fill dedup, stale-while-revalidate + stale-if-error,
//! exponential-backoff retry, and an LRU size bound — **callback delivery**.
//!
//! The cache does not return values. You give [`with_value`] a key and a
//! callback; the callback is invoked once with the result when it's
//! available. The fill is set once, in the constructor. This shape exists
//! because the IC can't block one call on another's in-flight work — waking
//! another call's task runs it inside the filler's context and mis-routes
//! its reply. So rather than make callers wait, the owning fill is detached
//! (`ic_cdk::spawn`) and every interested caller's callback runs when the
//! fill lands. Callers return immediately; downstream state is updated in the
//! callback and observed by later polls.
//!
//! ## The time model — one vocabulary
//!
//! ```text
//!   filled_at ──fresh_for──▶ fresh_until ──stale_for──▶ evict_at
//! ```
//!
//! - `fresh_for` — how long a freshly filled value is authoritative.
//! - `fresh_until = filled_at + fresh_for` — fresh below it, stale above.
//! - `stale_for` — extra window a stale value is still served as a fallback.
//! - `evict_at = fresh_until + stale_for` — hard deadline; dropped at/after it.
//! - `retry_at` — a failed fill parks the key until here, capped by `evict_at`.
//!
//! ## What a lookup decides ([`Lookup`])
//!
//! - **`Deliver(v)`** — a value is servable now (fresh, or stale within the
//!   cooldown); the callback runs immediately.
//! - **`StartFill`** — nothing servable and no fill in flight; the callback
//!   joins a fresh queue and the fill is spawned.
//! - **`Joining`** — a fill is already in flight; the callback joins its queue
//!   (dedup — no second fill).
//! - **`Throttled`** — a recent fill failed, backoff not elapsed, no value;
//!   the callback gets `Err(Throttled)` immediately.
//!
//! ## Delivery contract
//!
//! Every callback is invoked exactly once with a `Result<V,
//! CacheFillError<E>>`: `Ok` for a servable value (fresh, stale, or freshly
//! filled), or an error naming why nothing was servable (`FillFailed` /
//! `Throttled` / `QueueFull`). A fill is shared: one fan-out, N callbacks.
//! `fill_id` tokens make a superseded or abandoned fill's late completion a
//! no-op, and on takeover the queued callbacks are retained and drained by
//! the replacement fill — so a trapped fill never strands its waiters.
//!
//! ## Trap isolation
//!
//! When a fill completes, [`complete_fill`](SingleFlightCache::complete_fill)
//! commits the fetched value, then the queued callbacks are drained
//! synchronously in that one response execution. The callbacks are themselves
//! synchronous (their async tail — e.g. a follow-up fetch — re-enters
//! `with_value`, which spawns its own fill), so spawning them would not
//! isolate them; they must be no-trap, which the backend rule already
//! mandates. A trap in the drain rolls the batch back and the value is
//! re-fetched on the next trigger — fail-safe.

use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::hash::Hash;
use std::pin::Pin;
use std::thread::LocalKey;

/// Default for how long an in-flight claim may sit before a new arrival treats
/// the fill as abandoned and takes it over (e.g. its spawned task trapped
/// after the outcall). 120 s comfortably exceeds any HTTP-outcall round trip
/// while clearing a genuinely stuck entry within a couple of retries.
pub const DEFAULT_ABANDON_FILL_AFTER_SECS: u64 = 120;

/// Default cap on callbacks queued for a single in-flight key. Past it, a new
/// caller's callback fires immediately with [`CacheFillError::QueueFull`]
/// (transient) rather than enqueuing — bounding memory without dropping work:
/// the in-flight fill still completes and caches, so a retry hits the fresh
/// value with no extra fan-out.
pub const DEFAULT_MAX_WAITERS: usize = 32;

/// Exponential backoff for the failure path: `delay = base_secs *
/// multiplier^(consecutive_failures - 1)`, reset on success. No explicit cap —
/// the entry lifetime (`evict_at`) bounds and resets it.
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

/// One cache record. `value` is `Some` after a success and `None` for a
/// cold-failure marker. All deadlines are absolute seconds.
struct Entry<V> {
    value: Option<V>,
    fresh_until: u64,
    evict_at: u64,
    retry_at: u64,
    failures: u32,
    lru_stamp: u64,
}

impl<V> Entry<V> {
    fn is_fresh(&self, now: u64) -> bool {
        now < self.fresh_until
    }
    fn is_alive(&self, now: u64) -> bool {
        now < self.evict_at
    }
    fn is_throttled(&self, now: u64) -> bool {
        now < self.retry_at
    }
}

/// Marker for an in-flight fill. `fill_id` identifies *this* attempt so
/// [`complete_fill`](SingleFlightCache::complete_fill) can tell whether a
/// takeover replaced it; `claimed_at` drives the abandonment check.
struct InFlight {
    fill_id: u64,
    claimed_at: u64,
}

/// A boxed fill future and the boxed fill function that produces one per key.
type BoxFuture<V, E> = Pin<Box<dyn Future<Output = Result<V, E>>>>;
type FillFn<K, V, E> = Box<dyn Fn(K) -> BoxFuture<V, E>>;
/// A queued callback, invoked once with the delivered result.
type OnReady<V, E> = Box<dyn FnOnce(Result<V, CacheFillError<E>>)>;
/// What `complete_fill` hands back: the drained callbacks plus the single
/// result to deliver to each.
type Drained<V, E> = (Vec<OnReady<V, E>>, Result<V, CacheFillError<E>>);

/// In-memory single-flight cache keyed by `K`, holding values of type `V`
/// whose fill may fail with `E`. See the module docs.
pub struct SingleFlightCache<K, V, E> {
    entries: HashMap<K, Entry<V>>,
    in_flight: HashMap<K, InFlight>,
    /// Callbacks waiting on each in-flight key, drained when it completes.
    waiters: HashMap<K, Vec<OnReady<V, E>>>,
    /// The shared fill, set in the constructor.
    fill: FillFn<K, V, E>,
    next_fill_id: u64,
    access_clock: u64,
    fresh_for: u64,
    stale_for: u64,
    max_entries: usize,
    max_waiters: usize,
    backoff: Option<RetryBackoff>,
    abandon_fill_after: u64,
}

impl<K, V, E> SingleFlightCache<K, V, E> {
    /// Build a cache with its shared async `fill` — run once per cold/stale
    /// key under single-flight dedup. No stale window, no size bound, and no
    /// backoff by default; opt into those with the `with_*` builders.
    pub fn new<Fut>(fill: impl Fn(K) -> Fut + 'static) -> Self
    where
        Fut: Future<Output = Result<V, E>> + 'static,
    {
        Self {
            entries: HashMap::new(),
            in_flight: HashMap::new(),
            waiters: HashMap::new(),
            fill: Box::new(move |k| Box::pin(fill(k))),
            next_fill_id: 0,
            access_clock: 0,
            fresh_for: 0,
            stale_for: 0,
            max_entries: usize::MAX,
            max_waiters: DEFAULT_MAX_WAITERS,
            backoff: None,
            abandon_fill_after: DEFAULT_ABANDON_FILL_AFTER_SECS,
        }
    }

    /// How long a freshly filled value stays fresh (its TTL).
    pub fn with_fresh_for(mut self, fresh_for: u64) -> Self {
        self.fresh_for = fresh_for;
        self
    }

    /// Serve an expired value for `stale_for` past its `fresh_until` while a
    /// refresh is attempted (stale-while-revalidate / stale-if-error).
    pub fn with_stale_for(mut self, stale_for: u64) -> Self {
        self.stale_for = stale_for;
        self
    }

    /// Cap the cache at `max_entries`; over it, evict the least-recently-used.
    pub fn with_max_entries(mut self, max_entries: usize) -> Self {
        self.max_entries = max_entries.max(1);
        self
    }

    /// Cap callbacks queued per in-flight key; over it, a new caller gets
    /// [`CacheFillError::QueueFull`] (see [`DEFAULT_MAX_WAITERS`]).
    pub fn with_max_waiters(mut self, max_waiters: usize) -> Self {
        self.max_waiters = max_waiters.max(1);
        self
    }

    /// Debounce a failed fill with exponential backoff instead of an immediate
    /// retry.
    pub fn with_retry_backoff(mut self, backoff: RetryBackoff) -> Self {
        self.backoff = Some(backoff);
        self
    }

    /// Override the in-flight abandonment window
    /// ([`DEFAULT_ABANDON_FILL_AFTER_SECS`]).
    #[allow(dead_code)]
    pub fn with_abandon_fill_after(mut self, secs: u64) -> Self {
        self.abandon_fill_after = secs;
        self
    }
}

impl<K: Eq + Hash + Clone, V: Clone, E> SingleFlightCache<K, V, E> {
    /// Classify an incoming request for `key`, claiming the in-flight marker
    /// for a `StartFill`. Drops a fully-dead entry first.
    fn lookup(&mut self, key: &K, now: u64) -> Lookup<V> {
        if self.entries.get(key).is_some_and(|e| !e.is_alive(now)) {
            self.entries.remove(key);
        }

        let inflight_fresh = self
            .in_flight
            .get(key)
            .is_some_and(|f| now.saturating_sub(f.claimed_at) < self.abandon_fill_after);

        let Some(e) = self.entries.get(key) else {
            return if inflight_fresh {
                Lookup::Joining
            } else {
                Lookup::StartFill(self.claim(key, now))
            };
        };

        let value = e.value.clone();
        let is_fresh = e.is_fresh(now);
        let is_throttled = e.is_throttled(now);

        if is_fresh {
            if let Some(v) = value.as_ref() {
                self.touch(key);
                return Lookup::Deliver(v.clone());
            }
        }
        if inflight_fresh {
            return Lookup::Joining;
        }
        if is_throttled {
            return match value {
                Some(v) => {
                    self.touch(key);
                    Lookup::Deliver(v)
                }
                None => Lookup::Throttled,
            };
        }
        // Refresh/refill due. Any stale value stays in `entries` for
        // stale-if-error; claim the fill.
        Lookup::StartFill(self.claim(key, now))
    }

    /// Queue `cb` for an in-flight `key`. Returns it back (un-queued) if the
    /// key's waiter list is already at `max_waiters`.
    fn enqueue(&mut self, key: &K, cb: OnReady<V, E>) -> Result<(), OnReady<V, E>> {
        let q = self.waiters.entry(key.clone()).or_default();
        if q.len() >= self.max_waiters {
            return Err(cb);
        }
        q.push(cb);
        Ok(())
    }

    /// Apply the outcome of the fill identified by `token` and take its queued
    /// callbacks plus the value to deliver to each. `Some` stores fresh and
    /// resets backoff; `None` keeps any prior value for stale-serving and
    /// parks a backoff cooldown. A no-op returning no callbacks if the marker
    /// is no longer ours (a takeover replaced us). The caller delivers the
    /// returned callbacks outside the cache borrow.
    fn complete_fill(
        &mut self,
        key: &K,
        token: FillToken,
        result: Result<V, E>,
        now: u64,
    ) -> Drained<V, E> {
        let still_ours = self
            .in_flight
            .get(key)
            .is_some_and(|f| f.fill_id == token.fill_id);
        if !still_ours {
            // A takeover owns this key now; let its fill deliver. Drop our
            // result without clobbering fresher state.
            self.sweep_expired(now);
            return (Vec::new(), result.map_err(CacheFillError::FillFailed));
        }
        self.in_flight.remove(key);

        match result.as_ref().ok() {
            Some(v) => {
                let fresh_until = now.saturating_add(self.fresh_for);
                let evict_at = fresh_until.saturating_add(self.stale_for);
                self.upsert(
                    key,
                    Entry {
                        value: Some(v.clone()),
                        fresh_until,
                        evict_at,
                        retry_at: fresh_until,
                        failures: 0,
                        lru_stamp: 0,
                    },
                );
            }
            None => {
                if let Some(backoff) = self.backoff {
                    let failures = self
                        .entries
                        .get(key)
                        .map_or(0, |e| e.failures)
                        .saturating_add(1);
                    let retry_at = now.saturating_add(backoff.delay_secs(failures));
                    if let Some(e) = self.entries.get_mut(key) {
                        // Stale-if-error: keep the value and its `evict_at`
                        // (it dies relative to the last success); just bump
                        // the throttle. For a value-less marker, refresh life.
                        e.retry_at = retry_at;
                        e.failures = failures;
                        if e.value.is_none() {
                            e.evict_at = now
                                .saturating_add(self.fresh_for)
                                .saturating_add(self.stale_for);
                        }
                    } else {
                        let evict_at = now
                            .saturating_add(self.fresh_for)
                            .saturating_add(self.stale_for);
                        self.upsert(
                            key,
                            Entry {
                                value: None,
                                fresh_until: now,
                                evict_at,
                                retry_at,
                                failures,
                                lru_stamp: 0,
                            },
                        );
                    }
                }
            }
        }

        let waiters = self.waiters.remove(key).unwrap_or_default();
        // Stale-if-error: a kept value beats surfacing the failure.
        let delivery = match result {
            Ok(v) => Ok(v),
            Err(e) => match self.servable_value(key, now) {
                Some(v) => Ok(v),
                None => Err(CacheFillError::FillFailed(e)),
            },
        };
        self.sweep_expired(now);
        (waiters, delivery)
    }

    /// Mark a fresh in-flight attempt for `key`, returning its token.
    fn claim(&mut self, key: &K, now: u64) -> FillToken {
        let fill_id = self.next_fill_id;
        self.next_fill_id = self.next_fill_id.wrapping_add(1);
        self.in_flight.insert(
            key.clone(),
            InFlight {
                fill_id,
                claimed_at: now,
            },
        );
        FillToken { fill_id }
    }

    fn touch(&mut self, key: &K) {
        self.access_clock = self.access_clock.wrapping_add(1);
        let stamp = self.access_clock;
        if let Some(e) = self.entries.get_mut(key) {
            e.lru_stamp = stamp;
        }
    }

    fn upsert(&mut self, key: &K, mut entry: Entry<V>) {
        self.access_clock = self.access_clock.wrapping_add(1);
        entry.lru_stamp = self.access_clock;
        if !self.entries.contains_key(key) && self.entries.len() >= self.max_entries {
            self.evict_lru();
        }
        self.entries.insert(key.clone(), entry);
    }

    fn evict_lru(&mut self) {
        if let Some(victim) = self
            .entries
            .iter()
            .min_by_key(|(_, e)| e.lru_stamp)
            .map(|(k, _)| k.clone())
        {
            self.entries.remove(&victim);
        }
    }

    fn sweep_expired(&mut self, now: u64) {
        self.entries.retain(|_, e| e.is_alive(now));
    }

    fn servable_value(&self, key: &K, now: u64) -> Option<V> {
        self.entries
            .get(key)
            .filter(|e| e.is_alive(now))
            .and_then(|e| e.value.clone())
    }
}

/// What [`SingleFlightCache::lookup`] resolved to.
enum Lookup<V> {
    /// A servable value (fresh, or stale within the cooldown).
    Deliver(V),
    /// Nothing servable and no fill in flight; the holder owns the fill.
    StartFill(FillToken),
    /// A fill is already in flight; join its waiter queue.
    Joining,
    /// A recent fill failed, backoff not elapsed, no value to serve.
    Throttled,
}

/// Opaque handle for the fill a [`Lookup::StartFill`] caller owns.
struct FillToken {
    fill_id: u64,
}

/// Why a callback received an error instead of a value.
#[derive(Clone, Debug, PartialEq)]
pub enum CacheFillError<E> {
    /// The fill failed and there was no value to fall back on.
    FillFailed(E),
    /// A recent fill failed and the backoff cooldown hasn't elapsed.
    Throttled,
    /// The key's waiter queue was full; retry shortly.
    QueueFull,
}

/// Request the value for `key`, invoking `on_ready` once when it's available
/// — synchronously now for a servable value or an immediate error, or later
/// from the shared fill's completion. See the module docs for the contract.
pub fn with_value<K, V, E>(
    cache: &'static LocalKey<RefCell<SingleFlightCache<K, V, E>>>,
    key: K,
    on_ready: impl FnOnce(Result<V, CacheFillError<E>>) + 'static,
) where
    K: Eq + Hash + Clone + 'static,
    V: Clone + 'static,
    E: Clone + 'static,
{
    let on_ready: OnReady<V, E> = Box::new(on_ready);
    let now = now();
    let action = cache.with(|c| {
        let mut c = c.borrow_mut();
        match c.lookup(&key, now) {
            Lookup::Deliver(v) => Action::DeliverNow(on_ready, Ok(v)),
            Lookup::Throttled => Action::DeliverNow(on_ready, Err(CacheFillError::Throttled)),
            // A fresh `StartFill` claim has an empty queue, so enqueue can't
            // overflow; spawn the fill once the borrow is released.
            Lookup::StartFill(token) => {
                let _ = c.enqueue(&key, on_ready);
                Action::Spawn(token)
            }
            Lookup::Joining => match c.enqueue(&key, on_ready) {
                Ok(()) => Action::Joined,
                Err(cb) => Action::DeliverNow(cb, Err(CacheFillError::QueueFull)),
            },
        }
    });
    // Side effects run outside the cache borrow: a callback may re-enter
    // `with_value` (e.g. a follow-up fetch), and `spawn_fill` re-borrows.
    match action {
        Action::DeliverNow(cb, result) => cb(result),
        Action::Spawn(token) => spawn_fill(cache, key, token),
        Action::Joined => {}
    }
}

/// What [`with_value`] should do once the cache borrow is released.
enum Action<V, E> {
    DeliverNow(OnReady<V, E>, Result<V, CacheFillError<E>>),
    Spawn(FillToken),
    Joined,
}

/// Build the fill future for `key` and detach it; on completion apply the
/// outcome and deliver every queued callback.
fn spawn_fill<K, V, E>(
    cache: &'static LocalKey<RefCell<SingleFlightCache<K, V, E>>>,
    key: K,
    token: FillToken,
) where
    K: Eq + Hash + Clone + 'static,
    V: Clone + 'static,
    E: Clone + 'static,
{
    let fill_fut = cache.with(|c| (c.borrow().fill)(key.clone()));
    detach(async move {
        let result = fill_fut.await;
        let now = now();
        let (waiters, delivery) =
            cache.with(|c| c.borrow_mut().complete_fill(&key, token, result, now));
        for cb in waiters {
            cb(delivery.clone());
        }
    });
}

/// Current wall-clock time in seconds. The cache owns its time source so
/// `with_value` takes no clock (same cfg-seam shape as [`detach`]).
#[cfg(not(test))]
fn now() -> u64 {
    ic_cdk::api::time() / 1_000_000_000
}

/// Detach the fill task: on a real canister `ic_cdk::spawn` drives it (its
/// outcall await suspends it, so the spawning call returns immediately).
#[cfg(not(test))]
fn detach(fut: impl Future<Output = ()> + 'static) {
    ic_cdk::spawn(fut);
}

// ---- test-only time + executor ----
//
// `ic_cdk::spawn` traps off-wasm, so under `cfg(test)` detached fills are
// stashed and the test drives them via `run_detached`; `now()` reads a
// thread-local clock advanced with `set_test_now`.
#[cfg(test)]
thread_local! {
    static TEST_NOW: std::cell::Cell<u64> = const { std::cell::Cell::new(1_700_000_000) };
    static DETACHED: RefCell<Vec<Pin<Box<dyn Future<Output = ()>>>>> = const { RefCell::new(Vec::new()) };
}

#[cfg(test)]
fn now() -> u64 {
    TEST_NOW.with(|c| c.get())
}

/// Set the test clock (seconds). Used by this module's tests and the DoH
/// integration tests, which drive cache time through it.
#[cfg(test)]
pub(crate) fn set_test_now(secs: u64) {
    TEST_NOW.with(|c| c.set(secs));
}

#[cfg(test)]
fn detach(fut: impl Future<Output = ()> + 'static) {
    DETACHED.with(|d| d.borrow_mut().push(Box::pin(fut)));
}

/// Drive every detached fill to completion (and any fills they spawn in turn).
/// Test fills must be synchronous — a single poll completes them.
#[cfg(test)]
pub(crate) fn run_detached() {
    use std::sync::Arc;
    use std::task::{Context, Poll, Wake, Waker};
    struct NoopWaker;
    impl Wake for NoopWaker {
        fn wake(self: Arc<Self>) {}
    }
    let waker = Waker::from(Arc::new(NoopWaker));
    loop {
        let Some(mut fut) = DETACHED.with(|d| d.borrow_mut().pop()) else {
            break;
        };
        let mut cx = Context::from_waker(&waker);
        match fut.as_mut().poll(&mut cx) {
            Poll::Ready(()) => {}
            Poll::Pending => {
                panic!("detached fill returned Pending — test fills must be synchronous")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---- lookup / complete_fill state machine ----
    //
    // These drive the machine directly with an explicit `now`; the fill is
    // never run (it's the `with_value` path that spawns), so a stub suffices.
    fn cache(fresh_for: u64) -> SingleFlightCache<&'static str, &'static str, ()> {
        SingleFlightCache::new(|_k| async { Err(()) }).with_fresh_for(fresh_for)
    }

    fn expect_fill(state: Lookup<&'static str>) -> FillToken {
        match state {
            Lookup::StartFill(token) => token,
            _ => panic!("expected StartFill"),
        }
    }

    #[test]
    fn fresh_then_stale_then_dead() {
        // fresh_for 100, stale_for 50 → fresh [0,100), stale [100,150), dead ≥150.
        let mut c = cache(100).with_stale_for(50);
        let t = expect_fill(c.lookup(&"k", 0));
        let _ = c.complete_fill(&"k", t, Ok("v"), 0);
        assert!(matches!(c.lookup(&"k", 50), Lookup::Deliver("v")));

        let mut c = cache(100).with_stale_for(50);
        let t = expect_fill(c.lookup(&"k", 0));
        let _ = c.complete_fill(&"k", t, Ok("v"), 0);
        // Stale, refresh due → StartFill, value retained.
        assert!(matches!(c.lookup(&"k", 120), Lookup::StartFill(_)));
        assert!(c.entries.get("k").and_then(|e| e.value).is_some());

        let mut c = cache(100).with_stale_for(50);
        let t = expect_fill(c.lookup(&"k", 0));
        let _ = c.complete_fill(&"k", t, Ok("v"), 0);
        // Dead: evicted, fill path.
        assert!(matches!(c.lookup(&"k", 150), Lookup::StartFill(_)));
    }

    #[test]
    fn concurrent_lookup_joins_when_fill_in_flight() {
        let mut c = cache(0);
        let _t = expect_fill(c.lookup(&"k", 0));
        assert!(matches!(c.lookup(&"k", 0), Lookup::Joining));
    }

    #[test]
    fn failed_refresh_keeps_serving_stale() {
        let mut c = cache(100)
            .with_stale_for(1000)
            .with_retry_backoff(RetryBackoff::new(60, 2));
        let t = expect_fill(c.lookup(&"k", 0));
        let _ = c.complete_fill(&"k", t, Ok("good"), 0);
        let t2 = expect_fill(c.lookup(&"k", 100)); // refresh due
        let _ = c.complete_fill(&"k", t2, Err(()), 100); // refresh fails
                                                         // Within backoff: serve the OLD value, no fill.
        assert!(matches!(c.lookup(&"k", 130), Lookup::Deliver("good")));
        // After the backoff (base 60s): refresh due again.
        assert!(matches!(c.lookup(&"k", 160), Lookup::StartFill(_)));
    }

    #[test]
    fn cold_failure_throttles_then_retries() {
        let mut c = cache(100)
            .with_stale_for(1000)
            .with_retry_backoff(RetryBackoff::new(60, 2));
        let t = expect_fill(c.lookup(&"k", 0));
        let _ = c.complete_fill(&"k", t, Err(()), 0); // cold fill fails → marker
        assert!(matches!(c.lookup(&"k", 30), Lookup::Throttled));
        assert!(matches!(c.lookup(&"k", 61), Lookup::StartFill(_)));
    }

    #[test]
    fn backoff_grows_then_resets_on_success() {
        let mut c = cache(1000)
            .with_stale_for(100_000)
            .with_retry_backoff(RetryBackoff::new(60, 2));
        let t = expect_fill(c.lookup(&"k", 0));
        let _ = c.complete_fill(&"k", t, Err(()), 0);
        assert_eq!(c.entries.get("k").map(|e| e.retry_at), Some(60));
        let t = expect_fill(c.lookup(&"k", 60));
        let _ = c.complete_fill(&"k", t, Err(()), 60);
        assert_eq!(c.entries.get("k").map(|e| e.retry_at), Some(180));
        let t = expect_fill(c.lookup(&"k", 180));
        let _ = c.complete_fill(&"k", t, Ok("v"), 180);
        assert_eq!(c.entries.get("k").map(|e| e.failures), Some(0));
    }

    #[test]
    fn superseded_owner_complete_does_not_overwrite() {
        let mut c = cache(1000).with_abandon_fill_after(100);
        let a = expect_fill(c.lookup(&"k", 0));
        let _cc = expect_fill(c.lookup(&"k", 101)); // A abandoned, C takes over
        let _ = c.complete_fill(&"k", a, Ok("A"), 0); // A's late completion
        assert!(
            c.servable_value(&"k", 0).is_none(),
            "A's stale completion must not land after takeover"
        );
    }

    #[test]
    fn lru_evicts_least_recently_used_when_full() {
        let mut c = cache(10_000).with_max_entries(2);
        for (k, t) in [("a", 0u64), ("b", 1)] {
            let tok = expect_fill(c.lookup(&k, t));
            let _ = c.complete_fill(&k, tok, Ok("v"), t);
        }
        assert!(matches!(c.lookup(&"a", 2), Lookup::Deliver(_)));
        let tok = expect_fill(c.lookup(&"c", 3));
        let _ = c.complete_fill(&"c", tok, Ok("v"), 3);
        assert!(c.entries.contains_key("a"));
        assert!(c.entries.contains_key("c"));
        assert!(!c.entries.contains_key("b"), "b was least-recently-used");
    }

    // ---- with_value end to end (callback delivery via the test executor) ----

    thread_local! {
        static E2E: RefCell<SingleFlightCache<&'static str, i32, &'static str>> = RefCell::new(build_e2e());
        static DELIVERED: RefCell<Vec<Result<i32, CacheFillError<&'static str>>>> =
            const { RefCell::new(Vec::new()) };
        static FILL_COUNT: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
        static FILL_RESULT: std::cell::Cell<Result<i32, &'static str>> = const { std::cell::Cell::new(Ok(7)) };
    }

    fn build_e2e() -> SingleFlightCache<&'static str, i32, &'static str> {
        SingleFlightCache::new(|_k| async {
            FILL_COUNT.with(|c| c.set(c.get() + 1));
            FILL_RESULT.with(|r| r.get())
        })
        .with_fresh_for(100)
        .with_stale_for(50)
        .with_retry_backoff(RetryBackoff::new(60, 2))
    }

    fn reset_e2e(fill_result: Result<i32, &'static str>) {
        E2E.with(|c| *c.borrow_mut() = build_e2e());
        DELIVERED.with(|d| d.borrow_mut().clear());
        FILL_COUNT.with(|c| c.set(0));
        FILL_RESULT.with(|r| r.set(fill_result));
        set_test_now(0);
        run_detached(); // drain anything left from a prior test on this thread
    }

    fn record() -> impl FnOnce(Result<i32, CacheFillError<&'static str>>) + 'static {
        |r| DELIVERED.with(|d| d.borrow_mut().push(r))
    }
    fn delivered() -> Vec<Result<i32, CacheFillError<&'static str>>> {
        DELIVERED.with(|d| d.borrow().clone())
    }

    #[test]
    fn with_value_spawns_fill_then_delivers() {
        reset_e2e(Ok(7));
        with_value(&E2E, "k", record());
        // Cold: the fill is detached, nothing delivered yet.
        assert!(delivered().is_empty());
        run_detached();
        assert_eq!(delivered(), vec![Ok(7)]);
        assert_eq!(FILL_COUNT.with(|c| c.get()), 1);

        // Within TTL: delivered synchronously, no fill.
        set_test_now(50);
        DELIVERED.with(|d| d.borrow_mut().clear());
        with_value(&E2E, "k", record());
        assert_eq!(delivered(), vec![Ok(7)]);
        assert_eq!(FILL_COUNT.with(|c| c.get()), 1);
    }

    #[test]
    fn concurrent_callers_share_one_fill() {
        reset_e2e(Ok(7));
        with_value(&E2E, "k", record());
        with_value(&E2E, "k", record()); // joins the in-flight fill
        run_detached();
        assert_eq!(delivered(), vec![Ok(7), Ok(7)]);
        assert_eq!(FILL_COUNT.with(|c| c.get()), 1, "one fan-out for both");
    }

    #[test]
    fn cold_failure_delivers_error_then_throttles() {
        reset_e2e(Err("boom"));
        with_value(&E2E, "k", record());
        run_detached();
        assert_eq!(delivered(), vec![Err(CacheFillError::FillFailed("boom"))]);
        // Within backoff: throttled immediately, no fill.
        set_test_now(30);
        DELIVERED.with(|d| d.borrow_mut().clear());
        with_value(&E2E, "k", record());
        assert_eq!(delivered(), vec![Err(CacheFillError::Throttled)]);
        assert_eq!(FILL_COUNT.with(|c| c.get()), 1, "no refill while throttled");
    }

    #[test]
    fn overflowing_waiters_get_queue_full() {
        E2E.with(|c| *c.borrow_mut() = build_e2e().with_max_waiters(1));
        DELIVERED.with(|d| d.borrow_mut().clear());
        FILL_COUNT.with(|c| c.set(0));
        FILL_RESULT.with(|r| r.set(Ok(7)));
        set_test_now(0);

        with_value(&E2E, "k", record()); // StartFill, queue=[cb]
        with_value(&E2E, "k", record()); // Joining, queue full → QueueFull now
        assert_eq!(delivered(), vec![Err(CacheFillError::QueueFull)]);
        run_detached();
        assert_eq!(
            delivered(),
            vec![Err(CacheFillError::QueueFull), Ok(7)],
            "the queued caller still gets the value"
        );
    }
}
