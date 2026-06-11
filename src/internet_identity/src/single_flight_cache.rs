//! Single-flight cache for IC canister code: a memoised async fill with
//! concurrent-fill dedup, stale-while-revalidate + stale-if-error,
//! exponential-backoff retry, and an LRU size bound — **poll, not block**.
//!
//! [`get`] answers immediately with [`Cached`]:
//!
//! - **`Ready(v)`** — a value is servable now (fresh, or the last-good value
//!   while a refresh is backing off after a failure).
//! - **`Pending`** — nothing servable yet. A fill is in flight (this call may
//!   have just spawned it), or a failed fill is within its backoff. The
//!   caller polls again later; a subsequent `get` returns `Ready` once the
//!   value lands.
//!
//! The cache never blocks one call on another's in-flight work, and never
//! delivers via a callback. This shape exists because the IC can't suspend a
//! canister call on another call's task — resuming it runs it inside the
//! waiter's context and mis-routes the reply (`ic0.msg_reply … already
//! replied`). So the fill is detached (`ic_cdk::spawn`); its only job is to
//! fetch and store. Whoever needed the value re-issues `get` (the recovery
//! flow's status poll is exactly this) and reads it when it's `Ready`. The
//! fill is set once, in the constructor.
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
//! ## What a `get` resolves to
//!
//! - **Fresh value** → `Ready(v)`, no fill.
//! - **Refresh/refill due, none in flight** → spawn the fill; serve the stale
//!   value now if one exists (`Ready(stale)`, stale-while-revalidate), else
//!   `Pending`. The stale value also stays cached for stale-if-error.
//! - **Fill in flight** (cold, or a refill already spawned by an earlier
//!   `get`) → `Ready(stale)` if a stale value exists (served throughout the
//!   revalidation), else `Pending` (a cold fill has nothing yet). Either way
//!   no second fill is spawned — the in-flight marker dedups it.
//! - **Failed fill within backoff** → `Ready(stale)` if a last-good value
//!   exists (stale-if-error), else `Pending` (poll out the backoff).
//!
//! ## Single-flight + trap isolation
//!
//! The `in_flight` marker is the whole dedup mechanism: while a fill runs,
//! concurrent `get`s see it and return `Pending` instead of spawning a second
//! fill. A `fill_id` token tags each attempt, so a superseded or abandoned
//! fill's late completion is a no-op. An in-flight claim older than
//! `abandon_fill_after` is treated as abandoned and the next `get` takes the
//! fill over — recovery for a fill whose task trapped after committing state
//! at an `.await`. Each fill stores its result in its own response execution;
//! a trap there rolls only that fill back and the value is re-fetched on the
//! next poll — fail-safe.

use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::hash::Hash;
use std::pin::Pin;
use std::thread::LocalKey;

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

/// Required configuration for a [`SingleFlightCache`]. Deliberately has **no
/// `Default`**: every field is an availability/abuse decision, and for a
/// cache fronting a shared, consensus-replicated fill (e.g. the subnet's
/// bounded HTTPS-outcall capacity) none has a safe universal value — a silent
/// default would ship the cache with a protection disabled. The caller states
/// each one explicitly.
pub struct CacheConfig {
    /// How long a freshly filled value stays fresh (its TTL). Hits within the
    /// window skip the fill entirely; longer means fewer fills (more
    /// available) but staler values.
    pub fresh_for: u64,
    /// Extra window a value is served past `fresh_for` once a refresh has
    /// failed (stale-if-error) — serves the last-good value through a
    /// transient fill failure instead of failing the caller. `0` disables
    /// stale-serving (every miss past `fresh_for` must refill).
    pub stale_for: u64,
    /// Hard cap on cached keys; over it the least-recently-used is evicted.
    /// Bounds heap for an unbounded key space.
    pub max_entries: usize,
    /// Exponential backoff for a failed fill: parks the key so a failing or
    /// abused fill can't re-issue on every request and monopolise the shared
    /// fill resource (starving other callers). Always present — a cache with
    /// no throttle on its fill is the abuse/availability footgun this config
    /// refuses to let you build by accident.
    pub backoff: RetryBackoff,
    /// How long an in-flight claim may sit before a new arrival treats the
    /// fill as abandoned and takes it over — recovery for a fill whose task
    /// trapped after committing state at an `.await`, leaving the marker
    /// stranded (otherwise every later request for that key polls `Pending`
    /// until the entry dies). Set comfortably longer than a healthy fill
    /// round-trip.
    pub abandon_fill_after: u64,
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

/// In-memory single-flight cache keyed by `K`, holding values of type `V`
/// whose fill may fail with `E`. See the module docs.
pub struct SingleFlightCache<K, V, E> {
    entries: HashMap<K, Entry<V>>,
    in_flight: HashMap<K, InFlight>,
    /// The shared fill, set in the constructor.
    fill: FillFn<K, V, E>,
    next_fill_id: u64,
    access_clock: u64,
    fresh_for: u64,
    stale_for: u64,
    max_entries: usize,
    backoff: RetryBackoff,
    abandon_fill_after: u64,
}

impl<K, V, E> SingleFlightCache<K, V, E> {
    /// Build a cache with its shared async `fill` — run once per cold/stale
    /// key under single-flight dedup — and its [`CacheConfig`]. Every policy
    /// knob is required; see `CacheConfig` for why there are no defaults.
    pub fn new<Fut>(fill: impl Fn(K) -> Fut + 'static, config: CacheConfig) -> Self
    where
        Fut: Future<Output = Result<V, E>> + 'static,
    {
        Self {
            entries: HashMap::new(),
            in_flight: HashMap::new(),
            fill: Box::new(move |k| Box::pin(fill(k))),
            next_fill_id: 0,
            access_clock: 0,
            fresh_for: config.fresh_for,
            stale_for: config.stale_for,
            max_entries: config.max_entries.max(1),
            backoff: config.backoff,
            abandon_fill_after: config.abandon_fill_after,
        }
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
                Lookup::Pending
            } else {
                Lookup::StartFill(self.claim(key, now), None)
            };
        };

        let value = e.value.clone();
        let is_fresh = e.is_fresh(now);
        let is_throttled = e.is_throttled(now);

        if is_fresh {
            if let Some(v) = value.as_ref() {
                self.touch(key);
                return Lookup::Ready(v.clone());
            }
        }
        // A fill is in flight (cold, or a refill spawned by an earlier get), or
        // a recent fill failed and we're backing off. Either way we don't start
        // a fill here; serve the last-good value if we have one — stale-while-
        // revalidate while the refill runs, stale-if-error during backoff —
        // otherwise poll again (a cold fill has nothing to serve yet).
        if inflight_fresh || is_throttled {
            let Some(v) = value else {
                return Lookup::Pending;
            };
            self.touch(key);
            return Lookup::Ready(v);
        }
        // Refresh/refill due. Claim the fill and hand back any stale value so
        // the caller serves it now (stale-while-revalidate); it also stays in
        // `entries` for stale-if-error if the refill fails.
        Lookup::StartFill(self.claim(key, now), value)
    }

    /// Apply the outcome of the fill identified by `token`. `Ok` stores fresh
    /// and resets backoff; `Err` keeps any prior value for stale-serving and
    /// parks a backoff cooldown. A no-op if the marker is no longer ours (a
    /// takeover replaced us).
    fn complete_fill(&mut self, key: &K, token: FillToken, result: Result<V, E>, now: u64) {
        let still_ours = self
            .in_flight
            .get(key)
            .is_some_and(|f| f.fill_id == token.fill_id);
        if !still_ours {
            // A takeover owns this key now; let its fill store the result.
            // Drop ours without clobbering fresher state.
            self.sweep_expired(now);
            return;
        }
        self.in_flight.remove(key);

        match result {
            Ok(v) => {
                let fresh_until = now.saturating_add(self.fresh_for);
                let evict_at = fresh_until.saturating_add(self.stale_for);
                self.upsert(
                    key,
                    Entry {
                        value: Some(v),
                        fresh_until,
                        evict_at,
                        retry_at: fresh_until,
                        failures: 0,
                        lru_stamp: 0,
                    },
                );
            }
            Err(_) => {
                // The fill is always backoff-throttled (no opt-out), so a
                // failure always parks the key.
                let failures = self
                    .entries
                    .get(key)
                    .map_or(0, |e| e.failures)
                    .saturating_add(1);
                let retry_at = now.saturating_add(self.backoff.delay_secs(failures));
                if let Some(e) = self.entries.get_mut(key) {
                    // Stale-if-error: keep the value and its `evict_at` (it
                    // dies relative to the last success); just bump the
                    // throttle. For a value-less marker, refresh life.
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

        self.sweep_expired(now);
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

    #[cfg(test)]
    fn servable_value(&self, key: &K, now: u64) -> Option<V> {
        self.entries
            .get(key)
            .filter(|e| e.is_alive(now))
            .and_then(|e| e.value.clone())
    }
}

/// What [`SingleFlightCache::lookup`] resolved to.
enum Lookup<V> {
    /// A servable value (fresh, or last-good while a failed fill backs off).
    Ready(V),
    /// No fill in flight; the holder owns the fill it must spawn. Carries any
    /// stale value to serve meanwhile (stale-while-revalidate); `None` when
    /// cold (nothing servable yet).
    StartFill(FillToken, Option<V>),
    /// Nothing servable: a fill is in flight, or a failed fill is backing off
    /// with no value to serve. Poll again later.
    Pending,
}

/// Opaque handle for the fill a [`Lookup::StartFill`] caller owns.
struct FillToken {
    fill_id: u64,
}

/// The immediate answer from [`get`]. See the module docs.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Cached<V> {
    /// A value is servable now.
    Ready(V),
    /// Nothing servable yet — a fill is in flight (possibly just spawned by
    /// this call) or backing off. Poll again; a later `get` returns `Ready`.
    Pending,
}

/// Read the value for `key`. Returns `Ready(v)` for a servable value, or
/// `Pending` — spawning the fill if one isn't already in flight. Never
/// blocks. See the module docs for the contract.
pub fn get<K, V, E>(
    cache: &'static LocalKey<RefCell<SingleFlightCache<K, V, E>>>,
    key: K,
) -> Cached<V>
where
    K: Eq + Hash + Clone + 'static,
    V: Clone + 'static,
    E: 'static,
{
    let now = now();
    let outcome = cache.with(|c| c.borrow_mut().lookup(&key, now));
    match outcome {
        Lookup::Ready(v) => Cached::Ready(v),
        Lookup::Pending => Cached::Pending,
        // Spawn the fill once the cache borrow is released (`spawn_fill`
        // re-borrows it).
        Lookup::StartFill(token, value) => {
            spawn_fill(cache, key, token);
            value.map_or(Cached::Pending, Cached::Ready)
        }
    }
}

/// Build the fill future for `key` and detach it; on completion store the
/// outcome so the next `get` can read it.
fn spawn_fill<K, V, E>(
    cache: &'static LocalKey<RefCell<SingleFlightCache<K, V, E>>>,
    key: K,
    token: FillToken,
) where
    K: Eq + Hash + Clone + 'static,
    V: Clone + 'static,
    E: 'static,
{
    let fill_fut = cache.with(|c| (c.borrow().fill)(key.clone()));
    detach(async move {
        let result = fill_fut.await;
        let now = now();
        cache.with(|c| c.borrow_mut().complete_fill(&key, token, result, now));
    });
}

/// Current wall-clock time in seconds. The cache owns its time source so
/// `get` takes no clock (same cfg-seam shape as [`detach`]).
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
    // never run (it's the `get` path that spawns), so a stub suffices.
    /// Neutral config; tests override the fields they exercise via
    /// `CacheConfig { fresh_for: …, ..base_config() }`.
    fn base_config() -> CacheConfig {
        CacheConfig {
            fresh_for: 0,
            stale_for: 0,
            max_entries: usize::MAX,
            backoff: RetryBackoff::new(60, 2),
            abandon_fill_after: 120,
        }
    }
    fn cache(config: CacheConfig) -> SingleFlightCache<&'static str, &'static str, ()> {
        SingleFlightCache::new(|_k| async { Err(()) }, config)
    }

    fn expect_fill(state: Lookup<&'static str>) -> FillToken {
        match state {
            Lookup::StartFill(token, _) => token,
            _ => panic!("expected StartFill"),
        }
    }

    #[test]
    fn fresh_then_stale_then_dead() {
        // fresh_for 100, stale_for 50 → fresh [0,100), stale [100,150), dead ≥150.
        let mut c = cache(CacheConfig {
            fresh_for: 100,
            stale_for: 50,
            ..base_config()
        });
        let t = expect_fill(c.lookup(&"k", 0));
        c.complete_fill(&"k", t, Ok("v"), 0);
        assert!(matches!(c.lookup(&"k", 50), Lookup::Ready("v")));

        let mut c = cache(CacheConfig {
            fresh_for: 100,
            stale_for: 50,
            ..base_config()
        });
        let t = expect_fill(c.lookup(&"k", 0));
        c.complete_fill(&"k", t, Ok("v"), 0);
        // Stale, refresh due → StartFill, value retained for stale-if-error.
        assert!(matches!(c.lookup(&"k", 120), Lookup::StartFill(..)));
        assert!(c.entries.get("k").and_then(|e| e.value).is_some());

        let mut c = cache(CacheConfig {
            fresh_for: 100,
            stale_for: 50,
            ..base_config()
        });
        let t = expect_fill(c.lookup(&"k", 0));
        c.complete_fill(&"k", t, Ok("v"), 0);
        // Dead: evicted, fill path.
        assert!(matches!(c.lookup(&"k", 150), Lookup::StartFill(..)));
    }

    #[test]
    fn concurrent_lookup_pending_when_fill_in_flight() {
        let mut c = cache(base_config());
        let _t = expect_fill(c.lookup(&"k", 0));
        assert!(matches!(c.lookup(&"k", 0), Lookup::Pending));
    }

    #[test]
    fn refreshing_stale_serves_stale_to_concurrent_get() {
        // While a refill is in flight over a stale value, a concurrent get is
        // served the stale value (stale-while-revalidate), not made to wait.
        let mut c = cache(CacheConfig {
            fresh_for: 100,
            stale_for: 1000,
            ..base_config()
        });
        let t = expect_fill(c.lookup(&"k", 0));
        c.complete_fill(&"k", t, Ok("good"), 0);
        let _refill = expect_fill(c.lookup(&"k", 120)); // stale, refresh due → claim
        assert!(matches!(c.lookup(&"k", 121), Lookup::Ready("good")));
    }

    #[test]
    fn failed_refresh_keeps_serving_stale() {
        let mut c = cache(CacheConfig {
            fresh_for: 100,
            stale_for: 1000,
            ..base_config()
        });
        let t = expect_fill(c.lookup(&"k", 0));
        c.complete_fill(&"k", t, Ok("good"), 0);
        let t2 = expect_fill(c.lookup(&"k", 100)); // refresh due
        c.complete_fill(&"k", t2, Err(()), 100); // refresh fails
                                                 // Within backoff: serve the OLD value, no fill (stale-if-error).
        assert!(matches!(c.lookup(&"k", 130), Lookup::Ready("good")));
        // After the backoff (base 60s): refresh due again.
        assert!(matches!(c.lookup(&"k", 160), Lookup::StartFill(..)));
    }

    #[test]
    fn cold_failure_pending_then_retries() {
        let mut c = cache(CacheConfig {
            fresh_for: 100,
            stale_for: 1000,
            ..base_config()
        });
        let t = expect_fill(c.lookup(&"k", 0));
        c.complete_fill(&"k", t, Err(()), 0); // cold fill fails → marker, no value
        assert!(matches!(c.lookup(&"k", 30), Lookup::Pending)); // backing off, nothing to serve
        assert!(matches!(c.lookup(&"k", 61), Lookup::StartFill(..)));
    }

    #[test]
    fn backoff_grows_then_resets_on_success() {
        let mut c = cache(CacheConfig {
            fresh_for: 1000,
            stale_for: 100_000,
            ..base_config()
        });
        let t = expect_fill(c.lookup(&"k", 0));
        c.complete_fill(&"k", t, Err(()), 0);
        assert_eq!(c.entries.get("k").map(|e| e.retry_at), Some(60));
        let t = expect_fill(c.lookup(&"k", 60));
        c.complete_fill(&"k", t, Err(()), 60);
        assert_eq!(c.entries.get("k").map(|e| e.retry_at), Some(180));
        let t = expect_fill(c.lookup(&"k", 180));
        c.complete_fill(&"k", t, Ok("v"), 180);
        assert_eq!(c.entries.get("k").map(|e| e.failures), Some(0));
    }

    #[test]
    fn superseded_owner_complete_does_not_overwrite() {
        let mut c = cache(CacheConfig {
            fresh_for: 1000,
            abandon_fill_after: 100,
            ..base_config()
        });
        let a = expect_fill(c.lookup(&"k", 0));
        let _cc = expect_fill(c.lookup(&"k", 101)); // A abandoned, C takes over
        c.complete_fill(&"k", a, Ok("A"), 0); // A's late completion
        assert!(
            c.servable_value(&"k", 0).is_none(),
            "A's stale completion must not land after takeover"
        );
    }

    #[test]
    fn lru_evicts_least_recently_used_when_full() {
        let mut c = cache(CacheConfig {
            fresh_for: 10_000,
            max_entries: 2,
            ..base_config()
        });
        for (k, t) in [("a", 0u64), ("b", 1)] {
            let tok = expect_fill(c.lookup(&k, t));
            c.complete_fill(&k, tok, Ok("v"), t);
        }
        assert!(matches!(c.lookup(&"a", 2), Lookup::Ready(_)));
        let tok = expect_fill(c.lookup(&"c", 3));
        c.complete_fill(&"c", tok, Ok("v"), 3);
        assert!(c.entries.contains_key("a"));
        assert!(c.entries.contains_key("c"));
        assert!(!c.entries.contains_key("b"), "b was least-recently-used");
    }

    // ---- get end to end (poll model via the test executor) ----

    thread_local! {
        static E2E: RefCell<SingleFlightCache<&'static str, i32, &'static str>> = RefCell::new(build_e2e());
        static FILL_COUNT: std::cell::Cell<u32> = const { std::cell::Cell::new(0) };
        static FILL_RESULT: std::cell::Cell<Result<i32, &'static str>> = const { std::cell::Cell::new(Ok(7)) };
    }

    fn e2e_config() -> CacheConfig {
        CacheConfig {
            fresh_for: 100,
            stale_for: 50,
            ..base_config()
        }
    }
    fn build_e2e() -> SingleFlightCache<&'static str, i32, &'static str> {
        SingleFlightCache::new(
            |_k| async {
                FILL_COUNT.with(|c| c.set(c.get() + 1));
                FILL_RESULT.with(|r| r.get())
            },
            e2e_config(),
        )
    }

    fn reset_e2e(fill_result: Result<i32, &'static str>) {
        E2E.with(|c| *c.borrow_mut() = build_e2e());
        FILL_COUNT.with(|c| c.set(0));
        FILL_RESULT.with(|r| r.set(fill_result));
        set_test_now(0);
        run_detached(); // drain anything left from a prior test on this thread
    }

    fn fill_count() -> u32 {
        FILL_COUNT.with(|c| c.get())
    }

    #[test]
    fn get_spawns_fill_then_serves() {
        reset_e2e(Ok(7));
        // Cold: the fill is detached, nothing servable yet.
        assert_eq!(get(&E2E, "k"), Cached::Pending);
        run_detached();
        assert_eq!(fill_count(), 1);
        // Now cached and fresh: served, no second fill.
        assert_eq!(get(&E2E, "k"), Cached::Ready(7));
        set_test_now(50);
        assert_eq!(get(&E2E, "k"), Cached::Ready(7));
        assert_eq!(fill_count(), 1);
    }

    #[test]
    fn concurrent_gets_share_one_fill() {
        reset_e2e(Ok(7));
        assert_eq!(get(&E2E, "k"), Cached::Pending); // cold → spawn
        assert_eq!(get(&E2E, "k"), Cached::Pending); // joins the in-flight fill
        run_detached();
        assert_eq!(fill_count(), 1, "one fan-out for both");
        assert_eq!(get(&E2E, "k"), Cached::Ready(7));
    }

    #[test]
    fn cold_failure_pending_until_backoff_elapses() {
        reset_e2e(Err("boom"));
        assert_eq!(get(&E2E, "k"), Cached::Pending); // cold → spawn
        run_detached(); // fill fails, stores a value-less backoff marker
        assert_eq!(fill_count(), 1);
        // Within backoff (base 60s): still nothing to serve, no refill.
        set_test_now(30);
        assert_eq!(get(&E2E, "k"), Cached::Pending);
        assert_eq!(fill_count(), 1, "no refill while backing off");
        // After backoff: a fresh fill is spawned.
        set_test_now(61);
        FILL_RESULT.with(|r| r.set(Ok(9)));
        assert_eq!(get(&E2E, "k"), Cached::Pending);
        run_detached();
        assert_eq!(fill_count(), 2);
        assert_eq!(get(&E2E, "k"), Cached::Ready(9));
    }

    #[test]
    fn stale_refresh_polls_through_to_fresh() {
        reset_e2e(Ok(7));
        assert_eq!(get(&E2E, "k"), Cached::Pending);
        run_detached(); // fresh 7 at t0
                        // Stale window (fresh_for 100, stale_for 50 → stale [100,150)).
        set_test_now(120);
        FILL_RESULT.with(|r| r.set(Ok(8)));
        // Refresh due → spawn refill and serve the stale value now
        // (stale-while-revalidate). A concurrent poll while the refill runs is
        // served the stale value too, and does not spawn a second fill.
        assert_eq!(get(&E2E, "k"), Cached::Ready(7));
        assert_eq!(get(&E2E, "k"), Cached::Ready(7));
        run_detached();
        assert_eq!(get(&E2E, "k"), Cached::Ready(8));
        assert_eq!(fill_count(), 2);
    }

    #[test]
    fn stale_if_error_serves_last_good() {
        reset_e2e(Ok(7));
        assert_eq!(get(&E2E, "k"), Cached::Pending);
        run_detached(); // fresh 7 at t0
        set_test_now(120);
        FILL_RESULT.with(|r| r.set(Err("down")));
        // Refill due → spawn and serve stale 7 now (stale-while-revalidate).
        assert_eq!(get(&E2E, "k"), Cached::Ready(7));
        run_detached(); // refill fails, keeps stale 7, parks backoff
                        // Within backoff: still serve the last-good value (stale-if-error).
        set_test_now(121);
        assert_eq!(get(&E2E, "k"), Cached::Ready(7));
    }
}
