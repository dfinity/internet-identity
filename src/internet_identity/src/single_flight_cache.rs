//! Single-flight cache for IC canister code: stale-while-revalidate +
//! stale-if-error, with concurrent-fill dedup, exponential-backoff retry,
//! and an LRU size bound.
//!
//! ## The time model — one vocabulary
//!
//! Every value has two configured **durations** and three derived
//! **deadlines** (all absolute wall-clock seconds):
//!
//! ```text
//!   filled_at ──fresh_for──▶ fresh_until ──stale_for──▶ evict_at
//! ```
//!
//! - `fresh_for` — how long a freshly filled value is authoritative; cache-
//!   wide policy set once via `with_fresh_for` (for DoH it's the deploy arg
//!   `max_cache_age_secs`, read in `new_doh_cache`). This is **not** a DNS
//!   TTL — the DoH `transform` strips that.
//! - `fresh_until = filled_at + fresh_for` — fresh below it, stale above.
//! - `stale_for` — extra window a stale value is still served as a
//!   fallback while a refresh is attempted.
//! - `evict_at = fresh_until + stale_for` — hard deadline; the entry is
//!   dropped at/after it (or earlier under LRU pressure).
//! - `retry_at` — a failed fill parks the key until here
//!   (`now + backoff(failures)`), itself capped by `evict_at`.
//!
//! ## What a lookup decides
//!
//! [`get_or_fill`] is the entry point; it drives [`SingleFlightCache::lookup`],
//! which returns exactly one of four outcomes (see [`Lookup`]):
//!
//! - **`Serve(v)`** — a value is servable right now (fresh, or stale within
//!   the cooldown). Returned with no fill.
//! - **`Wait`** — a fill/refresh is in flight; the caller polls and
//!   re-checks. Carries the stale value to fall back on if the wait times
//!   out.
//! - **`Fill`** — nothing fresher is in flight; the caller owns a (blocking)
//!   fill. Carries the stale value to fall back on if that fill fails
//!   (stale-if-error).
//! - **`Throttled`** — a recent fill failed, its backoff hasn't elapsed, and
//!   there is no value to serve. The caller errors out.
//!
//! ## The one invariant
//!
//! **`get_or_fill` returns `Ok` iff a value is servable** — fresh, stale, or
//! freshly filled. The three [`CacheFillError`] variants only name *why*
//! nothing was servable (own fill failed cold / foreign fill wedged /
//! throttled with no value). Because `lookup` carries the servable value in
//! every outcome that has one, the fallback is read once, at lookup time —
//! there is no second "is anything servable" query.
//!
//! The value carried by `Wait`/`Fill` is captured at lookup time. For
//! `Fill` there is an `.await` (the fill) before the fallback is used, so a
//! served stale value may be up to one fill-duration past its `evict_at` —
//! a deliberate soft bound (a barely-expired value beats failing the
//! caller; the stale window is sized in minutes, the overshoot in seconds).
//!
//! ## Single-flight, uniform value
//!
//! Once a fill or refresh is in flight, concurrent callers `Wait` on it and
//! all converge on the same outcome: the fresh value on success, or the same
//! stale value on failure. `fill_id` tokens make a superseded or abandoned
//! fill's late [`SingleFlightCache::publish`] a no-op, so it can't clobber
//! fresher state.
//!
//! ## Why the waiter polls instead of being woken
//!
//! A waiter does not block on a shared future the filler wakes. The canister
//! runs a single wasm thread, and waking another call's task runs it to
//! completion *inside the filler's* call context — mis-routing the reply
//! (`ic0.msg_reply ... already replied`). Instead each waiter polls from its
//! OWN call context, yielding a cheap management-canister round
//! ([`yield_round`]) between checks. The cache holds no `Waker`s and
//! [`get_or_fill`] releases the cache borrow before every `.await`.

use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::hash::Hash;
use std::thread::LocalKey;

/// Default for how long an in-flight claim may sit before a new arrival
/// treats the fill as abandoned and takes it over.
///
/// The IC commits state changes made *before* an `.await`, so if a filler's
/// post-yield continuation traps its in-flight marker survives, and later
/// arrivals would otherwise wait on it forever; this window lets one take
/// the fill over. 120 s is comfortably longer than any plausible HTTP-outcall
/// round trip yet quick enough that a genuinely stuck entry clears within a
/// couple of retries.
pub const DEFAULT_ABANDON_FILL_AFTER_SECS: u64 = 120;

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
/// `None` for a cold-failure marker. All deadlines are absolute seconds; see
/// the module docs for the `fresh_until`/`evict_at`/`retry_at` model.
struct Entry<V> {
    value: Option<V>,
    /// Fresh below this (only meaningful with a value).
    fresh_until: u64,
    /// Hard-evict at/after this (`last_success + fresh_for + stale_for`, or
    /// for a cold-failure marker `failure_time + fresh_for + stale_for`).
    evict_at: u64,
    /// Earliest a (re)fill may run — the failure-backoff throttle.
    retry_at: u64,
    /// Consecutive failures, for the backoff. Reset to 0 on success.
    failures: u32,
    /// Monotonic access stamp for LRU eviction (a counter, not seconds).
    lru_stamp: u64,
}

impl<V> Entry<V> {
    /// Within the freshness window — servable with no refresh.
    fn is_fresh(&self, now: u64) -> bool {
        now < self.fresh_until
    }

    /// Not yet hard-evicted — still servable as a stale fallback.
    fn is_alive(&self, now: u64) -> bool {
        now < self.evict_at
    }

    /// Inside the failure-backoff window — a (re)fill must not run yet.
    fn is_throttled(&self, now: u64) -> bool {
        now < self.retry_at
    }
}

/// Marker for an in-flight fill. `fill_id` identifies *this* attempt so
/// [`SingleFlightCache::publish`] can tell whether a takeover replaced it;
/// `claimed_at` drives the abandonment check.
struct InFlight {
    fill_id: u64,
    claimed_at: u64,
}

/// In-memory single-flight cache keyed by `K`, holding values of type `V`.
pub struct SingleFlightCache<K, V> {
    entries: HashMap<K, Entry<V>>,
    in_flight: HashMap<K, InFlight>,
    next_fill_id: u64,
    /// Monotonic counter stamped onto entries on access, for LRU.
    access_clock: u64,
    /// How long a freshly filled value stays authoritative (its TTL).
    fresh_for: u64,
    /// Extra time a value is served past `fresh_until` (stale-while-revalidate).
    stale_for: u64,
    /// Hard cap on entries; over it, the least-recently-used is evicted.
    max_entries: usize,
    /// Failure backoff; `None` means immediately retryable.
    backoff: Option<RetryBackoff>,
    /// See [`DEFAULT_ABANDON_FILL_AFTER_SECS`].
    abandon_fill_after: u64,
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
            fresh_for: 0,
            stale_for: 0,
            max_entries: usize::MAX,
            backoff: None,
            abandon_fill_after: DEFAULT_ABANDON_FILL_AFTER_SECS,
        }
    }

    /// How long a freshly filled value stays fresh (its TTL). This is cache-
    /// wide policy: every fill gets the same window. `0` (the default) means
    /// values are born already stale.
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

    /// Override the in-flight abandonment window
    /// ([`DEFAULT_ABANDON_FILL_AFTER_SECS`]).
    #[allow(dead_code)]
    pub fn with_abandon_fill_after(mut self, secs: u64) -> Self {
        self.abandon_fill_after = secs;
        self
    }
}

impl<K, V> Default for SingleFlightCache<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

impl<K: Eq + Hash + Clone, V> SingleFlightCache<K, V> {
    /// Decide what an incoming request for `key` should do, carrying the
    /// value to serve (or fall back on) where one exists. See [`Lookup`] and
    /// the module docs for the semantics. Drops a fully-dead entry, honours
    /// the in-flight dedup, and otherwise classifies the entry.
    fn lookup<Q>(&mut self, key: &Q, now: u64) -> Lookup<V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
        V: Clone,
    {
        // Drop a fully-dead entry so the rest reasons over a live one; from
        // here on, a present entry is guaranteed alive (`now < evict_at`).
        if self.entries.get(key).is_some_and(|e| !e.is_alive(now)) {
            self.entries.remove(key);
        }

        let inflight_fresh = self
            .in_flight
            .get(key)
            .is_some_and(|f| now.saturating_sub(f.claimed_at) < self.abandon_fill_after);

        // No live entry: wait on an in-flight fill, else claim a cold one.
        let Some(e) = self.entries.get(key) else {
            return if inflight_fresh {
                Lookup::Wait { servable: None }
            } else {
                Lookup::Fill {
                    token: self.claim(key, now),
                    servable: None,
                }
            };
        };

        // Snapshot what we need as owned/copy values, then release the
        // borrow so `touch`/`claim` (which need `&mut self`) can run.
        let value = e.value.clone();
        let is_fresh = e.is_fresh(now);
        let is_throttled = e.is_throttled(now);

        // Priority order (matches the temperatures in the module docs):
        // fresh value → serve; else a live fill → wait; else a stale value
        // within the cooldown → serve; else value-less cooldown → cool;
        // else a (re)fill is due → claim it.
        if is_fresh {
            if let Some(v) = value.as_ref() {
                self.touch(key);
                return Lookup::Serve(v.clone());
            }
            // A fresh marker carries no value (can't occur today); fall through.
        }
        if inflight_fresh {
            return Lookup::Wait { servable: value };
        }
        if is_throttled {
            return match value {
                Some(v) => {
                    self.touch(key);
                    Lookup::Serve(v)
                }
                None => Lookup::Throttled,
            };
        }
        Lookup::Fill {
            token: self.claim(key, now),
            servable: value,
        }
    }

    /// Publish the outcome of the fill you owned. `Some` on success (stored
    /// fresh for `fresh_for`, backoff reset), `None` on failure (any prior
    /// value is kept for stale-serving; a backoff cooldown is parked).
    /// Cleared/written only if the in-flight marker is still *yours*, so a
    /// superseded fill can't clobber fresher state. Sweeps expired entries.
    fn publish<Q>(&mut self, key: &Q, token: FillToken, value: Option<V>, now: u64)
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
    {
        let fresh_for = self.fresh_for;
        let still_ours = self
            .in_flight
            .get(key)
            .is_some_and(|f| f.fill_id == token.fill_id);
        if !still_ours {
            self.sweep_expired(now);
            return;
        }
        self.in_flight.remove(key);

        match value {
            Some(v) => {
                let fresh_until = now.saturating_add(fresh_for);
                let evict_at = fresh_until.saturating_add(self.stale_for);
                // Success: store fresh, reset the backoff. `retry_at` points
                // at stale onset so the first stale lookup refreshes.
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
            None => {
                // Without a backoff configured there is no negative caching:
                // keep any existing value for stale-if-error serving, but
                // record nothing for a value-less failure, so the key stays
                // immediately re-fetchable. The in-flight marker was already
                // cleared above.
                if let Some(backoff) = self.backoff {
                    let failures = self
                        .entries
                        .get(key)
                        .map_or(0, |e| e.failures)
                        .saturating_add(1);
                    let retry_at = now.saturating_add(backoff.delay_secs(failures));
                    if let Some(e) = self.entries.get_mut(key) {
                        // Stale-if-error: keep the existing value and its
                        // `evict_at` (the value still dies relative to the
                        // last success); just bump the throttle and count.
                        // For a cold-failure marker, also refresh its life.
                        e.retry_at = retry_at;
                        e.failures = failures;
                        if e.value.is_none() {
                            e.evict_at =
                                now.saturating_add(fresh_for).saturating_add(self.stale_for);
                        }
                    } else {
                        // New cold-failure marker: no value to serve, but
                        // bounded by its own `evict_at` so the backoff can't
                        // grow forever.
                        let evict_at = now.saturating_add(fresh_for).saturating_add(self.stale_for);
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

        self.sweep_expired(now);
    }

    /// Mark a fresh in-flight attempt for `key`, returning its token.
    fn claim<Q>(&mut self, key: &Q, now: u64) -> FillToken
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
                claimed_at: now,
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
            e.lru_stamp = stamp;
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
        entry.lru_stamp = self.access_clock;
        if !self.entries.contains_key(key) && self.entries.len() >= self.max_entries {
            self.evict_lru();
        }
        self.entries.insert(key.to_owned(), entry);
    }

    /// Evict the least-recently-used entry (smallest `lru_stamp`).
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

    /// Drop every entry at/past its `evict_at` — bounds memory for an
    /// unbounded key space and clears dead cold-failure markers.
    fn sweep_expired(&mut self, now: u64) {
        self.entries.retain(|_, e| e.is_alive(now));
    }

    /// The value servable for `key` as of `now` (fresh or stale), if any.
    /// Test-only: production reads the servable value straight off the
    /// [`Lookup`] outcome, but tests assert on it directly.
    #[cfg(test)]
    fn servable_value<Q>(&self, key: &Q, now: u64) -> Option<V>
    where
        K: Borrow<Q>,
        Q: Eq + Hash + ?Sized,
        V: Clone,
    {
        self.entries
            .get(key)
            .filter(|e| e.is_alive(now))
            .and_then(|e| e.value.clone())
    }
}

/// What [`SingleFlightCache::lookup`] resolved to, carrying the servable
/// value where one exists. See the module docs for the full semantics.
enum Lookup<V> {
    /// A servable value (fresh, or stale within the cooldown) — return it.
    Serve(V),
    /// A fill/refresh is in flight; wait and re-check. `servable` is the
    /// value to fall back on if the wait times out (`None` if nothing yet).
    Wait { servable: Option<V> },
    /// Nothing fresher is in flight; the holder owns the fill. `servable` is
    /// the value to fall back on if the fill fails (stale-if-error).
    Fill {
        token: FillToken,
        servable: Option<V>,
    },
    /// A recent fill failed, its backoff hasn't elapsed, and there is no
    /// value to serve — the caller errors out.
    Throttled,
}

/// Opaque handle a [`Lookup::Fill`] caller passes back to
/// [`SingleFlightCache::publish`].
struct FillToken {
    fill_id: u64,
}

/// Why [`get_or_fill`] returned without a value. Each names a distinct "no
/// value was servable" situation — the only way to get an `Err`.
pub enum CacheFillError<E> {
    /// The fill we owned failed and there was no value to fall back on.
    Fill(E),
    /// A foreign in-flight fill didn't publish within [`MAX_WAIT_POLLS`]
    /// and there was no stale value to serve. Transient.
    WaitTimedOut,
    /// A recent fill failed and the backoff cooldown hasn't elapsed; the
    /// fill was not run and there is no servable value.
    Throttled,
}

/// Get `key` from `cache`, or fill it once under single-flight dedup, with
/// stale-while-revalidate + stale-if-error and exponential-backoff retry.
///
/// Returns `Ok` iff a value is servable — fresh, stale, or freshly filled;
/// the [`CacheFillError`] variants only name *why* nothing was servable. See
/// the module docs for the full semantics, including the soft `evict_at`
/// bound on the stale-if-error fallback.
///
/// Takes only the three operands — what to cache, under what key, and how to
/// produce it. All policy (freshness, stale window, backoff, size) lives on
/// the cache. Time comes from the module's [`now`] (IC time; a test clock
/// under `cfg(test)`). The cache borrow is released before every `.await`, as
/// the single-threaded executor requires.
pub async fn get_or_fill<K, V, Q, E, Fut>(
    cache: &'static LocalKey<RefCell<SingleFlightCache<K, V>>>,
    key: &Q,
    fill: impl FnOnce() -> Fut,
) -> Result<V, CacheFillError<E>>
where
    K: Eq + Hash + Clone + Borrow<Q>,
    Q: Eq + Hash + ToOwned<Owned = K> + ?Sized,
    V: Clone,
    Fut: Future<Output = Result<V, E>>,
{
    let mut waits = 0u32;
    let (token, fallback) = loop {
        let now = now();
        match cache.with(|c| c.borrow_mut().lookup(key, now)) {
            Lookup::Serve(value) => return Ok(value),
            Lookup::Throttled => return Err(CacheFillError::Throttled),
            Lookup::Fill { token, servable } => break (token, servable),
            Lookup::Wait { servable } => {
                if waits >= MAX_WAIT_POLLS {
                    // A wedged owner: serve the stale value if one exists,
                    // else report the transient timeout.
                    return servable
                        .map(Ok)
                        .unwrap_or(Err(CacheFillError::WaitTimedOut));
                }
                waits += 1;
                yield_round().await;
            }
        }
    };

    // We own the fill.
    let result = fill().await;
    let now = now();
    let fetched = result.as_ref().ok().cloned();
    cache.with(|c| c.borrow_mut().publish(key, token, fetched, now));
    match result {
        Ok(value) => Ok(value),
        // Stale-if-error: the value we saw at claim time beats the failure
        // (soft bound — it may be up to one fill-duration past `evict_at`).
        Err(e) => fallback.map(Ok).unwrap_or(Err(CacheFillError::Fill(e))),
    }
}

/// Current wall-clock time in seconds. The cache owns its time source (the
/// same cfg-seam shape as [`yield_round`]) so `get_or_fill` takes no clock.
#[cfg(not(test))]
fn now() -> u64 {
    ic_cdk::api::time() / 1_000_000_000
}

// Test clock — `now()` reads it; advance it with `set_test_now`. The
// canister-time accessor traps off a real canister, so under `cfg(test)` the
// cache reads a thread-local the test drives.
#[cfg(test)]
thread_local! {
    static TEST_NOW: std::cell::Cell<u64> = const { std::cell::Cell::new(1_700_000_000) };
}

#[cfg(test)]
fn now() -> u64 {
    TEST_NOW.with(|c| c.get())
}

/// Set the test clock (seconds). Used by this module's tests and by the DoH
/// integration tests, which drive cache time through it.
#[cfg(test)]
pub(crate) fn set_test_now(secs: u64) {
    TEST_NOW.with(|c| c.set(secs));
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

    fn expect_fill<V>(state: Lookup<V>) -> FillToken {
        match state {
            Lookup::Fill { token, .. } => token,
            _ => panic!("expected Fill"),
        }
    }

    // ---- lookup / publish state machine ----
    //
    // `lookup`/`publish` take an explicit `now` so these drive the state
    // machine tick by tick; `fresh_for` is cache-wide policy set on the
    // builder.

    #[test]
    fn fresh_then_stale_then_dead() {
        // fresh_for 100, stale_for 50 → fresh [0,100), stale [100,150), dead ≥150.
        let mut c = SingleFlightCache::<String, &str>::new()
            .with_fresh_for(100)
            .with_stale_for(50);
        let t = expect_fill(c.lookup("k", 0));
        c.publish("k", t, Some("v"), 0);
        assert!(matches!(c.lookup("k", 50), Lookup::Serve("v")));

        let mut c = SingleFlightCache::<String, &str>::new()
            .with_fresh_for(100)
            .with_stale_for(50);
        let t = expect_fill(c.lookup("k", 0));
        c.publish("k", t, Some("v"), 0);
        // Stale: refresh due → Fill, value retained.
        assert!(matches!(c.lookup("k", 120), Lookup::Fill { .. }));
        assert!(c.entries.get("k").and_then(|e| e.value).is_some());

        let mut c = SingleFlightCache::<String, &str>::new()
            .with_fresh_for(100)
            .with_stale_for(50);
        let t = expect_fill(c.lookup("k", 0));
        c.publish("k", t, Some("v"), 0);
        // Dead: evicted, fill path.
        assert!(matches!(c.lookup("k", 150), Lookup::Fill { .. }));
    }

    #[test]
    fn concurrent_lookup_waits_when_fill_in_flight() {
        let mut c = SingleFlightCache::<String, &str>::new();
        let _t = expect_fill(c.lookup("k", 0));
        assert!(matches!(c.lookup("k", 0), Lookup::Wait { .. }));
    }

    #[test]
    fn failed_refresh_keeps_serving_stale() {
        let mut c = SingleFlightCache::<String, &str>::new()
            .with_fresh_for(100)
            .with_stale_for(1000)
            .with_retry_backoff(RetryBackoff::new(60, 2));
        let t = expect_fill(c.lookup("k", 0));
        c.publish("k", t, Some("good"), 0);
        let t2 = expect_fill(c.lookup("k", 100)); // refresh due, owns refill
        c.publish("k", t2, None, 100); // refresh fails
                                       // Within the backoff window: serve the OLD value, no fill.
        assert!(matches!(c.lookup("k", 130), Lookup::Serve("good")));
        // After the backoff (base 60s): refresh due again.
        assert!(matches!(c.lookup("k", 160), Lookup::Fill { .. }));
    }

    #[test]
    fn cold_failure_throttles_then_retries() {
        let mut c = SingleFlightCache::<String, &str>::new()
            .with_fresh_for(100)
            .with_stale_for(1000)
            .with_retry_backoff(RetryBackoff::new(60, 2));
        let t = expect_fill(c.lookup("k", 0));
        c.publish("k", t, None, 0); // cold fill fails → marker, no value
        assert!(matches!(c.lookup("k", 30), Lookup::Throttled));
        // After base 60s the cold key may be refilled.
        assert!(matches!(c.lookup("k", 61), Lookup::Fill { .. }));
    }

    #[test]
    fn backoff_grows_then_resets_on_success() {
        let mut c = SingleFlightCache::<String, &str>::new()
            .with_fresh_for(1000)
            .with_stale_for(100_000)
            .with_retry_backoff(RetryBackoff::new(60, 2));
        // fail 1 @0 → next at 60.
        let t = expect_fill(c.lookup("k", 0));
        c.publish("k", t, None, 0);
        assert_eq!(c.entries.get("k").map(|e| e.retry_at), Some(60));
        // fail 2 @60 → next at 60 + 120 = 180.
        let t = expect_fill(c.lookup("k", 60));
        c.publish("k", t, None, 60);
        assert_eq!(c.entries.get("k").map(|e| e.retry_at), Some(180));
        // success resets the failure count.
        let t = expect_fill(c.lookup("k", 180));
        c.publish("k", t, Some("v"), 180);
        assert_eq!(c.entries.get("k").map(|e| e.failures), Some(0));
    }

    #[test]
    fn no_backoff_is_immediately_retryable() {
        let mut c = SingleFlightCache::<String, &str>::new().with_fresh_for(100);
        let t = expect_fill(c.lookup("k", 0));
        c.publish("k", t, None, 0);
        // No value cached, no throttle: next lookup is a fresh Fill.
        assert!(matches!(c.lookup("k", 0), Lookup::Fill { .. }));
    }

    #[test]
    fn superseded_owner_publish_does_not_overwrite() {
        let mut c = SingleFlightCache::<String, &str>::new()
            .with_fresh_for(1000)
            .with_abandon_fill_after(100);
        let a = expect_fill(c.lookup("k", 0));
        let cc = expect_fill(c.lookup("k", 101)); // A presumed abandoned, C takes over
        c.publish("k", a, Some("A"), 0); // A's late publish
        assert!(
            c.servable_value("k", 0).is_none(),
            "A's stale publish must not land after takeover"
        );
        c.publish("k", cc, Some("C"), 101);
        assert_eq!(c.servable_value("k", 200), Some("C"));
    }

    #[test]
    fn lru_evicts_least_recently_used_when_full() {
        let mut c = SingleFlightCache::<String, &str>::new()
            .with_fresh_for(10_000)
            .with_max_entries(2);
        for (k, t) in [("a", 0u64), ("b", 1)] {
            let tok = expect_fill(c.lookup(k, t));
            c.publish(k, tok, Some("v"), t);
        }
        // Touch "a" so "b" is the LRU.
        assert!(matches!(c.lookup("a", 2), Lookup::Serve(_)));
        // Insert "c": over capacity → evict the LRU ("b").
        let tok = expect_fill(c.lookup("c", 3));
        c.publish("c", tok, Some("v"), 3);
        assert!(c.entries.contains_key("a"));
        assert!(c.entries.contains_key("c"));
        assert!(!c.entries.contains_key("b"), "b was least-recently-used");
        assert_eq!(c.entries.len(), 2);
    }

    #[test]
    fn sweep_drops_dead_entries() {
        let mut c = SingleFlightCache::<String, &str>::new().with_fresh_for(1000);
        c.entries.insert(
            "old".into(),
            Entry {
                value: Some("x"),
                fresh_until: 10,
                evict_at: 10,
                retry_at: 10,
                failures: 0,
                lru_stamp: 0,
            },
        );
        // A publish at t=100 sweeps the dead "old" entry.
        let t = expect_fill(c.lookup("new", 100));
        c.publish("new", t, Some("v"), 100);
        assert!(!c.entries.contains_key("old"));
        assert!(c.entries.contains_key("new"));
    }

    // ---- get_or_fill end to end ----
    //
    // `get_or_fill` takes no clock; it reads the module's `now()`, which under
    // `cfg(test)` returns the [`set_test_now`] thread-local. `fresh_for` is on
    // the cache (100s here).

    thread_local! {
        static TEST_CACHE: RefCell<SingleFlightCache<String, i32>> = RefCell::new(
            SingleFlightCache::new()
                .with_fresh_for(100)
                .with_stale_for(50)
                .with_retry_backoff(RetryBackoff::new(60, 2)),
        );
    }
    fn reset() {
        TEST_CACHE.with(|c| {
            *c.borrow_mut() = SingleFlightCache::new()
                .with_fresh_for(100)
                .with_stale_for(50)
                .with_retry_backoff(RetryBackoff::new(60, 2))
        });
    }

    #[test]
    fn get_or_fill_warm_skips_fill() {
        reset();
        set_test_now(0);
        let r: Result<i32, CacheFillError<()>> =
            block_on(get_or_fill(&TEST_CACHE, "k", || async { Ok(7) }));
        assert!(matches!(r, Ok(7)));
        set_test_now(50);
        let r2: Result<i32, CacheFillError<()>> =
            block_on(get_or_fill(&TEST_CACHE, "k", || async {
                panic!("must not fill on warm hit")
            }));
        assert!(matches!(r2, Ok(7)));
    }

    #[test]
    fn get_or_fill_serves_stale_on_failed_refresh() {
        reset();
        set_test_now(0);
        let _: Result<i32, CacheFillError<()>> =
            block_on(get_or_fill(&TEST_CACHE, "k", || async { Ok(7) }));
        // now=110 → stale, refresh due; this caller owns it and it fails,
        // so stale-if-error returns the old value.
        set_test_now(110);
        let r: Result<i32, CacheFillError<&str>> =
            block_on(get_or_fill(&TEST_CACHE, "k", || async { Err("boom") }));
        assert!(matches!(r, Ok(7)), "stale value should be served on error");
        // Within backoff: still stale, fill not even called.
        set_test_now(120);
        let r2: Result<i32, CacheFillError<()>> =
            block_on(get_or_fill(&TEST_CACHE, "k", || async {
                panic!("must not fill during backoff")
            }));
        assert!(matches!(r2, Ok(7)));
    }

    #[test]
    fn get_or_fill_cold_failure_errors_then_throttles() {
        reset();
        set_test_now(0);
        let r: Result<i32, CacheFillError<&str>> =
            block_on(get_or_fill(&TEST_CACHE, "k", || async { Err("boom") }));
        assert!(matches!(r, Err(CacheFillError::Fill("boom"))));
        // Within backoff a cold key short-circuits without filling.
        set_test_now(30);
        let r2: Result<i32, CacheFillError<()>> =
            block_on(get_or_fill(&TEST_CACHE, "k", || async {
                panic!("must not fill while throttled")
            }));
        assert!(matches!(r2, Err(CacheFillError::Throttled)));
    }
}
