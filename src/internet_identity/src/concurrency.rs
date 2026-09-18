//! Bounded-concurrency limiter for a shared resource, used the same way as
//! [`SingleFlightCache`](crate::single_flight_cache): each caller holds its own
//! `thread_local` instance configured for its resource, and there is no central
//! registry. One limiter caps how many operations run at once; the aggregate
//! bound across resources is simply the sum of the per-instance budgets, chosen
//! (like each cache's `max_entries`) to stay within what the resource can take.
//!
//! [`acquire`] hands back a [`Permit`] when a slot is free (else `None`); the
//! caller holds it across the guarded work and the slot releases on `Drop`, so a
//! slot lives exactly as long as the permit. A permit stranded by a caller that
//! trapped after committing state at an `.await` (so its `Drop` never ran) is
//! reclaimed once older than the configured age, so a leak can't permanently
//! wedge the budget.

use std::cell::RefCell;
use std::collections::BTreeMap;
use std::thread::LocalKey;

/// Configuration for a [`ConcurrencyLimiter`]. Named fields so call sites read
/// clearly, the way [`CacheConfig`](crate::single_flight_cache::CacheConfig) is
/// passed to a cache.
pub struct LimiterConfig {
    /// Maximum operations allowed in flight at once.
    pub max_concurrent: usize,
    /// A permit older than this (seconds) is treated as stranded and reclaimed.
    /// Keep it above the longest healthy operation so a live slot is never
    /// reclaimed while still in use.
    pub max_age_secs: u64,
}

/// Caps concurrent operations for one resource. Pure accounting: no globals, no
/// I/O, no clock of its own — `now` is passed in.
pub struct ConcurrencyLimiter {
    config: LimiterConfig,
    /// In-flight permits: id -> claim time.
    slots: BTreeMap<u64, u64>,
    next_id: u64,
}

impl ConcurrencyLimiter {
    pub fn new(config: LimiterConfig) -> Self {
        Self {
            config,
            slots: BTreeMap::new(),
            next_id: 0,
        }
    }

    /// Drop any permit older than `max_age_secs`. Restores slots stranded by a
    /// caller that never released; a no-op on the healthy path.
    fn reclaim_stale(&mut self, now: u64) {
        let stale: Vec<u64> = self
            .slots
            .iter()
            .filter(|(_, &claimed_at)| now.saturating_sub(claimed_at) >= self.config.max_age_secs)
            .map(|(id, _)| *id)
            .collect();
        for id in stale {
            self.slots.remove(&id);
        }
    }

    /// Reserve a slot, or `None` if the budget is full. Sweeps stranded permits
    /// first, so a leak can never permanently wedge the budget.
    fn try_acquire(&mut self, now: u64) -> Option<u64> {
        self.reclaim_stale(now);
        if self.slots.len() >= self.config.max_concurrent {
            return None;
        }
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);
        self.slots.insert(id, now);
        Some(id)
    }

    /// Release the slot held by `id` (idempotent — an unknown id is a no-op).
    fn release(&mut self, id: u64) {
        self.slots.remove(&id);
    }

    #[cfg(test)]
    fn in_use(&self) -> usize {
        self.slots.len()
    }
}

/// A limiter held in a `thread_local`, e.g. `&FOO` for
/// `thread_local! { static FOO: RefCell<ConcurrencyLimiter> = ... }`.
type Limiter = &'static LocalKey<RefCell<ConcurrencyLimiter>>;

/// A reserved slot. Holding it holds the slot; dropping it frees the slot, so a
/// slot lives exactly as long as the permit — hold it across the work it guards.
#[must_use = "the reserved slot is released as soon as the permit is dropped"]
pub struct Permit {
    limiter: Limiter,
    id: u64,
}

impl Drop for Permit {
    fn drop(&mut self) {
        self.limiter.with_borrow_mut(|l| l.release(self.id));
    }
}

/// Reserve a slot from `limiter`, or `None` when its budget is full. Hold the
/// returned [`Permit`] for the duration of the guarded work; drop it to release.
pub fn acquire(limiter: Limiter) -> Option<Permit> {
    let now = now_secs();
    limiter
        .with_borrow_mut(|l| l.try_acquire(now))
        .map(|id| Permit { limiter, id })
}

#[cfg(not(test))]
fn now_secs() -> u64 {
    ic_cdk::api::time() / 1_000_000_000
}

// ---- test-only clock ----

#[cfg(test)]
thread_local! {
    static TEST_NOW: std::cell::Cell<u64> = const { std::cell::Cell::new(1_700_000_000) };
}

#[cfg(test)]
fn now_secs() -> u64 {
    TEST_NOW.with(|c| c.get())
}

#[cfg(test)]
mod tests {
    use super::*;

    // --- pure limiter accounting (clock passed in explicitly) ---

    #[test]
    fn acquires_up_to_the_budget_then_refuses() {
        let mut l = ConcurrencyLimiter::new(LimiterConfig { max_concurrent: 3, max_age_secs: 100 });
        let ids: Vec<_> = (0..3).map(|_| l.try_acquire(0).expect("within budget")).collect();
        assert_eq!(ids.len(), 3);
        assert!(l.try_acquire(0).is_none(), "budget of 3 reached");
    }

    #[test]
    fn releasing_frees_a_slot() {
        let mut l = ConcurrencyLimiter::new(LimiterConfig { max_concurrent: 2, max_age_secs: 100 });
        let a = l.try_acquire(0).unwrap();
        let _b = l.try_acquire(0).unwrap();
        assert!(l.try_acquire(0).is_none(), "saturated");
        l.release(a);
        assert!(l.try_acquire(0).is_some(), "a freed slot is re-acquirable");
    }

    #[test]
    fn a_stranded_permit_is_reclaimed_after_max_age() {
        let mut l = ConcurrencyLimiter::new(LimiterConfig { max_concurrent: 2, max_age_secs: 100 });
        // Leak both slots: acquire and drop the ids without releasing.
        let _ = l.try_acquire(0).unwrap();
        let _ = l.try_acquire(0).unwrap();
        assert!(l.try_acquire(0).is_none(), "leaked to the budget");
        assert!(
            l.try_acquire(100).is_some(),
            "permits older than max_age_secs are reclaimed"
        );
    }

    #[test]
    fn a_permit_younger_than_max_age_is_not_reclaimed() {
        let mut l = ConcurrencyLimiter::new(LimiterConfig { max_concurrent: 1, max_age_secs: 100 });
        let _ = l.try_acquire(0).unwrap();
        assert!(
            l.try_acquire(99).is_none(),
            "one second before max_age the slot is still held"
        );
    }

    // --- the `acquire` / `Permit` API over a thread_local limiter ---

    thread_local! {
        static TEST_LIMITER: RefCell<ConcurrencyLimiter> =
            RefCell::new(ConcurrencyLimiter::new(LimiterConfig { max_concurrent: 2, max_age_secs: 100 }));
    }

    fn reset_test_limiter() {
        TEST_LIMITER.with_borrow_mut(|l| *l = ConcurrencyLimiter::new(LimiterConfig { max_concurrent: 2, max_age_secs: 100 }));
    }

    #[test]
    fn acquire_hands_out_permits_up_to_the_budget_then_none() {
        reset_test_limiter();
        let held: Vec<_> = std::iter::from_fn(|| acquire(&TEST_LIMITER)).collect();
        assert_eq!(held.len(), 2, "budget of 2");
        assert!(acquire(&TEST_LIMITER).is_none(), "budget-plus-one is refused");
    }

    #[test]
    fn dropping_a_permit_frees_a_slot() {
        reset_test_limiter();
        let mut held: Vec<_> = std::iter::from_fn(|| acquire(&TEST_LIMITER)).collect();
        assert!(acquire(&TEST_LIMITER).is_none(), "saturated");
        drop(held.pop()); // release one permit
        assert_eq!(TEST_LIMITER.with_borrow(ConcurrencyLimiter::in_use), 1);
        assert!(acquire(&TEST_LIMITER).is_some(), "a freed slot is re-acquirable");
    }
}
