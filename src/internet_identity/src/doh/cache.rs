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
//!    for the same domain and the cache is cold, all of them share a
//!    single underlying outcall fan-out. The first arrival fires the
//!    outcalls and writes the result; subsequent arrivals register a
//!    `Waker` against the in-flight entry and `await` the publication.
//!
//! The dedup mechanism is a hand-rolled `oneshot`-style primitive
//! (see [`Pending`]) — `ic-cdk` doesn't ship with the tokio sync
//! primitives we'd otherwise reach for, but the standard Rust `Waker`
//! machinery is enough since the canister is single-threaded inside
//! one wasm execution.

use std::cell::RefCell;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::rc::Rc;
use std::task::{Context, Poll, Waker};

use super::types::DohError;

/// How long a `Pending` entry can sit in the cache before a new
/// arrival treats it as abandoned.
///
/// The IC doesn't roll back state across `.await` points: state
/// changes that happened *before* the yield are committed even if the
/// resumed-message continuation traps. So if the post-outcall code
/// path traps, the `Pending` entry inserted by the first arrival
/// stays in the map and every subsequent caller would `Wait` on a
/// `PendingState::InFlight` that nothing will ever resolve — until
/// canister upgrade.
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

/// Shared state used by the dedup primitive. `Rc<RefCell<...>>` because
/// the canister's wasm execution is single-threaded; Send/Sync don't
/// apply.
type SharedPending = Rc<RefCell<PendingState>>;

#[derive(Debug)]
enum PendingState {
    /// The first arrival is mid-fetch. Each subsequent arrival has
    /// registered its `Waker` here and will be re-polled when the
    /// fetch publishes a result.
    InFlight { wakers: Vec<Waker> },
    /// The fetch has completed. Late arrivals see this and pick up
    /// the result without waiting.
    Done(Result<Vec<u8>, DohError>),
}

/// What's stored in the `pending` map. We track `started_at_secs` so a
/// later arrival can detect an abandoned in-flight fetch (see
/// [`PENDING_STALE_AFTER_SECS`]) and start over rather than `Wait`ing
/// forever.
struct PendingEntry {
    state: SharedPending,
    started_at_secs: u64,
}

/// In-memory cache. The two maps are keyed by FQDN (e.g.
/// `selector1._domainkey.gmail.com`).
#[derive(Default)]
pub struct DohCache {
    entries: HashMap<String, CacheEntry>,
    pending: HashMap<String, PendingEntry>,
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

    /// Remove a stale entry (by name). Used when freshness has lapsed
    /// and we're about to re-fetch.
    pub fn invalidate(&mut self, name: &str) {
        self.entries.remove(name);
    }

    /// Decide what to do for an incoming fetch request:
    /// - `Hit(bytes)` — cache fresh, use immediately.
    /// - `Wait(future)` — someone else is fetching, await this future
    ///   and you'll be woken with their result.
    /// - `Fetch(token)` — you're the first; do the fetch, then call
    ///   [`Self::publish`] passing the token back.
    ///
    /// If we find a `Pending` entry that's older than
    /// [`PENDING_STALE_AFTER_SECS`], we treat it as abandoned (the
    /// continuation that should have published it likely trapped),
    /// wake any orphan waiters with [`DohError::AllProvidersFailed`]
    /// so they don't hang, and start a fresh fetch.
    ///
    /// Also opportunistically evicts the looked-up entry if we find
    /// it expired — keeps the cache from accumulating dead entries
    /// for FQDNs that get queried once and then go silent.
    pub fn lookup(&mut self, name: &str, now_secs: u64) -> CacheLookup {
        if let Some(entry) = self.entries.get(name) {
            if entry.expires_at_secs > now_secs {
                return CacheLookup::Hit(entry.bytes.clone());
            }
            // Expired: drop it now rather than waiting for a publish
            // to overwrite — most expired entries never see a republish.
            self.entries.remove(name);
        }
        if let Some(existing) = self.pending.get(name) {
            let age = now_secs.saturating_sub(existing.started_at_secs);
            if age < PENDING_STALE_AFTER_SECS {
                return CacheLookup::Wait(WaitForPending {
                    state: existing.state.clone(),
                });
            }
            // Abandoned: evict and wake any orphan waiters. The
            // SharedPending may still be held by `WaitForPending`
            // futures that registered before eviction; flipping it to
            // `Done(Err)` resolves their `await` instead of hanging.
            let stale = self.pending.remove(name).expect("just checked");
            wake_with(&stale.state, Err(DohError::AllProvidersFailed));
        }
        let state = Rc::new(RefCell::new(PendingState::InFlight { wakers: Vec::new() }));
        self.pending.insert(
            name.to_string(),
            PendingEntry {
                state: state.clone(),
                started_at_secs: now_secs,
            },
        );
        CacheLookup::Fetch(FetchToken { state })
    }

    /// Publish the result of the in-flight fetch for `name`.
    ///
    /// The `token` is whatever [`Self::lookup`] handed back when this
    /// caller got the `Fetch` arm — passing it back lets us detect
    /// the case where this fetch was evicted by a stale-takeover
    /// (another caller saw the pending entry as abandoned and started
    /// over). In that case we don't touch the cache map but we still
    /// wake any subscribers tied to *our* `PendingState`, so even
    /// orphaned waiters resolve cleanly.
    ///
    /// Also opportunistically sweeps any value entries that are
    /// already expired at `now_secs`. Sweeping on publish (the rare
    /// write path) rather than on every `lookup` keeps reads cheap
    /// while still bounding cache size — every successful fetch
    /// drops the dead weight that accumulated since the last write.
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
            .pending
            .get(name)
            .is_some_and(|e| Rc::ptr_eq(&e.state, &token.state));
        if still_ours {
            if let Ok(bytes) = &result {
                self.insert(name, bytes.clone(), expires_at_secs);
            }
            self.pending.remove(name);
        }
        wake_with(&token.state, result);
    }

    /// Drop every value entry whose `expires_at_secs` is at or before
    /// `now_secs`. Linear in `entries.len()`, which is bounded by
    /// "unique FQDNs queried in the last `max_cache_age_secs`" — small
    /// in practice. Called from `publish` so the per-fetch cost is
    /// amortised across cache hits.
    fn sweep_expired(&mut self, now_secs: u64) {
        self.entries.retain(|_, e| e.expires_at_secs > now_secs);
    }
}

/// Flip a `PendingState` to `Done(result)` and wake every registered
/// waker. Idempotent: if the state is already `Done`, this is a no-op
/// for the wakers (they were woken on the first transition).
fn wake_with(state: &SharedPending, result: Result<Vec<u8>, DohError>) {
    let mut s = state.borrow_mut();
    let prev = std::mem::replace(&mut *s, PendingState::Done(result));
    if let PendingState::InFlight { wakers } = prev {
        for w in wakers {
            w.wake();
        }
    }
}

/// What [`DohCache::lookup`] returned. The caller acts on the variant.
pub enum CacheLookup {
    Hit(Vec<u8>),
    Wait(WaitForPending),
    Fetch(FetchToken),
}

/// Opaque handle a `Fetch` caller passes back to [`DohCache::publish`].
/// It identifies "this particular in-flight attempt" so the cache can
/// tell whether the publishing caller is still the owner or whether a
/// stale-takeover replaced them in the interim.
pub struct FetchToken {
    state: SharedPending,
}

/// Future returned to a caller that arrived while another task was
/// already fetching the same name. Polls the shared `Pending` state;
/// returns `Ready` once the in-flight task publishes its result.
pub struct WaitForPending {
    state: SharedPending,
}

impl Future for WaitForPending {
    type Output = Result<Vec<u8>, DohError>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut state = self.state.borrow_mut();
        match &mut *state {
            PendingState::Done(r) => Poll::Ready(r.clone()),
            PendingState::InFlight { wakers } => {
                // Avoid registering the same waker twice if the future
                // is re-polled spuriously: walk the existing wakers
                // and replace any that say `will_wake` of this one.
                let new = cx.waker();
                if !wakers.iter().any(|w| w.will_wake(new)) {
                    wakers.push(new.clone());
                }
                Poll::Pending
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};
    use std::sync::Arc;
    use std::task::Wake;

    /// Simple `Wake` that counts how many times it's been woken.
    /// Lets the dedup tests assert "publishing the result wakes every
    /// subscriber exactly once".
    struct CountingWaker {
        count: AtomicUsize,
    }

    impl Wake for CountingWaker {
        fn wake(self: Arc<Self>) {
            self.count.fetch_add(1, Ordering::SeqCst);
        }
    }

    fn make_waker(w: &Arc<CountingWaker>) -> Waker {
        Waker::from(w.clone())
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

    /// Helper: take the `Fetch` arm or fail the test.
    fn expect_fetch(lookup: CacheLookup) -> FetchToken {
        match lookup {
            CacheLookup::Fetch(t) => t,
            _ => panic!("expected Fetch"),
        }
    }

    #[test]
    fn first_lookup_returns_fetch() {
        let mut cache = DohCache::default();
        let _token = expect_fetch(cache.lookup("a.com", 0));
        // After Fetch, the pending map should have an entry.
        assert!(cache.pending.contains_key("a.com"));
    }

    #[test]
    fn second_lookup_returns_wait() {
        let mut cache = DohCache::default();
        let _token = expect_fetch(cache.lookup("a.com", 0));
        match cache.lookup("a.com", 0) {
            CacheLookup::Wait(_) => {}
            _ => panic!("expected Wait"),
        }
    }

    #[test]
    fn publish_wakes_all_subscribers() {
        let mut cache = DohCache::default();
        let token = expect_fetch(cache.lookup("a.com", 0));

        // Two subscribers register their wakers.
        let w1 = Arc::new(CountingWaker {
            count: AtomicUsize::new(0),
        });
        let w2 = Arc::new(CountingWaker {
            count: AtomicUsize::new(0),
        });

        let waker_1 = make_waker(&w1);
        match cache.lookup("a.com", 0) {
            CacheLookup::Wait(mut fut) => {
                let mut cx = Context::from_waker(&waker_1);
                assert!(matches!(Pin::new(&mut fut).poll(&mut cx), Poll::Pending));
            }
            _ => panic!("expected Wait"),
        }
        let waker_2 = make_waker(&w2);
        match cache.lookup("a.com", 0) {
            CacheLookup::Wait(mut fut) => {
                let mut cx = Context::from_waker(&waker_2);
                assert!(matches!(Pin::new(&mut fut).poll(&mut cx), Poll::Pending));
            }
            _ => panic!("expected Wait"),
        }

        cache.publish("a.com", token, Ok(b"hello".to_vec()), 1000, 0);
        assert_eq!(w1.count.load(Ordering::SeqCst), 1);
        assert_eq!(w2.count.load(Ordering::SeqCst), 1);

        assert!(!cache.pending.contains_key("a.com"));
        assert_eq!(cache.get_fresh("a.com", 999), Some(b"hello".to_vec()));
    }

    #[test]
    fn published_subscriber_resolves_to_result() {
        let mut cache = DohCache::default();
        let token = expect_fetch(cache.lookup("a.com", 0));
        let mut wait = match cache.lookup("a.com", 0) {
            CacheLookup::Wait(f) => f,
            _ => panic!(),
        };

        cache.publish("a.com", token, Ok(b"hello".to_vec()), 1000, 0);

        let w = Arc::new(CountingWaker {
            count: AtomicUsize::new(0),
        });
        let waker = make_waker(&w);
        let mut cx = Context::from_waker(&waker);
        match Pin::new(&mut wait).poll(&mut cx) {
            Poll::Ready(Ok(b)) => assert_eq!(b, b"hello"),
            other => panic!("expected Ready(Ok), got {:?}", other),
        }
    }

    #[test]
    fn publish_on_failure_does_not_cache_but_still_wakes() {
        let mut cache = DohCache::default();
        let token = expect_fetch(cache.lookup("a.com", 0));

        let w = Arc::new(CountingWaker {
            count: AtomicUsize::new(0),
        });
        let waker = make_waker(&w);
        match cache.lookup("a.com", 0) {
            CacheLookup::Wait(mut fut) => {
                let mut cx = Context::from_waker(&waker);
                let _ = Pin::new(&mut fut).poll(&mut cx);
            }
            _ => panic!(),
        }
        cache.publish("a.com", token, Err(DohError::AllProvidersFailed), 1000, 0);
        assert_eq!(w.count.load(Ordering::SeqCst), 1);
        assert!(cache.get_fresh("a.com", 0).is_none());
    }

    // ---- Stale-pending eviction ----

    #[test]
    fn fresh_pending_within_threshold_returns_wait() {
        let mut cache = DohCache::default();
        let _orig = expect_fetch(cache.lookup("a.com", 100));
        // Just under the threshold: still fresh.
        match cache.lookup("a.com", 100 + PENDING_STALE_AFTER_SECS - 1) {
            CacheLookup::Wait(_) => {}
            _ => panic!("expected Wait while pending is still fresh"),
        }
    }

    #[test]
    fn stale_pending_is_evicted_and_caller_gets_fetch() {
        let mut cache = DohCache::default();
        let _abandoned = expect_fetch(cache.lookup("a.com", 100));
        // Past the staleness threshold — the prior fetch is presumed
        // dead. The new arrival should get a fresh Fetch, not Wait.
        let _new_token = expect_fetch(cache.lookup("a.com", 100 + PENDING_STALE_AFTER_SECS));
    }

    #[test]
    fn stale_eviction_wakes_orphan_waiters_with_error() {
        // Setup: A starts a fetch, B registers as a waiter while A is
        // still in flight, then A's continuation never runs (simulated
        // here by just letting time pass without calling publish). C
        // arrives after the staleness threshold. B should get woken
        // with an error rather than hanging on a pending state forever.
        let mut cache = DohCache::default();
        let _a_token = expect_fetch(cache.lookup("a.com", 100));

        let mut b_wait = match cache.lookup("a.com", 110) {
            CacheLookup::Wait(f) => f,
            _ => panic!("B should have got Wait"),
        };
        let waker_b = Arc::new(CountingWaker {
            count: AtomicUsize::new(0),
        });
        let waker = make_waker(&waker_b);
        let mut cx = Context::from_waker(&waker);
        assert!(matches!(Pin::new(&mut b_wait).poll(&mut cx), Poll::Pending));

        // C arrives way past the threshold.
        let _c_token = expect_fetch(cache.lookup("a.com", 100 + PENDING_STALE_AFTER_SECS + 1));

        // B's waker should have fired during eviction.
        assert_eq!(
            waker_b.count.load(Ordering::SeqCst),
            1,
            "orphan waiter must be woken on stale eviction"
        );

        // And the next poll resolves to an error rather than Pending.
        match Pin::new(&mut b_wait).poll(&mut cx) {
            Poll::Ready(Err(DohError::AllProvidersFailed)) => {}
            other => panic!("orphan waiter should resolve to AllProvidersFailed, got {other:?}"),
        }
    }

    // ---- Eviction of expired value entries ----

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

    #[test]
    fn evicted_owner_publishing_does_not_overwrite_new_fetch() {
        // A is evicted, C starts a new fetch. A then unexpectedly
        // returns and tries to publish. The cache must NOT replace
        // C's pending entry or write A's data into the value cache.
        let mut cache = DohCache::default();
        let a_token = expect_fetch(cache.lookup("a.com", 100));
        let c_token = expect_fetch(cache.lookup("a.com", 100 + PENDING_STALE_AFTER_SECS + 1));

        // A's late publish: no-op on cache state.
        cache.publish("a.com", a_token, Ok(b"A's stale data".to_vec()), 9999, 0);
        assert!(
            cache.get_fresh("a.com", 0).is_none(),
            "A's publish must not write to the cache after eviction"
        );
        assert!(
            cache.pending.contains_key("a.com"),
            "C's pending entry must still be in place"
        );

        // Now C publishes its real result and that DOES land.
        cache.publish("a.com", c_token, Ok(b"C's fresh data".to_vec()), 9999, 0);
        assert_eq!(
            cache.get_fresh("a.com", 0),
            Some(b"C's fresh data".to_vec())
        );
    }
}
