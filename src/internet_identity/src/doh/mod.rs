//! DoH (DNS-over-HTTPS) fallback for fetching DKIM/DMARC TXT records
//! when the queried domain isn't DNSSEC-signed.
//!
//! Design constraints (from the team Slack writeup + design doc §7.6):
//!
//! - **Allowlist-gated.** Outcalls happen only for domains explicitly
//!   listed in the deploy/upgrade-arg `DohConfig.allowed_domains`. The
//!   verifier never goes to the internet for an arbitrary domain.
//! - **Lazy.** No background polling. The cache is populated on
//!   demand the first time a real `smtp_request` arrives for a listed
//!   domain.
//! - **Quorum across independent operators.** Every cache miss fires
//!   parallel outcalls to five providers across four jurisdictions
//!   (Cloudflare 🇺🇸, Google 🇺🇸, Quad9 🇨🇭, CIRA Canadian Shield 🇨🇦,
//!   IIJ 🇯🇵) and accepts the result iff at least 3-of-5 agree on the
//!   TXT bytes. No single operator is trusted; one provider being down
//!   or returning forged bytes never breaks the verifier.
//! - **In-flight dedup.** Multiple concurrent verification requests
//!   for the same domain share one outcall fan-out, not five each.
//! - **Heap cache.** Fast and cheap. Keys are re-fetchable, so an
//!   upgrade rebuilding the cache from scratch is fine.
//!
//! ## Replica-consensus shape
//!
//! HTTP outcalls run on every replica of the subnet (typically 13).
//! For consensus, every replica must observe the same response bytes,
//! which raw DoH responses don't satisfy: TTLs decrement second by
//! second, so two replicas making the query a few hundred ms apart
//! see different bytes. We attach a `transform` query function that
//! parses the wire-format DNS response down to the TXT RDATA — that
//! reduction is deterministic, so all replicas converge on the same
//! bytes and consensus succeeds.

#![allow(dead_code)]

mod cache;
mod parser;
mod quorum;
mod types;

use std::cell::RefCell;

use cache::{CacheLookup, DohCache};
use parser::build_txt_query;
use quorum::{decide_quorum, Outcome};

#[allow(unused_imports)]
pub use parser::{parse_txt_response, ParseError};
#[allow(unused_imports)]
pub use types::{DohError, DohProvider, PROVIDERS};

use types::{DEFAULT_CACHE_AGE_SECS, MAX_CACHE_AGE_SECS};

thread_local! {
    /// Per-canister DoH cache. Heap-only — losing it on upgrade is
    /// fine, the next `smtp_request` for an affected domain just
    /// re-fetches.
    static DOH_CACHE: RefCell<DohCache> = RefCell::new(DohCache::default());
}

// =====================================================================
// Transform / outcall classification: pure helpers shared by the prod
// outcall plumbing (next section) and the unit tests (cfg(test)).
//
// `transform_doh` and `outcall` need each other's status conventions to
// stay in lockstep, but only `outcall` can be exercised live in tests.
// Extracting the two pure step functions here keeps the security-
// critical invariant — "an `Outcome::NoAnswer` can only originate from
// our own transform, never from an upstream-reported status" — testable
// without spinning up an outcall harness.
// =====================================================================

/// HTTP 200. The transform emits this iff `parse_txt_response`
/// returned an in-band TXT payload (placed in `body`).
const HTTP_STATUS_OK: u16 = 200;

/// HTTP 204 ("No Content"). The transform emits this iff
/// `parse_txt_response` returned [`ParseError::NoAnswer`] — i.e. the
/// upstream's wire-format response carried RCODE=3 (NXDOMAIN) or an
/// RCODE=0 empty answer section. [`status_to_outcome`] maps it to
/// [`Outcome::NoAnswer`].
const HTTP_STATUS_TRANSFORM_NO_ANSWER: u16 = 204;

/// HTTP 422 ("Unprocessable Entity"). The transform emits this iff
/// `parse_txt_response` returned a non-NoAnswer parse error — the
/// upstream returned 200 but the body wasn't a parseable DNS message.
/// We signal failure via the status code (with an empty body) rather
/// than via a sentinel body string because any TXT-bytes body the
/// transform might emit could collide with a legitimate (if unusual)
/// TXT payload.
const HTTP_STATUS_TRANSFORM_PARSE_FAILED: u16 = 422;

/// HTTP 502 ("Bad Gateway"). The transform emits this iff the
/// upstream provider returned anything other than 200 (including a
/// counterfeit 204). We do NOT pass the upstream status through —
/// otherwise a misbehaving or adversarial provider could emit 204 to
/// forge our `NoAnswer` sentinel and trick the quorum into reporting
/// "this record doesn't exist" when in fact the provider was just
/// broken. Forcing every upstream non-200 onto a fixed sentinel that
/// is *not* 204 makes "the answer is `Outcome::NoAnswer`" a property
/// only our transform can grant.
const HTTP_STATUS_TRANSFORM_UPSTREAM_ERROR: u16 = 502;

/// Map an upstream `(status, body)` pair to the `(status, body)` the
/// transform will return to consensus. Pure function so we can unit-
/// test the security property "an upstream 204 doesn't pass through
/// as our NoAnswer sentinel" without going through the IC outcall
/// harness.
fn classify_upstream(upstream_status: u16, upstream_body: &[u8]) -> (u16, Vec<u8>) {
    if upstream_status != HTTP_STATUS_OK {
        // Any non-200 from upstream — including a counterfeit 204 —
        // collapses onto a single sentinel that callers map to a
        // generic FetchError. See the doc on
        // `HTTP_STATUS_TRANSFORM_UPSTREAM_ERROR`.
        return (HTTP_STATUS_TRANSFORM_UPSTREAM_ERROR, Vec::new());
    }
    match parse_txt_response(upstream_body) {
        Ok(txt) => (HTTP_STATUS_OK, txt),
        Err(ParseError::NoAnswer) => (HTTP_STATUS_TRANSFORM_NO_ANSWER, Vec::new()),
        Err(_) => (HTTP_STATUS_TRANSFORM_PARSE_FAILED, Vec::new()),
    }
}

/// Map a post-transform `(status, body)` pair to the [`Outcome`] the
/// quorum step counts. Pure function, paired with [`classify_upstream`]
/// — anything the transform might emit MUST land in exactly one
/// branch here, and the only branch that yields [`Outcome::NoAnswer`]
/// is the one keyed off [`HTTP_STATUS_TRANSFORM_NO_ANSWER`].
fn status_to_outcome(status: u16, body: Vec<u8>) -> Outcome {
    match status {
        HTTP_STATUS_OK => Outcome::Txt(body),
        HTTP_STATUS_TRANSFORM_NO_ANSWER => Outcome::NoAnswer,
        // Includes both the parse-failed sentinel (422) and the
        // upstream-error sentinel (502); the quorum step doesn't
        // distinguish, and the format string preserves the code for
        // logs.
        other => Outcome::FetchError(format!("HTTP {other}")),
    }
}

/// Public entry point. Returns the TXT bytes for `name` if at least
/// the quorum threshold of providers agree.
///
/// `name` is the full FQDN (e.g., `selector1._domainkey.gmail.com`);
/// `registered_domain` (e.g., `gmail.com`) is what the allowlist gate
/// checks. The caller — the DKIM/DMARC code — already extracts the
/// registered domain when validating From-header alignment, so we
/// don't re-implement that here.
///
/// The function never panics on misconfiguration: if `DohConfig` is
/// absent, returns [`DohError::NotConfigured`] without touching the
/// network.
pub async fn fetch_txt(name: &str, registered_domain: &str) -> Result<Vec<u8>, DohError> {
    let config = match crate::state::persistent_state(|p| p.doh_config.clone()) {
        Some(c) => c,
        None => {
            return Err(DohError::NotConfigured);
        }
    };
    if !config
        .allowed_domains
        .iter()
        .any(|d| d.eq_ignore_ascii_case(registered_domain))
    {
        return Err(DohError::DomainNotAllowed);
    }
    // Defence-in-depth: the allowlist gate above only checks the
    // registered_domain the caller hands us — but `name` is what we'd
    // actually query. A caller bug or future regression that decoupled
    // the two could pass an allowlisted registered_domain alongside a
    // completely unrelated name (e.g., `evil.com`) and we'd happily
    // outcall for it. Insist that `name` sits inside `registered_domain`
    // with a label-anchored suffix match.
    if !name_within_domain(name, registered_domain) {
        return Err(DohError::NameOutsideRegisteredDomain {
            name: name.to_string(),
            registered_domain: registered_domain.to_string(),
        });
    }
    let max_age = config
        .max_cache_age_secs
        .unwrap_or(DEFAULT_CACHE_AGE_SECS)
        .min(MAX_CACHE_AGE_SECS);

    // Build the query early so an InvalidName failure short-circuits
    // before we touch the cache or claim a Fetch token.
    let query = build_txt_query(name).map_err(|e| DohError::InvalidName(format!("{e:?}")))?;

    // Dedup-aware fetch. The first arrival for a cold name owns the
    // five-provider fan-out; concurrent arrivals observe the in-flight
    // marker and poll the cache until it publishes. Each waiter yields a
    // round from its OWN call context (`yield_round`) and re-checks — it
    // must NOT block on a future the fetcher wakes, because waking
    // another call's task runs it to completion in the fetcher's call
    // context and mis-routes its reply (`ic0.msg_reply ... already
    // replied`). The cache borrow is released before each await: the
    // canister is single-threaded and a borrow held across a yield would
    // trap.
    let mut waits = 0u32;
    let token = loop {
        let now = now_secs();
        match DOH_CACHE.with(|c| c.borrow_mut().lookup(name, now)) {
            CacheLookup::Hit(bytes) => return Ok(bytes),
            CacheLookup::Fetch(token) => break token,
            CacheLookup::Wait => {
                if waits >= MAX_DEDUP_WAIT_POLLS {
                    // The in-flight fetch is taking implausibly long (or
                    // its owner trapped before the staleness window let
                    // someone take over). Fail transiently rather than
                    // poll forever; a retry — or the post-staleness
                    // takeover — resolves it.
                    return Err(DohError::AllProvidersFailed);
                }
                waits += 1;
                yield_round().await;
            }
        }
    };

    // We own the fetch: fan out to every provider in parallel, decide,
    // then publish (writes the value entry on success, clears our
    // in-flight marker either way).
    let outcomes = fetch_all(&query).await;
    let result = decide_quorum(&outcomes);
    let now = now_secs();
    let expires_at = now.saturating_add(max_age);
    DOH_CACHE.with(|c| {
        c.borrow_mut()
            .publish(name, token, result.clone(), expires_at, now)
    });
    result
}

/// Whether a queried FQDN sits inside `registered_domain` (case-
/// insensitive, label-anchored).
///
/// Returns true iff `name == registered_domain` or `name` ends with
/// `.<registered_domain>`. The dot anchor is what stops
/// `evilexample.com` from passing as inside `example.com`. A trailing
/// dot on either side (root marker) is normalised away first.
fn name_within_domain(name: &str, registered_domain: &str) -> bool {
    let n = name.trim_end_matches('.');
    let d = registered_domain.trim_end_matches('.');
    if d.is_empty() {
        return false;
    }
    if n.eq_ignore_ascii_case(d) {
        return true;
    }
    if n.len() <= d.len() + 1 {
        return false;
    }
    let suffix = &n[n.len() - d.len()..];
    let dot_idx = n.len() - d.len() - 1;
    suffix.eq_ignore_ascii_case(d) && n.as_bytes()[dot_idx] == b'.'
}

#[cfg(not(test))]
fn now_secs() -> u64 {
    ic_cdk::api::time() / 1_000_000_000
}

/// Test-time clock. The canister-time accessor traps outside a real
/// canister, so under `cfg(test)` we read from a thread-local the test
/// can advance.
#[cfg(test)]
fn now_secs() -> u64 {
    test_support::TEST_NOW_SECS.with(|t| *t.borrow())
}

/// Upper bound on how many times a dedup waiter polls the cache before
/// giving up. Each poll yields one consensus round via [`yield_round`],
/// so this is ~100 rounds of patience — far more than a healthy fan-out
/// needs — and bounds the wait so a wedged fetch can't pin a waiter
/// indefinitely. The post-staleness takeover (see
/// [`cache::PENDING_STALE_AFTER_SECS`]) is the real recovery path; this
/// is just a backstop against an unbounded poll loop.
const MAX_DEDUP_WAIT_POLLS: u32 = 100;

/// Yield one consensus round, resuming in the caller's OWN call context.
///
/// A dedup waiter uses this to let the in-flight fetch make progress and
/// then re-check the cache, *without* being driven by the fetcher's
/// wake. A timer is no good — its callback can't reply to the waiter's
/// ingress call — and completing inside another call's context
/// mis-routes the reply. Awaiting a cheap management-canister round-trip
/// is the idiomatic way (already used elsewhere in this canister) to
/// give up a round and come back in our own context; the random bytes
/// are unused.
#[cfg(not(test))]
async fn yield_round() {
    let _ =
        ic_cdk::call::<(), (Vec<u8>,)>(candid::Principal::management_canister(), "raw_rand", ())
            .await;
}

/// Host-test stand-in: unit tests drive `fetch_txt` sequentially with a
/// mocked fan-out, so the dedup `Wait` path is never exercised and there
/// is nothing to yield to.
#[cfg(test)]
async fn yield_round() {}

// =====================================================================
// Production outcall path (cfg(not(test)) only).
//
// Each cache miss fans out to all PROVIDERS in parallel via
// `futures::future::join_all`. Each individual outcall:
//   - POSTs the wire-format query to the provider's /dns-query.
//   - Caps response size at 4 KiB (cycles charged on cap, not actual).
//   - Attaches a transform that reduces the wire response to its TXT
//     RDATA so replicas converge.
//   - Attaches a fixed cycle budget per outcall.
// =====================================================================

#[cfg(not(test))]
mod prod {
    use super::{classify_upstream, status_to_outcome, Outcome};
    use ic_cdk::api::management_canister::http_request::{
        http_request_with_closure, CanisterHttpRequestArgument, HttpHeader, HttpMethod,
        HttpResponse,
    };

    /// 4 KiB upper bound on a DoH response. A typical DKIM-SHA256 TXT
    /// record (RSA-2048) sits well under 1 KiB; allowing four times
    /// that gives slack for multi-string TXT and oversized
    /// experimental keys without paying 2 MiB worth of cycles.
    pub(super) const MAX_DOH_RESPONSE_BYTES: u64 = 4096;

    /// Per-outcall cycle budget. Mirrors the OIDC discovery path's
    /// budget (`30_000_000_000`) — generous enough to cover the 13-
    /// replica fan-out plus the transform invocation, with refund of
    /// unused cycles. Each cache miss spends ~5× this in total.
    pub(super) const DOH_CALL_CYCLES: u128 = 30_000_000_000;

    pub(super) async fn fetch_all(query: &[u8]) -> Vec<Outcome> {
        let futures: Vec<_> = super::PROVIDERS.iter().map(|p| outcall(p, query)).collect();
        futures::future::join_all(futures).await
    }

    async fn outcall(provider: &super::DohProvider, query: &[u8]) -> Outcome {
        let request = CanisterHttpRequestArgument {
            url: provider.url.to_string(),
            method: HttpMethod::POST,
            body: Some(query.to_vec()),
            max_response_bytes: Some(MAX_DOH_RESPONSE_BYTES),
            // The closure-based call below carries the transform; we
            // leave this field None and pass `transform_doh` directly.
            transform: None,
            headers: vec![
                HttpHeader {
                    name: "Accept".into(),
                    value: "application/dns-message".into(),
                },
                HttpHeader {
                    name: "Content-Type".into(),
                    value: "application/dns-message".into(),
                },
                HttpHeader {
                    name: "User-Agent".into(),
                    value: "internet_identity_canister".into(),
                },
            ],
        };
        match http_request_with_closure(request, DOH_CALL_CYCLES, transform_doh).await {
            Ok((response,)) => {
                let status: u16 = response.status.0.try_into().unwrap_or(u16::MAX);
                status_to_outcome(status, response.body)
            }
            Err((_, err)) => Outcome::FetchError(err),
        }
    }

    /// Transform query for replica consensus.
    ///
    /// DoH wire-format responses contain TTLs that decrement once a
    /// second, so replicas making the same query a few hundred ms
    /// apart can observe different bytes — consensus then fails. The
    /// transform reduces the response to a fixed `(status, body)`
    /// pair, computed by the pure [`super::classify_upstream`]
    /// helper, so all replicas converge.
    ///
    /// The classifier owns the rule that an upstream non-200 — even
    /// if the upstream literally returned 204 — gets remapped to the
    /// upstream-error sentinel rather than the NoAnswer sentinel.
    /// Without that remapping a misbehaving or adversarial provider
    /// could counterfeit "no record" by returning 204 directly.
    #[allow(clippy::needless_pass_by_value)]
    pub(super) fn transform_doh(response: HttpResponse) -> HttpResponse {
        use candid::Nat;
        let upstream_status: u16 = response.status.0.try_into().unwrap_or(u16::MAX);
        let (new_status, new_body) = classify_upstream(upstream_status, &response.body);
        HttpResponse {
            status: Nat::from(u32::from(new_status)),
            headers: vec![],
            body: new_body,
        }
    }
}

#[cfg(not(test))]
async fn fetch_all(query: &[u8]) -> Vec<Outcome> {
    prod::fetch_all(query).await
}

// =====================================================================
// Test path: routes outcalls through a thread-local mock so the same
// `fetch_txt` body can be exercised end-to-end without the IC
// management canister. The mock returns *already-parsed* TXT bytes —
// matching the shape of what `transform_doh` produces in production.
// =====================================================================

#[cfg(test)]
async fn fetch_all(query: &[u8]) -> Vec<Outcome> {
    test_support::run_mock(query)
}

#[cfg(test)]
pub(super) mod test_support {
    use super::*;
    use std::collections::HashMap;

    thread_local! {
        pub(super) static TEST_NOW_SECS: RefCell<u64> = const { RefCell::new(1_700_000_000) };
        pub(super) static MOCK_RESPONSES: RefCell<
            HashMap<&'static str, Outcome>,
        > = RefCell::new(HashMap::new());
        pub(super) static MOCK_CALL_COUNT: RefCell<usize> = const { RefCell::new(0) };
    }

    pub(super) fn set_now(t: u64) {
        TEST_NOW_SECS.with(|c| *c.borrow_mut() = t);
    }

    pub(super) fn set_mock(responses: &[(&'static str, Outcome)]) {
        MOCK_RESPONSES.with(|m| {
            let mut m = m.borrow_mut();
            m.clear();
            for (url, r) in responses {
                m.insert(*url, r.clone());
            }
        });
    }

    pub(super) fn call_count() -> usize {
        MOCK_CALL_COUNT.with(|c| *c.borrow())
    }

    /// Wipes both cache + counter so each test starts from a known
    /// state. Call this at the top of every test that asserts on
    /// `call_count`.
    pub(super) fn reset_cache() {
        DOH_CACHE.with(|c| *c.borrow_mut() = DohCache::default());
        MOCK_CALL_COUNT.with(|c| *c.borrow_mut() = 0);
    }

    pub(super) fn run_mock(_query: &[u8]) -> Vec<Outcome> {
        MOCK_CALL_COUNT.with(|c| *c.borrow_mut() += 1);
        MOCK_RESPONSES.with(|m| {
            let m = m.borrow();
            PROVIDERS
                .iter()
                .map(|p| {
                    m.get(p.url)
                        .cloned()
                        .unwrap_or_else(|| Outcome::FetchError("no canned response".into()))
                })
                .collect()
        })
    }
}

#[cfg(test)]
mod tests {
    use super::test_support::*;
    use super::*;
    use crate::state::{persistent_state_mut, PersistentState};
    use internet_identity_interface::internet_identity::types::DohConfig;
    use std::future::Future;
    use std::pin::Pin;
    use std::sync::Arc;
    use std::task::{Context, Poll, Wake, Waker};

    /// Minimal block-on. The async futures in these tests complete in
    /// a single poll (the mock fetcher is synchronous, the cache path
    /// is synchronous, and we don't exercise the dedup `Wait` arm
    /// here). Pulling in `futures::executor` would force the
    /// `executor` + `std` features and bloat the wasm build, so we
    /// poll by hand.
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
            // No I/O drives these futures from outside; if a single
            // poll returns Pending in test, the test setup is wrong.
            Poll::Pending => panic!("test future returned Pending — test setup is incorrect"),
        }
    }

    fn install_config(domains: &[&str], max_age: Option<u64>) {
        persistent_state_mut(|p: &mut PersistentState| {
            p.doh_config = Some(DohConfig {
                allowed_domains: domains.iter().map(|d| (*d).to_string()).collect(),
                max_cache_age_secs: max_age,
            });
        });
    }

    fn clear_config() {
        persistent_state_mut(|p| p.doh_config = None);
    }

    /// The mock-side equivalent of `transform_doh`'s output: just the
    /// TXT bytes a real provider would have parsed out, wrapped in
    /// the same `Outcome` enum the prod outcall path constructs.
    fn ok(bytes: &[u8]) -> Outcome {
        Outcome::Txt(bytes.to_vec())
    }
    fn fail(s: &str) -> Outcome {
        Outcome::FetchError(s.into())
    }
    fn no_answer() -> Outcome {
        Outcome::NoAnswer
    }

    fn agreeing_dkim() -> Vec<(&'static str, Outcome)> {
        // Three provider responses agree, two disagree — quorum (3-of-
        // 5) hits.
        vec![
            (PROVIDERS[0].url, ok(b"v=DKIM1; k=rsa; p=...")),
            (PROVIDERS[1].url, ok(b"v=DKIM1; k=rsa; p=...")),
            (PROVIDERS[2].url, ok(b"v=DKIM1; k=rsa; p=...")),
            (PROVIDERS[3].url, fail("network down")),
            (PROVIDERS[4].url, fail("403")),
        ]
    }

    #[test]
    fn returns_not_configured_without_doh_config() {
        clear_config();
        reset_cache();
        let r = block_on(fetch_txt("selector._domainkey.example.com", "example.com"));
        assert_eq!(r, Err(DohError::NotConfigured));
    }

    #[test]
    fn rejects_unallowed_domain() {
        install_config(&["gmail.com"], None);
        reset_cache();
        let r = block_on(fetch_txt("selector._domainkey.evil.com", "evil.com"));
        assert_eq!(r, Err(DohError::DomainNotAllowed));
    }

    #[test]
    fn allowlist_match_is_case_insensitive() {
        install_config(&["GMAIL.com"], None);
        reset_cache();
        set_mock(&agreeing_dkim());
        let r = block_on(fetch_txt("selector._domainkey.gmail.com", "gmail.com"));
        assert!(r.is_ok(), "expected Ok, got {r:?}");
    }

    #[test]
    fn first_fetch_runs_outcalls_and_caches() {
        install_config(&["example.com"], Some(3600));
        reset_cache();
        set_mock(&agreeing_dkim());
        set_now(1_000);

        let r1 = block_on(fetch_txt("x._domainkey.example.com", "example.com"));
        assert_eq!(r1, Ok(b"v=DKIM1; k=rsa; p=...".to_vec()));
        assert_eq!(call_count(), 1, "first call should hit the network");

        // Second call within TTL: cache hit, no outcall.
        set_now(1_500);
        let r2 = block_on(fetch_txt("x._domainkey.example.com", "example.com"));
        assert_eq!(r2, Ok(b"v=DKIM1; k=rsa; p=...".to_vec()));
        assert_eq!(call_count(), 1, "second call should be cache-served");
    }

    #[test]
    fn refetches_after_ttl_expires() {
        install_config(&["example.com"], Some(60));
        reset_cache();
        set_mock(&agreeing_dkim());
        set_now(1_000);

        let _ = block_on(fetch_txt("x._domainkey.example.com", "example.com"));
        assert_eq!(call_count(), 1);

        // Advance past the TTL — the cache should be cold again.
        set_now(1_000 + 60 + 1);
        let _ = block_on(fetch_txt("x._domainkey.example.com", "example.com"));
        assert_eq!(call_count(), 2, "should re-fetch after TTL");
    }

    #[test]
    fn quorum_failure_is_not_cached() {
        install_config(&["example.com"], Some(3600));
        reset_cache();
        // Five distinct answers — biggest bucket is 1, well short of
        // the 3-of-5 threshold.
        set_mock(&[
            (PROVIDERS[0].url, ok(b"a")),
            (PROVIDERS[1].url, ok(b"b")),
            (PROVIDERS[2].url, ok(b"c")),
            (PROVIDERS[3].url, ok(b"d")),
            (PROVIDERS[4].url, ok(b"e")),
        ]);
        set_now(1_000);

        let r1 = block_on(fetch_txt("x._domainkey.example.com", "example.com"));
        assert!(matches!(r1, Err(DohError::QuorumFailed { .. })));

        // Failure must NOT poison the cache: a follow-up call (with
        // different mock) should re-fetch and now succeed.
        set_mock(&agreeing_dkim());
        set_now(1_001);
        let r2 = block_on(fetch_txt("x._domainkey.example.com", "example.com"));
        assert_eq!(r2, Ok(b"v=DKIM1; k=rsa; p=...".to_vec()));
        assert_eq!(call_count(), 2);
    }

    #[test]
    fn max_cache_age_is_capped() {
        // Asking for 999_999s caps at MAX_CACHE_AGE_SECS (24h).
        install_config(&["example.com"], Some(999_999));
        reset_cache();
        set_mock(&agreeing_dkim());
        set_now(0);

        let _ = block_on(fetch_txt("x._domainkey.example.com", "example.com"));

        // Day after the cap (24h+1s): cache must have expired.
        set_now(types::MAX_CACHE_AGE_SECS + 1);
        let _ = block_on(fetch_txt("x._domainkey.example.com", "example.com"));
        assert_eq!(call_count(), 2, "cache must respect MAX_CACHE_AGE_SECS");
    }

    #[test]
    fn rejects_name_outside_registered_domain() {
        install_config(&["gmail.com"], None);
        reset_cache();
        // Allowlisted registered_domain, but `name` doesn't sit inside
        // it — should fail closed without an outcall.
        let r = block_on(fetch_txt("selector._domainkey.evil.com", "gmail.com"));
        match r {
            Err(DohError::NameOutsideRegisteredDomain {
                name,
                registered_domain,
            }) => {
                assert_eq!(name, "selector._domainkey.evil.com");
                assert_eq!(registered_domain, "gmail.com");
            }
            other => panic!("expected NameOutsideRegisteredDomain, got {other:?}"),
        }
        assert_eq!(call_count(), 0, "no outcall should have been attempted");
    }

    #[test]
    fn label_anchored_suffix_blocks_evilexample_com() {
        install_config(&["example.com"], None);
        reset_cache();
        let r = block_on(fetch_txt("evilexample.com", "example.com"));
        assert!(matches!(
            r,
            Err(DohError::NameOutsideRegisteredDomain { .. })
        ));
    }

    #[test]
    fn name_within_domain_helper_cases() {
        assert!(name_within_domain("gmail.com", "gmail.com"));
        assert!(name_within_domain("Gmail.COM", "gmail.com"));
        assert!(name_within_domain(
            "selector._domainkey.gmail.com",
            "gmail.com"
        ));
        assert!(name_within_domain("a.b.c.gmail.com.", "gmail.com"));
        assert!(!name_within_domain("evilgmail.com", "gmail.com"));
        assert!(!name_within_domain("gmail.com.evil.com", "gmail.com"));
        assert!(!name_within_domain("", "gmail.com"));
    }

    #[test]
    fn three_providers_no_answer_returns_no_answer() {
        // Three providers authoritatively report "no record" (the wire
        // shape that lands here is the transform's 204), two are down.
        // The quorum on absence is itself an answer.
        install_config(&["example.com"], Some(3600));
        reset_cache();
        set_mock(&[
            (PROVIDERS[0].url, no_answer()),
            (PROVIDERS[1].url, no_answer()),
            (PROVIDERS[2].url, no_answer()),
            (PROVIDERS[3].url, fail("network down")),
            (PROVIDERS[4].url, fail("timeout")),
        ]);
        let r = block_on(fetch_txt("_dmarc.example.com", "example.com"));
        assert_eq!(r, Err(DohError::NoAnswer));
    }

    #[test]
    fn no_answer_result_is_not_cached() {
        // Surfacing NoAnswer must NOT poison the cache: a follow-up
        // call with a different mock (record was just published) re-
        // fetches and now gets the real bytes.
        install_config(&["example.com"], Some(3600));
        reset_cache();
        set_mock(&[
            (PROVIDERS[0].url, no_answer()),
            (PROVIDERS[1].url, no_answer()),
            (PROVIDERS[2].url, no_answer()),
            (PROVIDERS[3].url, no_answer()),
            (PROVIDERS[4].url, no_answer()),
        ]);
        set_now(1_000);
        let r1 = block_on(fetch_txt("_dmarc.example.com", "example.com"));
        assert_eq!(r1, Err(DohError::NoAnswer));

        set_mock(&agreeing_dkim());
        set_now(1_001);
        let r2 = block_on(fetch_txt("_dmarc.example.com", "example.com"));
        assert_eq!(r2, Ok(b"v=DKIM1; k=rsa; p=...".to_vec()));
        assert_eq!(call_count(), 2);
    }

    #[test]
    fn rejects_invalid_qname() {
        install_config(&["example.com"], None);
        reset_cache();
        // 64-byte label busts the per-label cap; should be rejected
        // before any outcall is attempted (and before any cache state
        // is mutated).
        let oversize = "a".repeat(64);
        let name = format!("{oversize}.example.com");
        let r = block_on(fetch_txt(&name, "example.com"));
        assert!(matches!(r, Err(DohError::InvalidName(_))));
        assert_eq!(call_count(), 0);
    }

    // ---- Pure transform/outcall classifier coverage ----

    /// Build a wire-format DoH response carrying one TXT answer.
    /// Header: ID=0, flags=0x8180 (QR + RD + RA, RCODE=0), QDCOUNT=1,
    /// ANCOUNT=1. Mirrors the helper in `parser.rs::tests`.
    fn fake_txt_response(rdata: &[u8]) -> Vec<u8> {
        use parser::{CLASS_IN, TYPE_TXT};
        let mut out = Vec::new();
        out.extend_from_slice(&[0, 0, 0x81, 0x80, 0, 1, 0, 1, 0, 0, 0, 0]);
        out.extend_from_slice(b"\x07example\x03com\x00\x00\x10\x00\x01");
        out.extend_from_slice(&[0xC0, 0x0C]);
        out.extend_from_slice(&TYPE_TXT.to_be_bytes());
        out.extend_from_slice(&CLASS_IN.to_be_bytes());
        out.extend_from_slice(&300u32.to_be_bytes());
        out.extend_from_slice(&(rdata.len() as u16).to_be_bytes());
        out.extend_from_slice(rdata);
        out
    }

    #[test]
    fn classify_upstream_200_with_valid_txt_yields_ok_and_txt_bytes() {
        let body = fake_txt_response(b"\x05hello");
        let (status, out) = classify_upstream(HTTP_STATUS_OK, &body);
        assert_eq!(status, HTTP_STATUS_OK);
        assert_eq!(out, b"hello");
    }

    #[test]
    fn classify_upstream_200_with_empty_answer_yields_no_answer_sentinel() {
        // ANCOUNT=0 → parser emits ParseError::NoAnswer.
        let mut body = Vec::new();
        body.extend_from_slice(&[0, 0, 0x81, 0x80, 0, 1, 0, 0, 0, 0, 0, 0]);
        body.extend_from_slice(b"\x07example\x03com\x00\x00\x10\x00\x01");
        let (status, out) = classify_upstream(HTTP_STATUS_OK, &body);
        assert_eq!(status, HTTP_STATUS_TRANSFORM_NO_ANSWER);
        assert!(out.is_empty());
    }

    #[test]
    fn classify_upstream_200_with_garbage_yields_parse_failed_sentinel() {
        let (status, out) = classify_upstream(HTTP_STATUS_OK, b"not a DNS message");
        assert_eq!(status, HTTP_STATUS_TRANSFORM_PARSE_FAILED);
        assert!(out.is_empty());
    }

    #[test]
    fn classify_upstream_spoofed_204_is_remapped_to_upstream_error() {
        // The security-relevant case: a misbehaving or adversarial
        // provider that returns 204 directly must NOT be treated as
        // the transform's NoAnswer sentinel — that would let a single
        // bad actor (or three colluding ones, against the quorum)
        // counterfeit "this record doesn't exist" out of nothing. The
        // classifier collapses every upstream non-200 onto the
        // upstream-error sentinel instead, with an empty body.
        let (status, out) = classify_upstream(HTTP_STATUS_TRANSFORM_NO_ANSWER, b"counterfeit body");
        assert_eq!(status, HTTP_STATUS_TRANSFORM_UPSTREAM_ERROR);
        assert!(out.is_empty());
    }

    #[test]
    fn classify_upstream_non_200_is_remapped_to_upstream_error() {
        for upstream in [301u16, 400, 403, 500, 502, 503] {
            let (status, out) = classify_upstream(upstream, b"whatever body");
            assert_eq!(
                status, HTTP_STATUS_TRANSFORM_UPSTREAM_ERROR,
                "upstream {upstream} should collapse to upstream-error sentinel"
            );
            assert!(out.is_empty());
        }
    }

    #[test]
    fn status_to_outcome_maps_each_transform_sentinel() {
        match status_to_outcome(HTTP_STATUS_OK, b"some-txt".to_vec()) {
            Outcome::Txt(b) => assert_eq!(b, b"some-txt"),
            other => panic!("expected Txt, got {other:?}"),
        }
        assert_eq!(
            status_to_outcome(HTTP_STATUS_TRANSFORM_NO_ANSWER, vec![]),
            Outcome::NoAnswer
        );
        match status_to_outcome(HTTP_STATUS_TRANSFORM_PARSE_FAILED, vec![]) {
            Outcome::FetchError(msg) => assert!(msg.contains("422")),
            other => panic!("expected FetchError, got {other:?}"),
        }
        match status_to_outcome(HTTP_STATUS_TRANSFORM_UPSTREAM_ERROR, vec![]) {
            Outcome::FetchError(msg) => assert!(msg.contains("502")),
            other => panic!("expected FetchError, got {other:?}"),
        }
    }
}
