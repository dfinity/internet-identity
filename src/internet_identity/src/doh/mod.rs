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

    // Cache lookup. We must release the borrow before awaiting — the
    // canister is single-threaded but futures across yields can re-
    // enter the same `RefCell`, which would trap.
    let now = now_secs();
    let lookup = DOH_CACHE.with(|c| c.borrow_mut().lookup(name, now));
    let token = match lookup {
        CacheLookup::Hit(bytes) => {
            return Ok(bytes);
        }
        CacheLookup::Wait(fut) => {
            return fut.await;
        }
        CacheLookup::Fetch(token) => {
            token
        }
    };

    // Fan out to every provider in parallel, then decide.
    let outcomes = fetch_all(&query).await;
    let result = decide_quorum(&outcomes);

    // Publish: stores on success, removes the pending entry, wakes any
    // dedup subscribers. The expires-at uses the timestamp we captured
    // before the await so multiple subscribers see consistent freshness.
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
    use super::Outcome;
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

    pub(super) const HTTP_STATUS_OK: u8 = 200;

    /// HTTP status the transform returns when the wire-format DNS
    /// response was a valid "no such record" answer (NXDOMAIN or an
    /// RCODE=0 empty answer section). 204 ("No Content") is a
    /// defensible bucket for "the request succeeded but there's
    /// nothing to return", and [`outcall`] maps it to
    /// [`Outcome::NoAnswer`] so the quorum can count "no record"
    /// agreement separately from transient outages.
    pub(super) const HTTP_STATUS_TRANSFORM_NO_ANSWER: u16 = 204;

    /// HTTP status the transform returns when the wire-format DNS
    /// response can't be parsed down to a TXT RDATA. We deliberately
    /// signal failure via the *status code*, not via a sentinel body
    /// string, because any TXT-bytes body the transform might emit
    /// could collide with a real DKIM/DMARC payload (a sentinel like
    /// `b"!!doh-malformed"` is bytes-equal to a (legal-but-unusual)
    /// TXT record). 422 ("Unprocessable Entity") is a defensible
    /// bucket for "you sent us bytes we can't make sense of"; what
    /// actually matters is the IC-replica consensus on a non-200,
    /// which the existing `if response.status != HTTP_STATUS_OK`
    /// branch in [`outcall`] turns into an `Err`.
    pub(super) const HTTP_STATUS_TRANSFORM_PARSE_FAILED: u16 = 422;

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
                let status_u16: u16 = response.status.0.try_into().unwrap_or(u16::MAX);
                if status_u16 == u16::from(HTTP_STATUS_OK) {
                    Outcome::Txt(response.body)
                } else if status_u16 == HTTP_STATUS_TRANSFORM_NO_ANSWER {
                    Outcome::NoAnswer
                } else {
                    Outcome::FetchError(format!("HTTP {status_u16}"))
                }
            }
            Err((_, err)) => Outcome::FetchError(err),
        }
    }

    /// Transform query for replica consensus.
    ///
    /// DoH wire-format responses contain TTLs that decrement once a
    /// second, so replicas making the same query a few hundred ms
    /// apart can observe different bytes — consensus then fails. The
    /// transform reduces the response to just the parsed TXT bytes,
    /// which depend only on the resolver's cached answer (stable
    /// within a given TTL bucket), so all replicas converge.
    ///
    /// On parse failure we signal the failure out-of-band via a non-
    /// 200 status code (with empty body) rather than encoding it in
    /// the body. An in-band sentinel string would risk colliding with
    /// a legitimate (if unusual) TXT payload and silently turning a
    /// valid record into a "malformed" failure for the quorum. The
    /// "no such record" case (NXDOMAIN / empty answer section) is
    /// reported as a separate status (204) so the quorum step can
    /// count "everyone agrees there is no record" distinctly from
    /// "everyone failed to respond".
    #[allow(clippy::needless_pass_by_value)]
    pub(super) fn transform_doh(response: HttpResponse) -> HttpResponse {
        use super::ParseError;
        use candid::Nat;
        if response.status != HTTP_STATUS_OK {
            // Non-200 upstream responses are passed through untouched
            // (status preserved, body emptied for consensus) —
            // replicas converge on the status code even if their
            // original bodies differed.
            return HttpResponse {
                status: response.status,
                headers: vec![],
                body: vec![],
            };
        }
        match super::parse_txt_response(&response.body) {
            Ok(txt) => HttpResponse {
                status: Nat::from(HTTP_STATUS_OK),
                headers: vec![],
                body: txt,
            },
            Err(ParseError::NoAnswer) => HttpResponse {
                // Out-of-band "no record" signal: see `HTTP_STATUS_TRANSFORM_NO_ANSWER`.
                status: Nat::from(u32::from(HTTP_STATUS_TRANSFORM_NO_ANSWER)),
                headers: vec![],
                body: vec![],
            },
            Err(_) => HttpResponse {
                // Out-of-band parse-failure signal: see `HTTP_STATUS_TRANSFORM_PARSE_FAILED`.
                status: Nat::from(u32::from(HTTP_STATUS_TRANSFORM_PARSE_FAILED)),
                headers: vec![],
                body: vec![],
            },
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
}
