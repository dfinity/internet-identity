//! Parallel multi-provider DoH fetch with k-of-n quorum.
//!
//! Issues a wire-format DoH query to each provider in [`PROVIDERS`]
//! and returns the TXT bytes that at least [`QUORUM_THRESHOLD`] of
//! them agree on. We don't rely on any single resolver being honest
//! or available: as long as a strict majority of the providers we
//! picked return the same TXT, the verifier accepts the response.
//!
//! Failure modes:
//! - Every provider down → [`DohError::AllProvidersFailed`].
//! - Providers up but the largest agreeing bucket is below the
//!   threshold → [`DohError::QuorumFailed`].
//! - Mix of failures + agreements that still hits the threshold →
//!   accept (a couple of providers being down or returning forged
//!   bytes doesn't punish the user as long as enough of the rest
//!   agree).

use super::parser::{build_txt_query, parse_txt_response};
use super::types::{DohError, DohProvider, PROVIDERS, QUORUM_THRESHOLD};

/// Trait for the per-provider HTTP outcall. Production swaps in an
/// `ic_cdk` HTTP-outcall implementation; tests use a deterministic
/// in-memory mock that returns canned bytes.
///
/// `async fn` in traits requires `async_trait`-style boxing. We avoid
/// the dep by using `impl Future<Output = ...>` and a manual `dyn`
/// bound through a function-pointer-shaped helper. Instead the trait
/// uses a simpler synchronous `&self` API and the production wrapper
/// wires it into the IC outcall machinery in [`super::mod`].
pub trait DohFetcher {
    /// Issue a wire-format DoH query (POST `application/dns-message`
    /// with `query_bytes` as body) to `provider.url`. Returns the raw
    /// response body bytes on success.
    ///
    /// This trait is the **synchronous test seam**: tests build a
    /// `MockFetcher` and call [`run_quorum`] which iterates `PROVIDERS`
    /// sequentially. The production async path in [`super::fetch_txt`]
    /// does NOT go through this trait — it constructs its own per-
    /// provider futures and fans them out via `futures::future::join_all`
    /// for genuine parallelism. The two paths share only the bucketing
    /// logic in [`decide_quorum`].
    ///
    /// Tests pass a `&MockDohFetcher` that has a `HashMap<DohProvider,
    /// Result<Vec<u8>, ()>>` and just returns the canned answer.
    fn fetch_blocking(&self, provider: &DohProvider, query_bytes: &[u8])
        -> Result<Vec<u8>, String>;
}

/// Run the per-provider fetches and pick the result that hits the
/// quorum threshold.
///
/// `fetcher` is parameterised so tests can inject canned responses;
/// production passes a real outcall implementation.
///
/// Returns `Ok(txt_bytes)` iff at least [`QUORUM_THRESHOLD`] providers
/// returned the same TXT bytes. The DNS message envelope (header,
/// question section, TTLs) is intentionally *not* compared — only the
/// TXT RDATA is.
pub fn run_quorum<F: DohFetcher>(name: &str, fetcher: &F) -> Result<Vec<u8>, DohError> {
    let query = build_txt_query(name).map_err(|e| DohError::InvalidName(format!("{e:?}")))?;

    // Per-provider outcomes. Order matches PROVIDERS so logs are
    // attributable, but the quorum count doesn't care about order.
    let mut outcomes: Vec<Outcome> = Vec::with_capacity(PROVIDERS.len());
    for provider in PROVIDERS {
        outcomes.push(match fetcher.fetch_blocking(provider, &query) {
            Ok(bytes) => match parse_txt_response(&bytes) {
                Ok(txt) => Outcome::Txt(txt),
                Err(e) => Outcome::ParseError(format!("{:?}", e)),
            },
            Err(e) => Outcome::FetchError(e),
        });
    }
    decide_quorum(&outcomes)
}

/// One provider's contribution to the quorum count. The async path in
/// [`super::fetch_txt`] constructs these from outcall results
/// directly; the sync [`run_quorum`] above constructs them while
/// iterating its fetcher.
#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) enum Outcome {
    Txt(Vec<u8>),
    FetchError(String),
    ParseError(String),
}

/// Count agreement and apply the threshold. Pure function so it's
/// trivially testable without a fetcher.
pub(super) fn decide_quorum(outcomes: &[Outcome]) -> Result<Vec<u8>, DohError> {
    let succeeded: Vec<&Vec<u8>> = outcomes
        .iter()
        .filter_map(|o| match o {
            Outcome::Txt(b) => Some(b),
            _ => None,
        })
        .collect();
    let total = outcomes.len();

    if succeeded.is_empty() {
        return Err(DohError::AllProvidersFailed);
    }

    // Bucket by exact TXT byte equality. The largest bucket is the
    // candidate; if it's at the threshold, we accept.
    let mut buckets: Vec<(&[u8], usize)> = Vec::new();
    for txt in &succeeded {
        let bytes = txt.as_slice();
        if let Some((_, count)) = buckets.iter_mut().find(|(b, _)| *b == bytes) {
            *count += 1;
        } else {
            buckets.push((bytes, 1));
        }
    }
    let (winner_bytes, winner_count) = buckets.iter().max_by_key(|(_, c)| *c).copied().unwrap();

    if winner_count >= QUORUM_THRESHOLD {
        Ok(winner_bytes.to_vec())
    } else {
        Err(DohError::QuorumFailed {
            agreeing: winner_count,
            total,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::RefCell;
    use std::collections::HashMap;

    /// In-memory fetcher for tests. Maps `provider.url` →
    /// `Result<Vec<u8>, String>`. Records each call so tests can
    /// assert "fired exactly 3 times".
    pub(crate) struct MockFetcher {
        responses: HashMap<String, Result<Vec<u8>, String>>,
        pub(crate) calls: RefCell<usize>,
    }

    impl MockFetcher {
        fn new(responses: &[(&str, Result<Vec<u8>, String>)]) -> Self {
            let mut m = HashMap::new();
            for (url, r) in responses {
                m.insert((*url).to_string(), r.clone());
            }
            Self {
                responses: m,
                calls: RefCell::new(0),
            }
        }
    }

    impl DohFetcher for MockFetcher {
        fn fetch_blocking(&self, provider: &DohProvider, _query: &[u8]) -> Result<Vec<u8>, String> {
            *self.calls.borrow_mut() += 1;
            self.responses
                .get(provider.url)
                .cloned()
                .unwrap_or(Err("no canned response for provider".into()))
        }
    }

    /// Build a fake DoH response carrying the TXT RDATA `txt`. The
    /// message envelope mirrors the test in `parser.rs`; we re-emit
    /// here so each test can vary the answer per provider.
    fn fake_response(txt: &[u8]) -> Vec<u8> {
        use super::super::parser::{CLASS_IN, TYPE_TXT};
        let mut out = Vec::new();
        // Header: ID=0, flags=0x8180, QDCOUNT=1, ANCOUNT=1
        out.extend_from_slice(&[0, 0, 0x81, 0x80, 0, 1, 0, 1, 0, 0, 0, 0]);
        // Question: example.com TXT IN
        out.extend_from_slice(b"\x07example\x03com\x00\x00\x10\x00\x01");
        // Answer name (compression pointer)
        out.extend_from_slice(&[0xC0, 0x0C]);
        out.extend_from_slice(&TYPE_TXT.to_be_bytes());
        out.extend_from_slice(&CLASS_IN.to_be_bytes());
        out.extend_from_slice(&300u32.to_be_bytes());
        // RDLENGTH and RDATA
        let mut rdata = vec![txt.len() as u8];
        rdata.extend_from_slice(txt);
        out.extend_from_slice(&(rdata.len() as u16).to_be_bytes());
        out.extend_from_slice(&rdata);
        out
    }

    /// Build a `MockFetcher` from a per-index outcome — index `i`
    /// targets `PROVIDERS[i].url`. Keeps the tests below readable
    /// without restating the URL list.
    fn mock_from(outcomes: Vec<Result<Vec<u8>, String>>) -> MockFetcher {
        assert_eq!(
            outcomes.len(),
            PROVIDERS.len(),
            "test setup must cover every provider"
        );
        let pairs: Vec<(&str, Result<Vec<u8>, String>)> = PROVIDERS
            .iter()
            .zip(outcomes)
            .map(|(p, r)| (p.url, r))
            .collect();
        let pairs_ref: Vec<(&str, Result<Vec<u8>, String>)> = pairs;
        // MockFetcher::new takes &[(&str, Result<...>)]; we already
        // built it.
        MockFetcher::new(&pairs_ref)
    }

    fn ok_dkim() -> Result<Vec<u8>, String> {
        Ok(fake_response(b"v=DKIM1"))
    }
    fn ok_other(s: &[u8]) -> Result<Vec<u8>, String> {
        Ok(fake_response(s))
    }
    fn fail(s: &str) -> Result<Vec<u8>, String> {
        Err(s.into())
    }

    #[test]
    fn all_five_agree_passes() {
        let fetcher = mock_from(vec![ok_dkim(), ok_dkim(), ok_dkim(), ok_dkim(), ok_dkim()]);
        let txt = run_quorum("example.com", &fetcher).unwrap();
        assert_eq!(txt, b"v=DKIM1");
        assert_eq!(*fetcher.calls.borrow(), 5);
    }

    #[test]
    fn three_of_five_agree_two_disagree_passes() {
        // Threshold is 3. Three agree on "v=DKIM1", two return forged
        // bytes — honest majority wins.
        let fetcher = mock_from(vec![
            ok_dkim(),
            ok_dkim(),
            ok_dkim(),
            ok_other(b"forged-A"),
            ok_other(b"forged-B"),
        ]);
        let txt = run_quorum("example.com", &fetcher).unwrap();
        assert_eq!(txt, b"v=DKIM1");
    }

    #[test]
    fn three_agree_two_fail_passes() {
        let fetcher = mock_from(vec![
            ok_dkim(),
            ok_dkim(),
            ok_dkim(),
            fail("network down"),
            fail("timeout"),
        ]);
        let txt = run_quorum("example.com", &fetcher).unwrap();
        assert_eq!(txt, b"v=DKIM1");
    }

    #[test]
    fn two_agreeing_misses_quorum() {
        // 2-of-5 is short of the 3-of-5 threshold — even with a clear
        // plurality, we refuse rather than fall back to majority-of-
        // -successful-responses.
        let fetcher = mock_from(vec![ok_dkim(), ok_dkim(), fail("a"), fail("b"), fail("c")]);
        let err = run_quorum("example.com", &fetcher).unwrap_err();
        assert_eq!(
            err,
            DohError::QuorumFailed {
                agreeing: 2,
                total: 5
            }
        );
    }

    #[test]
    fn split_buckets_miss_quorum() {
        // Five providers split across three answers (2/2/1) — biggest
        // bucket is 2, short of the 3-of-5 threshold.
        let fetcher = mock_from(vec![
            ok_other(b"answer-A"),
            ok_other(b"answer-A"),
            ok_other(b"answer-B"),
            ok_other(b"answer-B"),
            ok_other(b"answer-C"),
        ]);
        let err = run_quorum("example.com", &fetcher).unwrap_err();
        assert_eq!(
            err,
            DohError::QuorumFailed {
                agreeing: 2,
                total: 5
            }
        );
    }

    #[test]
    fn all_fail_returns_all_providers_failed() {
        let fetcher = mock_from(vec![fail("a"), fail("b"), fail("c"), fail("d"), fail("e")]);
        let err = run_quorum("example.com", &fetcher).unwrap_err();
        assert_eq!(err, DohError::AllProvidersFailed);
    }

    #[test]
    fn two_malformed_three_valid_passes() {
        // Malformed responses are counted as parse failures; the three
        // valid agreeing responses still hit the threshold.
        let fetcher = mock_from(vec![
            ok_dkim(),
            ok_dkim(),
            ok_dkim(),
            Ok(b"not a DNS message".to_vec()),
            Ok(b"also garbage".to_vec()),
        ]);
        let txt = run_quorum("example.com", &fetcher).unwrap();
        assert_eq!(txt, b"v=DKIM1");
    }

    #[test]
    fn all_malformed_returns_all_providers_failed() {
        let fetcher = mock_from(vec![
            Ok(b"g1".to_vec()),
            Ok(b"g2".to_vec()),
            Ok(b"g3".to_vec()),
            Ok(b"g4".to_vec()),
            Ok(b"g5".to_vec()),
        ]);
        let err = run_quorum("example.com", &fetcher).unwrap_err();
        // None parsed → no successes → AllProvidersFailed.
        assert_eq!(err, DohError::AllProvidersFailed);
    }
}
