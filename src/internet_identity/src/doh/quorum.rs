//! Quorum decision over per-provider outcomes.
//!
//! The async fan-out lives in [`super::fetch_txt`]: it issues one
//! outcall per provider in [`super::types::PROVIDERS`], parses each
//! response into an [`Outcome`], and hands the resulting slice to
//! [`decide_quorum`]. Keeping the decision step a pure function makes
//! the threshold logic trivially testable without faking outcalls.
//!
//! Failure modes:
//! - Every provider down → [`DohError::AllProvidersFailed`].
//! - Providers up but the largest agreeing bucket is below the
//!   threshold → [`DohError::QuorumFailed`].
//! - Mix of failures + agreements that still hits the threshold →
//!   accept (a couple of providers being down or returning forged
//!   bytes doesn't punish the user as long as enough of the rest
//!   agree).
//!
//! End-to-end coverage of the outcall loop (provider fan-out, the
//! transform function, real `http_request_with_closure` plumbing)
//! lives in the email-recovery integration tests, which mock outcall
//! replies at the PocketIC boundary rather than abstracting the
//! fetcher.

use super::types::{DohError, QUORUM_THRESHOLD};

/// One provider's contribution to the quorum count. Constructed by
/// [`super::fetch_txt`] from outcall results — `Txt` carries the
/// parsed RDATA, the two error variants record why a provider didn't
/// contribute (logged but not surfaced beyond aggregate counts).
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

    fn txt(s: &[u8]) -> Outcome {
        Outcome::Txt(s.to_vec())
    }
    fn fetch_err(s: &str) -> Outcome {
        Outcome::FetchError(s.into())
    }
    fn parse_err(s: &str) -> Outcome {
        Outcome::ParseError(s.into())
    }

    #[test]
    fn all_five_agree_passes() {
        let outcomes = vec![
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
        ];
        assert_eq!(decide_quorum(&outcomes).unwrap(), b"v=DKIM1");
    }

    #[test]
    fn three_of_five_agree_two_disagree_passes() {
        // Threshold is 3. Three agree on "v=DKIM1", two return forged
        // bytes — honest majority wins.
        let outcomes = vec![
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            txt(b"forged-A"),
            txt(b"forged-B"),
        ];
        assert_eq!(decide_quorum(&outcomes).unwrap(), b"v=DKIM1");
    }

    #[test]
    fn three_agree_two_fail_passes() {
        let outcomes = vec![
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            fetch_err("network down"),
            fetch_err("timeout"),
        ];
        assert_eq!(decide_quorum(&outcomes).unwrap(), b"v=DKIM1");
    }

    #[test]
    fn two_agreeing_misses_quorum() {
        // 2-of-5 is short of the 3-of-5 threshold — even with a clear
        // plurality, we refuse rather than fall back to majority-of-
        // -successful-responses.
        let outcomes = vec![
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            fetch_err("a"),
            fetch_err("b"),
            fetch_err("c"),
        ];
        assert_eq!(
            decide_quorum(&outcomes).unwrap_err(),
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
        let outcomes = vec![
            txt(b"answer-A"),
            txt(b"answer-A"),
            txt(b"answer-B"),
            txt(b"answer-B"),
            txt(b"answer-C"),
        ];
        assert_eq!(
            decide_quorum(&outcomes).unwrap_err(),
            DohError::QuorumFailed {
                agreeing: 2,
                total: 5
            }
        );
    }

    #[test]
    fn all_fail_returns_all_providers_failed() {
        let outcomes = vec![
            fetch_err("a"),
            fetch_err("b"),
            fetch_err("c"),
            fetch_err("d"),
            fetch_err("e"),
        ];
        assert_eq!(
            decide_quorum(&outcomes).unwrap_err(),
            DohError::AllProvidersFailed
        );
    }

    #[test]
    fn two_malformed_three_valid_passes() {
        // Malformed responses are counted as parse failures; the three
        // valid agreeing responses still hit the threshold.
        let outcomes = vec![
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            parse_err("not a DNS message"),
            parse_err("also garbage"),
        ];
        assert_eq!(decide_quorum(&outcomes).unwrap(), b"v=DKIM1");
    }

    #[test]
    fn all_malformed_returns_all_providers_failed() {
        let outcomes = vec![
            parse_err("g1"),
            parse_err("g2"),
            parse_err("g3"),
            parse_err("g4"),
            parse_err("g5"),
        ];
        // None parsed → no successes → AllProvidersFailed.
        assert_eq!(
            decide_quorum(&outcomes).unwrap_err(),
            DohError::AllProvidersFailed
        );
    }
}
