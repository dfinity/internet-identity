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
//! - Quorum of providers authoritatively reported no record at this
//!   name (NXDOMAIN / empty answer section) → [`DohError::NoAnswer`].
//!   Kept distinct from `QuorumFailed` / `AllProvidersFailed` so a
//!   caller (e.g. DMARC, which treats a missing `_dmarc` TXT as "no
//!   policy published" and falls back to strict alignment) can react
//!   to "this record genuinely doesn't exist" without also reacting
//!   to "we couldn't reach a verdict".
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
/// parsed RDATA, `NoAnswer` is an authoritative "no record exists"
/// (NXDOMAIN or empty answer section), and the two error variants
/// record why a provider didn't contribute (logged but not surfaced
/// beyond aggregate counts).
#[derive(Clone, Debug, Eq, PartialEq)]
pub(super) enum Outcome {
    Txt(Vec<u8>),
    NoAnswer,
    FetchError(String),
    ParseError(String),
}

/// Count agreement and apply the threshold. Pure function so it's
/// trivially testable without a fetcher.
///
/// NoAnswer is itself an authoritative answer ("this record doesn't
/// exist") and contributes its own bucket — three providers all
/// reporting NXDOMAIN is a quorum on "no record", reported as
/// `DohError::NoAnswer`. We pick the largest bucket overall (TXT
/// buckets vs. the NoAnswer bucket) so a 3-of-5 NoAnswer beats a
/// 2-of-5 TXT, and any larger TXT bucket beats a smaller NoAnswer.
pub(super) fn decide_quorum(outcomes: &[Outcome]) -> Result<Vec<u8>, DohError> {
    let total = outcomes.len();

    let no_answer_count = outcomes
        .iter()
        .filter(|o| matches!(o, Outcome::NoAnswer))
        .count();

    let txt_responses: Vec<&Vec<u8>> = outcomes
        .iter()
        .filter_map(|o| match o {
            Outcome::Txt(b) => Some(b),
            _ => None,
        })
        .collect();

    if txt_responses.is_empty() && no_answer_count == 0 {
        return Err(DohError::AllProvidersFailed);
    }

    // Bucket TXT responses by exact byte equality.
    let mut buckets: Vec<(&[u8], usize)> = Vec::new();
    for txt in &txt_responses {
        let bytes = txt.as_slice();
        if let Some((_, count)) = buckets.iter_mut().find(|(b, _)| *b == bytes) {
            *count += 1;
        } else {
            buckets.push((bytes, 1));
        }
    }

    let (txt_winner_bytes, txt_winner_count) = buckets
        .iter()
        .max_by_key(|(_, c)| *c)
        .copied()
        .map_or((&[][..], 0), |(b, c)| (b, c));

    // The "winner" is whichever bucket is larger: a TXT bucket or the
    // NoAnswer bucket. Ties go to TXT — a TXT and a NoAnswer at the
    // same count means the real answer is the TXT (the NoAnswer
    // providers are stale/wrong) more often than the reverse.
    if txt_winner_count >= QUORUM_THRESHOLD && txt_winner_count >= no_answer_count {
        return Ok(txt_winner_bytes.to_vec());
    }
    if no_answer_count >= QUORUM_THRESHOLD {
        return Err(DohError::NoAnswer);
    }
    Err(DohError::QuorumFailed {
        agreeing: txt_winner_count.max(no_answer_count),
        total,
    })
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
    fn no_answer() -> Outcome {
        Outcome::NoAnswer
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

    #[test]
    fn three_no_answers_returns_no_answer() {
        // Three providers authoritatively report "no record" (NXDOMAIN
        // / empty answer section), two are down. The quorum on absence
        // is itself an answer — callers like DMARC need to distinguish
        // this from a transient outage so they can apply the right
        // fallback (strict alignment, in DMARC's case).
        let outcomes = vec![
            no_answer(),
            no_answer(),
            no_answer(),
            fetch_err("network down"),
            fetch_err("timeout"),
        ];
        assert_eq!(decide_quorum(&outcomes).unwrap_err(), DohError::NoAnswer);
    }

    #[test]
    fn all_five_no_answer_returns_no_answer() {
        let outcomes = vec![
            no_answer(),
            no_answer(),
            no_answer(),
            no_answer(),
            no_answer(),
        ];
        assert_eq!(decide_quorum(&outcomes).unwrap_err(), DohError::NoAnswer);
    }

    #[test]
    fn two_no_answers_misses_quorum() {
        // Two NoAnswer + three errors — the NoAnswer bucket is short of
        // the threshold; we refuse to fall back to "majority of
        // responding providers".
        let outcomes = vec![
            no_answer(),
            no_answer(),
            fetch_err("a"),
            fetch_err("b"),
            fetch_err("c"),
        ];
        assert_eq!(
            decide_quorum(&outcomes).unwrap_err(),
            DohError::QuorumFailed {
                agreeing: 2,
                total: 5,
            }
        );
    }

    #[test]
    fn txt_majority_beats_no_answer_minority() {
        // Three TXT agree, two NoAnswer — quorum hits on TXT. The
        // NoAnswer providers are stale; we report the real record.
        let outcomes = vec![
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            no_answer(),
            no_answer(),
        ];
        assert_eq!(decide_quorum(&outcomes).unwrap(), b"v=DKIM1");
    }

    #[test]
    fn no_answer_majority_beats_txt_minority() {
        // Three NoAnswer, two TXT — the record really doesn't exist
        // and two providers are returning forged bytes.
        let outcomes = vec![
            no_answer(),
            no_answer(),
            no_answer(),
            txt(b"forged"),
            txt(b"forged"),
        ];
        assert_eq!(decide_quorum(&outcomes).unwrap_err(), DohError::NoAnswer);
    }

    #[test]
    fn split_between_txt_and_no_answer_misses_quorum() {
        // Two TXT agreeing + two NoAnswer + one error. Neither bucket
        // hits the threshold of 3, so we refuse to decide either way.
        let outcomes = vec![
            txt(b"v=DKIM1"),
            txt(b"v=DKIM1"),
            no_answer(),
            no_answer(),
            fetch_err("a"),
        ];
        assert_eq!(
            decide_quorum(&outcomes).unwrap_err(),
            DohError::QuorumFailed {
                agreeing: 2,
                total: 5,
            }
        );
    }
}
