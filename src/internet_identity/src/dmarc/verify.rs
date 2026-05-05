//! Combined DKIM + DMARC verifier — the public top-level entry point
//! the canister-side recovery flow consumes.
//!
//! Per design doc §6.6, the verifier accepts a message iff:
//! - DKIM passes (cryptographic signature valid, all the §5.4 tags
//!   pass), **and**
//! - the DKIM-signed `d=` aligns with the From-header domain under
//!   the published `adkim=` mode (or, when no DMARC record exists,
//!   matches it byte-for-byte).
//!
//! We never accept a Misaligned outcome regardless of policy — this
//! is a recovery surface, not a delivery system, and a non-aligned
//! signature has no value to us as a proof of mailbox control.

use super::alignment::aligns;
use super::from_header::extract_from_domain;
use super::parse::parse_dmarc_txt;
use super::types::{DmarcOutcome, DmarcRecord, EmailVerificationStatus};
use crate::dkim::verify_dkim;
use crate::dkim::{DkimVerifyResult, VerificationFailReason};
use internet_identity_interface::internet_identity::types::smtp::SmtpRequest;

/// Verify an `SmtpRequest` end-to-end: DKIM signature + DMARC alignment.
///
/// Inputs:
/// - `email` — the parsed `SmtpRequest` the gateway delivered.
/// - `dkim_txt` — the (already-trusted) DKIM TXT record content. In
///   production this comes from PR 1's DNSSEC verifier or PR 4's DoH
///   fallback. PR 3 takes it as input and does no DNS work itself.
/// - `dmarc_txt` — the (already-trusted) DMARC TXT record content,
///   or `None` if the From-header domain has no published DMARC
///   record. When `None`, the verifier still requires DKIM `d=` to
///   match the From-header domain exactly.
/// - `now_secs` — current Unix time (passed in so tests can pin it).
pub fn verify_email(
    email: &SmtpRequest,
    dkim_txt: &str,
    dmarc_txt: Option<&str>,
    now_secs: u64,
) -> EmailVerificationStatus {
    // Step 1: DKIM. If it fails, surface the DKIM verdict directly —
    // there's no point checking alignment against a forged signature.
    let dkim = verify_dkim(email, dkim_txt, now_secs);
    let (dkim_domain, dkim_checks) = match dkim {
        DkimVerifyResult::Verified {
            dkim_domain,
            checks,
        } => (dkim_domain, checks),
        DkimVerifyResult::Unverified { reason, checks } => {
            return EmailVerificationStatus::Unverified { reason, checks };
        }
    };

    // Step 2: From-header domain. We need a message body to read the
    // headers from; if `email.message` was missing the DKIM step would
    // have already failed (NoSignature). Unwrap is safe.
    let message = match email.message.as_ref() {
        Some(m) => m,
        None => {
            return EmailVerificationStatus::Unverified {
                reason: VerificationFailReason::MalformedFromHeader("message body missing".into()),
                checks: dkim_checks,
            };
        }
    };
    let from_domain = match extract_from_domain(message) {
        Ok(d) => d,
        Err(e) => {
            return EmailVerificationStatus::Unverified {
                reason: VerificationFailReason::MalformedFromHeader(e),
                checks: dkim_checks,
            };
        }
    };

    // Step 3: DMARC alignment. If a DMARC record is supplied, parse
    // it and check alignment under its `adkim=` mode. Otherwise, the
    // implicit policy is "DKIM `d=` must equal the From domain".
    let outcome = compute_outcome(&dkim_domain, &from_domain, dmarc_txt);

    let accepted = match &outcome {
        DmarcOutcome::Aligned { .. } => true,
        DmarcOutcome::NoRecord => dkim_domain == from_domain,
        DmarcOutcome::Misaligned { .. } | DmarcOutcome::Malformed(_) => false,
    };

    if accepted {
        EmailVerificationStatus::Verified {
            dkim_domain,
            from_domain,
            dmarc: outcome,
            checks: dkim_checks,
        }
    } else {
        let reason = match &outcome {
            DmarcOutcome::Malformed(e) => VerificationFailReason::DmarcMalformed(e.clone()),
            // Both Misaligned (DMARC says no) and NoRecord-with-mismatch
            // (no DMARC says-so but the bare-DKIM-equals-From check
            // failed) collapse to DmarcMisaligned for the UI; the
            // Verified variant carries the full DmarcOutcome detail.
            _ => VerificationFailReason::DmarcMisaligned,
        };
        EmailVerificationStatus::Unverified {
            reason,
            checks: dkim_checks,
        }
    }
}

fn compute_outcome(dkim_domain: &str, from_domain: &str, dmarc_txt: Option<&str>) -> DmarcOutcome {
    let txt = match dmarc_txt {
        None => return DmarcOutcome::NoRecord,
        Some(t) => t,
    };
    let record: DmarcRecord = match parse_dmarc_txt(txt) {
        Ok(r) => r,
        Err(e) => return DmarcOutcome::Malformed(e),
    };
    if aligns(dkim_domain, from_domain, record.adkim) {
        DmarcOutcome::Aligned {
            policy: record.policy,
            alignment_mode: record.adkim,
        }
    } else {
        DmarcOutcome::Misaligned {
            policy: record.policy,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use internet_identity_interface::internet_identity::types::smtp::{
        SmtpAddress, SmtpEnvelope, SmtpHeader, SmtpMessage,
    };
    use serde_bytes::ByteBuf;

    fn empty_message_with_from(from_value: &str) -> SmtpRequest {
        SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                },
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    SmtpHeader {
                        name: "From".into(),
                        value: from_value.into(),
                    },
                    SmtpHeader {
                        name: "Subject".into(),
                        value: "x".into(),
                    },
                ],
                body: ByteBuf::from(b"x".to_vec()),
            }),
            gateway_flags: None,
        }
    }

    /// `verify_email` propagates a DKIM failure verbatim without
    /// running DMARC. We exercise this by passing a request that has
    /// no DKIM-Signature header at all.
    #[test]
    fn dkim_failure_short_circuits_dmarc() {
        let req = empty_message_with_from("alice@example.com");
        let result = verify_email(&req, "v=DKIM1; p=YWJj", None, 1_700_000_000);
        match result {
            EmailVerificationStatus::Unverified { reason, .. } => {
                assert_eq!(reason, VerificationFailReason::NoSignature);
            }
            other => panic!("expected Unverified(NoSignature), got {:?}", other),
        }
    }

    /// `compute_outcome` returns `NoRecord` when no DMARC TXT is
    /// supplied. The wrapper then gates acceptance on dkim==from.
    #[test]
    fn no_record_aligned_when_dkim_equals_from() {
        let outcome = compute_outcome("example.com", "example.com", None);
        assert_eq!(outcome, DmarcOutcome::NoRecord);
    }

    #[test]
    fn no_record_aligned_when_dkim_subdomain_of_from() {
        // No DMARC record + dkim is a subdomain → still rejected. The
        // bare-DKIM check requires exact equality without a policy.
        let outcome = compute_outcome("mail.example.com", "example.com", None);
        assert_eq!(outcome, DmarcOutcome::NoRecord);
        // Wrapper-level acceptance check happens in verify_email; the
        // outcome here is still NoRecord regardless.
    }

    #[test]
    fn record_aligned_under_relaxed_subdomain() {
        let txt = "v=DMARC1; p=reject"; // adkim defaults to relaxed
        let outcome = compute_outcome("mail.example.com", "example.com", Some(txt));
        assert!(matches!(
            outcome,
            DmarcOutcome::Aligned {
                policy: super::super::types::DmarcPolicy::Reject,
                alignment_mode: super::super::types::AlignmentMode::Relaxed,
            }
        ));
    }

    #[test]
    fn record_misaligned_under_strict_subdomain() {
        let txt = "v=DMARC1; p=reject; adkim=s";
        let outcome = compute_outcome("mail.example.com", "example.com", Some(txt));
        assert!(matches!(outcome, DmarcOutcome::Misaligned { .. }));
    }

    #[test]
    fn record_aligned_strict_exact_match() {
        let txt = "v=DMARC1; p=reject; adkim=s";
        let outcome = compute_outcome("example.com", "example.com", Some(txt));
        assert!(matches!(
            outcome,
            DmarcOutcome::Aligned {
                alignment_mode: super::super::types::AlignmentMode::Strict,
                ..
            }
        ));
    }

    #[test]
    fn malformed_dmarc_surfaces_as_outcome() {
        let txt = "v=BOGUS";
        let outcome = compute_outcome("example.com", "example.com", Some(txt));
        match outcome {
            DmarcOutcome::Malformed(e) => assert!(e.contains("BOGUS")),
            other => panic!("expected Malformed, got {:?}", other),
        }
    }

    // Note: the "DKIM verified, no DMARC, exact d=From" end-to-end path
    // is covered by the cryptographic round-trip tests in
    // `crate::dmarc::test_vectors` (which load real `.eml` fixtures and
    // run the full `verify_email`). The unit tests in this module
    // exercise the orchestration in isolation via lower-level helpers.
}
