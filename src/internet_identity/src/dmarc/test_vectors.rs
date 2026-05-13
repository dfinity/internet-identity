//! End-to-end tests for the combined DKIM + DMARC verifier.
//!
//! Reuses the synthetic `.eml` fixtures committed for PR 2 (DKIM
//! verifier) — they all sign `d=test.example.com` with `From:
//! alice@test.example.com`, so we get exact-match alignment regardless
//! of the published DMARC mode. The corresponding DKIM TXT record is
//! also reused; we synthesise the DMARC TXT inline.

use super::types::{AlignmentMode, DmarcOutcome, DmarcPolicy, EmailVerificationStatus};
use super::verify::verify_email;
use crate::dkim::VerificationFailReason;
use internet_identity_interface::internet_identity::types::smtp::{
    SmtpAddress, SmtpEnvelope, SmtpHeader, SmtpMessage, SmtpRequest,
};
use serde_bytes::ByteBuf;

const SYNTH_RSA_RELAXED_RELAXED: &[u8] =
    include_bytes!("../../../../test_vectors/dkim/synth-rsa-relaxed-relaxed.eml");
const SYNTH_RSA_TXT: &str =
    include_str!("../../../../test_vectors/dkim/synth-rsa-test1._domainkey.test.example.com.txt");

/// `now_secs` pinned to the synth fixtures' signing time so the
/// `t=`/`x=` checks in the DKIM layer pass even after the captured
/// timestamps drift past real wall-clock time.
fn frozen_now() -> u64 {
    1_777_972_289
}

/// Parse the synth RSA `.eml` into the gateway-shaped `SmtpRequest`,
/// reusing the loader we wrote for PR 2's tests. We can't `pub use`
/// it from `crate::dkim::test_vectors` (it's in `#[cfg(test)]`), so
/// the implementation is duplicated here. Both copies parse the same
/// way; the duplication is contained to test code.
fn parse_eml(raw: &[u8]) -> SmtpRequest {
    let mut header_end = 0;
    while header_end + 4 <= raw.len() {
        if &raw[header_end..header_end + 4] == b"\r\n\r\n" {
            break;
        }
        header_end += 1;
    }
    if header_end + 4 > raw.len() {
        header_end = raw.len();
    }
    let header_bytes = &raw[..header_end];
    let body = if header_end + 4 <= raw.len() {
        raw[header_end + 4..].to_vec()
    } else {
        Vec::new()
    };

    let mut headers: Vec<SmtpHeader> = Vec::new();
    let mut i = 0;
    while i < header_bytes.len() {
        let mut end = i;
        while end < header_bytes.len() {
            let mut nl = end;
            while nl + 1 < header_bytes.len() {
                if header_bytes[nl] == b'\r' && header_bytes[nl + 1] == b'\n' {
                    break;
                }
                nl += 1;
            }
            if nl + 1 >= header_bytes.len() {
                end = header_bytes.len();
                break;
            }
            if nl + 2 < header_bytes.len()
                && (header_bytes[nl + 2] == b' ' || header_bytes[nl + 2] == b'\t')
            {
                end = nl + 2;
                continue;
            }
            end = nl;
            break;
        }
        let raw_header = &header_bytes[i..end];
        i = if end + 2 <= header_bytes.len() && &header_bytes[end..end + 2] == b"\r\n" {
            end + 2
        } else {
            end
        };
        if raw_header.is_empty() {
            continue;
        }
        if let Some(colon_idx) = raw_header.iter().position(|&b| b == b':') {
            let name = String::from_utf8_lossy(&raw_header[..colon_idx])
                .trim()
                .to_string();
            let mut value = String::from_utf8_lossy(&raw_header[colon_idx + 1..]).to_string();
            if value.starts_with(' ') {
                value.remove(0);
            }
            headers.push(SmtpHeader { name, value });
        }
    }

    SmtpRequest {
        envelope: Some(SmtpEnvelope {
            from: SmtpAddress {
                user: "alice".into(),
                domain: "test.example.com".into(),
            },
            to: SmtpAddress {
                user: "recover".into(),
                domain: "id.ai".into(),
            },
        }),
        message: Some(SmtpMessage {
            headers,
            body: ByteBuf::from(body),
        }),
        gateway_flags: None,
    }
}

#[test]
fn verifies_end_to_end_with_no_dmarc_record_when_dkim_equals_from() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let result = verify_email(&req, SYNTH_RSA_TXT, None, frozen_now());
    match result {
        EmailVerificationStatus::Verified {
            dkim_domain,
            from_domain,
            dmarc,
            ..
        } => {
            assert_eq!(dkim_domain, "test.example.com");
            assert_eq!(from_domain, "test.example.com");
            assert_eq!(dmarc, DmarcOutcome::NoRecord);
        }
        other => panic!("expected Verified, got {:?}", other),
    }
}

#[test]
fn verifies_end_to_end_with_aligned_dmarc_strict() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let dmarc_txt = "v=DMARC1; p=reject; adkim=s";
    let result = verify_email(&req, SYNTH_RSA_TXT, Some(dmarc_txt), frozen_now());
    match result {
        EmailVerificationStatus::Verified { dmarc, .. } => {
            assert_eq!(
                dmarc,
                DmarcOutcome::Aligned {
                    policy: DmarcPolicy::Reject,
                    alignment_mode: AlignmentMode::Strict,
                }
            );
        }
        other => panic!("expected Verified, got {:?}", other),
    }
}

#[test]
fn verifies_end_to_end_with_aligned_dmarc_relaxed_default() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let dmarc_txt = "v=DMARC1; p=quarantine"; // adkim defaults to relaxed
    let result = verify_email(&req, SYNTH_RSA_TXT, Some(dmarc_txt), frozen_now());
    match result {
        EmailVerificationStatus::Verified { dmarc, .. } => {
            assert_eq!(
                dmarc,
                DmarcOutcome::Aligned {
                    policy: DmarcPolicy::Quarantine,
                    alignment_mode: AlignmentMode::Relaxed,
                }
            );
        }
        other => panic!("expected Verified, got {:?}", other),
    }
}

#[test]
fn rejects_end_to_end_when_dmarc_record_is_malformed() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let bad_dmarc = "this is not a DMARC record";
    let result = verify_email(&req, SYNTH_RSA_TXT, Some(bad_dmarc), frozen_now());
    match result {
        EmailVerificationStatus::Unverified { reason, .. } => {
            assert!(matches!(reason, VerificationFailReason::DmarcMalformed(_)));
        }
        other => panic!("expected Unverified(DmarcMalformed), got {:?}", other),
    }
}

#[test]
fn verifies_end_to_end_when_dmarc_record_has_unknown_tags() {
    // RFC 7489 §6.3 requires verifiers to ignore tags they don't
    // understand. We exercise the full verify_email path with a DMARC
    // record carrying reporting tags (`rua=`, `ruf=`, `fo=`), a vendor
    // extension we've never heard of, and a duplicate-looking-but-
    // different reporting tag — verification must still succeed.
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let dmarc_txt = "v=DMARC1; p=reject; rua=mailto:reports@example.com; \
                     ruf=mailto:forensics@example.com; fo=1; \
                     vendorext=experimental-value; adkim=s";
    let result = verify_email(&req, SYNTH_RSA_TXT, Some(dmarc_txt), frozen_now());
    match result {
        EmailVerificationStatus::Verified { dmarc, .. } => {
            assert_eq!(
                dmarc,
                DmarcOutcome::Aligned {
                    policy: DmarcPolicy::Reject,
                    alignment_mode: AlignmentMode::Strict,
                }
            );
        }
        other => panic!("expected Verified, got {:?}", other),
    }
}

#[test]
fn rejects_end_to_end_when_from_header_is_address_list() {
    let mut req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    // Replace From: with an address list. DKIM signs From, so this
    // also breaks the DKIM signature — but the verifier checks From
    // *after* DKIM, so the From-header parse failure will only show
    // up if DKIM happens to verify. Here we expect either NoSignature-
    // adjacent failures or, if DKIM passes, MalformedFromHeader.
    let message = req.message.as_mut().unwrap();
    for h in message.headers.iter_mut() {
        if h.name.eq_ignore_ascii_case("From") {
            h.value = "alice@test.example.com, eve@test.example.com".into();
        }
    }
    let result = verify_email(&req, SYNTH_RSA_TXT, None, frozen_now());
    // Whatever the precise failure mode, the result must be Unverified.
    assert!(matches!(result, EmailVerificationStatus::Unverified { .. }));
}
