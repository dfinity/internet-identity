//! Test-vector loader and end-to-end DKIM verification tests.
//!
//! Each fixture is an `.eml` file (an RFC 5322 message including the
//! `DKIM-Signature` header that signed it) plus a `.txt` file holding
//! the corresponding DKIM TXT record. The loader parses the `.eml` into
//! the same `SmtpRequest` shape the gateway delivers in production:
//! per-header `(name, value)` pairs with continuation lines unfolded,
//! plus the body bytes verbatim.
//!
//! Synthetic vectors were produced offline (in the sandbox host) using
//! [`dkimpy`](https://launchpad.net/dkimpy) against a freshly-generated
//! 2048-bit RSA key. **The private key is not committed**: only the
//! signed `.eml` files and the matching DKIM TXT record (containing the
//! public key) live in `test_vectors/dkim/`. See that directory's
//! README for the regeneration procedure.
//!
//! Each test drives the email-recovery typestate end-to-end: stage 1
//! (RFC 5322 §3.6 well-formedness) → stage 2 (parse every
//! `DKIM-Signature`) → stage 3 (cryptographic check + DMARC alignment).
//! Failures surface via `VerificationError.last_reason`, which carries
//! the same `VerificationFailReason` values the per-signature inner
//! loop produces.

use super::types::VerificationFailReason;
use crate::email_recovery::typestate::{
    SignedSmtpRequestProjection, UnverifiedSmtpRequest, VerificationContext, VerificationError,
    VerifiedSmtpRequest,
};
use internet_identity_interface::internet_identity::types::smtp::{
    SmtpAddress, SmtpEnvelope, SmtpHeader, SmtpMessage, SmtpRequest,
};
use serde_bytes::ByteBuf;

const SYNTH_RSA_RELAXED_RELAXED: &[u8] =
    include_bytes!("../../../../test_vectors/dkim/synth-rsa-relaxed-relaxed.eml");
const SYNTH_RSA_RELAXED_SIMPLE: &[u8] =
    include_bytes!("../../../../test_vectors/dkim/synth-rsa-relaxed-simple.eml");
const SYNTH_RSA_SIMPLE_SIMPLE: &[u8] =
    include_bytes!("../../../../test_vectors/dkim/synth-rsa-simple-simple.eml");
const SYNTH_RSA_TXT: &str =
    include_str!("../../../../test_vectors/dkim/synth-rsa-test1._domainkey.test.example.com.txt");

/// Parse a raw RFC 5322 message into the `SmtpRequest` shape the gateway
/// produces. Continuation lines (those starting with WSP) are unfolded
/// into the previous header's value, with the leading WSP preserved per
/// RFC 5322 §2.2.3 so DKIM relaxed canonicalisation can collapse it.
pub(crate) fn parse_eml(raw: &[u8]) -> SmtpRequest {
    // Find the first \r\n\r\n that separates headers from body.
    let mut header_end = 0;
    while header_end + 4 <= raw.len() {
        if &raw[header_end..header_end + 4] == b"\r\n\r\n" {
            break;
        }
        header_end += 1;
    }
    if header_end + 4 > raw.len() {
        // No double-CRLF found — treat the whole thing as headers.
        header_end = raw.len();
    }

    let header_bytes = &raw[..header_end];
    let body = if header_end + 4 <= raw.len() {
        raw[header_end + 4..].to_vec()
    } else {
        Vec::new()
    };

    let headers = parse_headers(header_bytes);

    // Pick the From address as the envelope sender; this isn't strictly
    // necessary for DKIM verification but keeps the SmtpRequest a
    // realistic shape.
    let from = headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("From"))
        .map(|h| extract_address(&h.value))
        .unwrap_or_else(|| SmtpAddress {
            user: "unknown".into(),
            domain: "example.com".into(),
        });

    SmtpRequest {
        envelope: Some(SmtpEnvelope {
            from,
            to: vec![SmtpAddress {
                user: "recover".into(),
                domain: "id.ai".into(),
            }],
        }),
        message: Some(SmtpMessage {
            headers,
            body: ByteBuf::from(body),
        }),
        gateway_flags: None,
    }
}

/// Walk the header section, splitting into `(name, value)` pairs with
/// continuation lines unfolded.
fn parse_headers(bytes: &[u8]) -> Vec<SmtpHeader> {
    let mut headers: Vec<SmtpHeader> = Vec::new();
    let mut i = 0;
    while i < bytes.len() {
        // Find the end of this logical header (which may include
        // folded continuation lines).
        let mut end = i;
        while end < bytes.len() {
            // Find the next CRLF.
            let mut nl = end;
            while nl + 1 < bytes.len() {
                if bytes[nl] == b'\r' && bytes[nl + 1] == b'\n' {
                    break;
                }
                nl += 1;
            }
            if nl + 1 >= bytes.len() {
                end = bytes.len();
                break;
            }
            // Check whether the line after this CRLF is a continuation
            // (starts with WSP). If so, keep going; otherwise stop.
            if nl + 2 < bytes.len() && (bytes[nl + 2] == b' ' || bytes[nl + 2] == b'\t') {
                end = nl + 2;
                continue;
            }
            end = nl;
            break;
        }

        let raw_header = &bytes[i..end];
        // Skip the trailing CRLF if present.
        i = if end + 2 <= bytes.len() && &bytes[end..end + 2] == b"\r\n" {
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
            // The value preserves internal CRLF + WSP boundaries verbatim
            // — the verifier's relaxed canonicalisation handles unfolding.
            // Strip the single leading SP that conventionally follows
            // the colon (the gateway parser does the same).
            let mut value = String::from_utf8_lossy(&raw_header[colon_idx + 1..]).to_string();
            if value.starts_with(' ') {
                value.remove(0);
            }
            headers.push(SmtpHeader { name, value });
        }
    }
    headers
}

/// Extract `user@domain` from an address like `alice@example.com`. We
/// ignore RFC 5322 display-name complications because the test fixtures
/// don't include them.
fn extract_address(value: &str) -> SmtpAddress {
    let trimmed = value.trim();
    if let Some((user, domain)) = trimmed.split_once('@') {
        SmtpAddress {
            user: user.to_string(),
            domain: domain.to_string(),
        }
    } else {
        SmtpAddress {
            user: trimmed.to_string(),
            domain: "example.com".into(),
        }
    }
}

/// `now_secs` pinned to the synth fixtures' signing time so the
/// `t=`/`x=` checks pass even if the captured timestamps drift past
/// real wall-clock time.
fn frozen_now() -> u64 {
    1_777_972_289 // matches t= in the committed fixtures
}

/// Run an `SmtpRequest` through the full typestate pipeline against
/// the given DKIM/DMARC inputs. Stage-1 / stage-2 failures panic
/// (the fixtures must satisfy them — they're real signed `.eml`
/// files); only stage-3 results are returned.
fn run(
    req: SmtpRequest,
    dkim_txt: &str,
    dmarc_txt: Option<&str>,
    now: u64,
) -> Result<VerifiedSmtpRequest, VerificationError> {
    let unverified = UnverifiedSmtpRequest::try_from(req)
        .expect("fixture must satisfy stage 1 (bounds + RFC 5322 §3.6)");
    let projections: Vec<SignedSmtpRequestProjection> = unverified
        .try_into()
        .expect("fixture must parse at least one DKIM-Signature");
    let ctx = VerificationContext {
        dkim_txt,
        dmarc_txt,
        now_secs: now,
    };
    VerifiedSmtpRequest::try_from((projections, &ctx))
}

#[test]
fn verifies_synthetic_rsa_relaxed_relaxed() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let verified = run(req, SYNTH_RSA_TXT, None, frozen_now())
        .expect("synth-rsa-relaxed-relaxed.eml must verify");
    assert_eq!(verified.winning_dkim_domain, "test.example.com");
}

#[test]
fn verifies_synthetic_rsa_relaxed_simple_body() {
    let req = parse_eml(SYNTH_RSA_RELAXED_SIMPLE);
    let verified = run(req, SYNTH_RSA_TXT, None, frozen_now())
        .expect("synth-rsa-relaxed-simple.eml must verify");
    assert_eq!(verified.winning_dkim_domain, "test.example.com");
}

#[test]
fn rejects_simple_simple_canonicalization() {
    let req = parse_eml(SYNTH_RSA_SIMPLE_SIMPLE);
    let err = run(req, SYNTH_RSA_TXT, None, frozen_now()).unwrap_err();
    assert_eq!(
        err.last_reason,
        VerificationFailReason::UnsupportedCanonicalization,
    );
}

#[test]
fn rejects_flipped_body_byte() {
    let mut req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let message = req.message.as_mut().unwrap();
    let mut body = message.body.to_vec();
    if !body.is_empty() {
        body[0] ^= 0x01;
    }
    message.body = ByteBuf::from(body);
    let err = run(req, SYNTH_RSA_TXT, None, frozen_now()).unwrap_err();
    assert_eq!(err.last_reason, VerificationFailReason::BodyHashMismatch);
}

#[test]
fn rejects_flipped_signature_byte() {
    let mut req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let message = req.message.as_mut().unwrap();
    // Flip a byte inside the b= value of the DKIM-Signature header.
    for header in message.headers.iter_mut() {
        if header.name.eq_ignore_ascii_case("DKIM-Signature") {
            // Find a base64 char inside b= and toggle a bit. The b=
            // value is the last tag; we mutate near the end of the
            // header value to land inside it.
            let mut bytes: Vec<u8> = header.value.clone().into_bytes();
            // Walk backward to find a base64 alphanumeric near the end
            // (skipping trailing whitespace and `=` padding).
            for i in (0..bytes.len()).rev() {
                if bytes[i].is_ascii_alphabetic() {
                    bytes[i] = if bytes[i] == b'A' { b'B' } else { b'A' };
                    break;
                }
            }
            header.value = String::from_utf8(bytes).unwrap();
        }
    }
    let err = run(req, SYNTH_RSA_TXT, None, frozen_now()).unwrap_err();
    assert!(
        matches!(
            err.last_reason,
            VerificationFailReason::SignatureInvalid
                | VerificationFailReason::SignatureMalformed(_)
                | VerificationFailReason::BodyHashMismatch,
        ),
        "expected signature/body failure, got {:?}",
        err.last_reason
    );
}

#[test]
fn rejects_wrong_public_key() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    // A valid-shaped DKIM TXT record but with a different key
    // (truncated to a structural-but-not-correct value).
    let bad_txt = "v=DKIM1; k=rsa; p=MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxIDAQAB";
    let err = run(req, bad_txt, None, frozen_now()).unwrap_err();
    assert!(
        matches!(
            err.last_reason,
            VerificationFailReason::SignatureInvalid
                | VerificationFailReason::DnsRecordMalformed(_),
        ),
        "expected signature/DNS failure, got {:?}",
        err.last_reason
    );
}

#[test]
fn rejects_missing_dkim_signature_header() {
    // The DNS layer in production never delivers a message without a
    // DKIM-Signature; we exercise the negative path by stripping it.
    // Under the typestate the rejection moves up to stage 1 — the
    // RFC 5322 §3.6 well-formedness pass enforces ≥1 DKIM-Signature
    // when a message is present. The visible verdict is therefore an
    // `RfcError::HeaderCount` rather than a `VerificationFailReason`
    // — but the meaning is the same: "we can't reason about this
    // message because it has no signature."
    use crate::email_recovery::typestate::{HeaderCount, RfcError};
    let mut req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let message = req.message.as_mut().unwrap();
    message
        .headers
        .retain(|h| !h.name.eq_ignore_ascii_case("DKIM-Signature"));
    let err = UnverifiedSmtpRequest::try_from(req).unwrap_err();
    match err {
        RfcError::HeaderCount {
            header: "DKIM-Signature",
            found: 0,
            expected,
        } => {
            assert_eq!(expected, HeaderCount::AT_LEAST_ONE);
        }
        other => panic!("expected missing DKIM-Signature, got {other:?}"),
    }
}

#[test]
fn parse_eml_extracts_dkim_signature_header() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let message = req.message.unwrap();
    let dkim = message
        .headers
        .iter()
        .find(|h| h.name.eq_ignore_ascii_case("DKIM-Signature"))
        .expect("eml must have DKIM-Signature");
    // The value should contain the v=1, a=rsa-sha256 tags.
    assert!(dkim.value.contains("v=1"));
    assert!(dkim.value.contains("a=rsa-sha256"));
}
