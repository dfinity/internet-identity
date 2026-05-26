//! DKIM canonicalisation primitives used by the email-recovery
//! typestate.
//!
//! This module no longer carries a top-level `verify` entry point —
//! the canister consumes a [`crate::email_recovery::typestate::VerifiedSmtpRequest`]
//! produced by stage 3 of the typestate pipeline; the per-signature
//! DKIM math lives in that module's `verify_one_signature`.
//!
//! What stays here are two RFC 6376 §3.7 primitives the typestate
//! (and the DNSSEC-path partial-verification flow) calls into:
//!
//! - [`build_header_hash_input`] — selects the headers named by `h=`
//!   from the message bottom-up (RFC 6376 §5.4) and concatenates their
//!   relaxed-canonical forms, plus the DKIM-Signature header itself
//!   with its `b=` value blanked.
//! - [`simple_body`] — the `simple/*` body canonicalisation form.
//!   (`relaxed_body` lives in [`super::canonicalize`].)

use super::canonicalize::relaxed_header;
use super::parse::DkimSignature;
use internet_identity_interface::internet_identity::types::smtp::SmtpHeader;

const DKIM_SIGNATURE_HEADER: &str = "DKIM-Signature";

/// Construct the byte sequence the DKIM signature is computed over per
/// RFC 6376 §3.7:
///
/// ```text
/// signed_data = canon(h_header_1) || canon(h_header_2) || ... ||
///               canon(DKIM-Signature header with b= blanked, no
///                     trailing CRLF)
/// ```
///
/// The h= list is processed in the order given; for each name we pick
/// the *latest* matching header in the message (RFC 6376 §5.4 — bottom-
/// up pick when multiple instances exist). A name listed in h= that
/// doesn't appear in the message contributes the empty string.
///
/// `dkim_header_value` is the original (gateway-supplied) value of the
/// DKIM-Signature header itself; we blank its `b=` content as the spec
/// requires before canonicalising.
pub(crate) fn build_header_hash_input(
    all_headers: &[SmtpHeader],
    sig: &DkimSignature,
    dkim_header_value: &str,
) -> Vec<u8> {
    let mut out: Vec<u8> = Vec::new();

    // Track which occurrence of each name we've already used so we can
    // walk bottom-up correctly when the signer listed the same name
    // twice (RFC 6376 §5.4).
    let mut consumed: Vec<usize> = Vec::new();

    for h_name in &sig.h {
        // Find the latest matching header that isn't already consumed,
        // walking from the bottom up.
        let mut chosen: Option<usize> = None;
        for (idx, hdr) in all_headers.iter().enumerate().rev() {
            if hdr.name.eq_ignore_ascii_case(h_name) && !consumed.contains(&idx) {
                chosen = Some(idx);
                break;
            }
        }
        if let Some(idx) = chosen {
            consumed.push(idx);
            out.extend_from_slice(&relaxed_header(
                &all_headers[idx].name,
                &all_headers[idx].value,
            ));
        }
        // Else: "header named in h= but not present" — contributes empty
        // string to the hash input per RFC 6376 §3.7.
    }

    // Append the DKIM-Signature header itself, with `b=` value blanked,
    // and *no* trailing CRLF (§3.7).
    let dkim_blank = blank_b_tag_value(dkim_header_value);
    let mut canon_dkim = relaxed_header(DKIM_SIGNATURE_HEADER, &dkim_blank);
    // Strip the trailing CRLF that relaxed_header emits.
    if canon_dkim.ends_with(b"\r\n") {
        canon_dkim.truncate(canon_dkim.len() - 2);
    }
    out.extend_from_slice(&canon_dkim);
    out
}

/// Blank the value of the `b=` tag in a DKIM-Signature header value
/// while preserving the surrounding structure verbatim.
///
/// RFC 6376 §3.7 step 2 says the verifier hashes the DKIM-Signature
/// header "with the value of the 'b=' tag (including all surrounding
/// whitespace) deleted (i.e., treated as the empty string)". The bytes
/// **outside** the value — the tag name, any whitespace between the
/// name and `=`, the `=` itself, and everything that follows the next
/// `;` — must come through unchanged. With relaxed header canonicalisation
/// downstream collapsing whitespace and case anyway, the difference is
/// often invisible; with `B=` (uppercase) or `b\t=` (tab between name
/// and `=`), preserving original bytes is what keeps the relaxed-canon
/// hash matching the signer's input.
fn blank_b_tag_value(value: &str) -> String {
    let bytes = value.as_bytes();
    let mut out = String::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if !at_tag_start(bytes, i) || (bytes[i] != b'b' && bytes[i] != b'B') {
            out.push(bytes[i] as char);
            i += 1;
            continue;
        }
        // Possible `b=` tag head. Look ahead past optional WSP to the
        // `=`. If the `=` isn't there, this isn't actually a `b` tag —
        // just emit `b`/`B` and continue.
        let mut eq_idx = i + 1;
        while eq_idx < bytes.len() && bytes[eq_idx].is_ascii_whitespace() {
            eq_idx += 1;
        }
        if eq_idx >= bytes.len() || bytes[eq_idx] != b'=' {
            out.push(bytes[i] as char);
            i += 1;
            continue;
        }
        // Confirmed: `b...=` at structural position. Copy original bytes
        // verbatim from `i` through `eq_idx` so case (`B=`) and any
        // whitespace between the tag name and `=` (`b\t=`) survive.
        out.push_str(&value[i..=eq_idx]);
        // Drop the value: skip everything from after `=` to the next
        // `;` (or end-of-string).
        let mut k = eq_idx + 1;
        while k < bytes.len() && bytes[k] != b';' {
            k += 1;
        }
        i = k;
    }
    out
}

/// Whether index `i` in `bytes` is the start of a tag — either at the
/// very beginning, or preceded by `;` with only WSP in between.
fn at_tag_start(bytes: &[u8], i: usize) -> bool {
    if i == 0 {
        return true;
    }
    let mut j = i;
    while j > 0 && bytes[j - 1].is_ascii_whitespace() {
        j -= 1;
    }
    j > 0 && bytes[j - 1] == b';'
}

/// `simple` body canonicalisation (RFC 6376 §3.4.3): identity, except
/// trailing empty lines are stripped and the body must end with a
/// single `\r\n`. RFC §3.4.3 explicitly: "a completely empty or
/// missing body is canonicalized as a single 'CRLF'; that is, the
/// canonicalized length will be 2 octets" — so empty body → `\r\n`,
/// not the empty buffer. We support this only because the body side
/// of a `c=relaxed/simple` signature uses it; the header side is
/// rejected upstream.
pub(crate) fn simple_body(body: &[u8]) -> Vec<u8> {
    if body.is_empty() {
        return b"\r\n".to_vec();
    }
    let mut out = body.to_vec();
    // Strip trailing empty lines (`\r\n\r\n` → `\r\n`).
    while out.len() >= 4 && out.ends_with(b"\r\n\r\n") {
        out.truncate(out.len() - 2);
    }
    if !out.ends_with(b"\r\n") {
        out.extend_from_slice(b"\r\n");
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    // ---------------------------------------------------------------
    // `blank_b_tag_value` — unit tests for the RFC 6376 §3.7 step-2
    // primitive. Coverage moved here from the old `verify` entry
    // point's test module; the behaviour they assert is the same.
    // ---------------------------------------------------------------

    #[test]
    fn blank_b_tag_strips_simple_value() {
        let s = "v=1; a=rsa-sha256; b=YWJj; bh=ZGVm";
        let blanked = blank_b_tag_value(s);
        assert_eq!(blanked, "v=1; a=rsa-sha256; b=; bh=ZGVm");
    }

    #[test]
    fn blank_b_tag_only_targets_b_tag_not_bh() {
        let s = "v=1; bh=YWJj; b=ZGVm";
        let blanked = blank_b_tag_value(s);
        // bh= retained, b= blanked
        assert_eq!(blanked, "v=1; bh=YWJj; b=");
    }

    #[test]
    fn blank_b_tag_handles_b_at_start() {
        let s = "b=YWJj; v=1";
        let blanked = blank_b_tag_value(s);
        assert_eq!(blanked, "b=; v=1");
    }

    #[test]
    fn blank_b_tag_does_not_blank_internal_b_substring() {
        // A `b=`-like substring inside another tag's value (like a base64
        // payload) must not be blanked.
        let s = "v=1; bh=Yj1ub3RhdGFn; b=ZGVm";
        let blanked = blank_b_tag_value(s);
        assert_eq!(blanked, "v=1; bh=Yj1ub3RhdGFn; b=");
    }

    #[test]
    fn blank_b_tag_preserves_uppercase_b() {
        // RFC 6376 §3.5: tag names are case-insensitive. A signer that
        // emits `B=` (uppercase) is valid; we must keep the casing so
        // the relaxed-canon header hash matches the signer's input.
        let s = "v=1; B=YWJj; bh=ZGVm";
        let blanked = blank_b_tag_value(s);
        assert_eq!(blanked, "v=1; B=; bh=ZGVm");
    }

    #[test]
    fn blank_b_tag_preserves_wsp_before_equals() {
        // FWS is allowed between the tag name and `=` (RFC 6376 §3.2).
        // We must preserve those bytes — relaxed canon collapses them
        // later, but if we replaced them with `b=` here we'd be lying
        // to the canonicalizer about what bytes existed.
        let s = "v=1; b\t=YWJj; bh=ZGVm";
        let blanked = blank_b_tag_value(s);
        assert_eq!(blanked, "v=1; b\t=; bh=ZGVm");
    }

    #[test]
    fn blank_b_tag_preserves_space_before_equals() {
        let s = "v=1; b =YWJj; bh=ZGVm";
        let blanked = blank_b_tag_value(s);
        assert_eq!(blanked, "v=1; b =; bh=ZGVm");
    }

    // ---------------------------------------------------------------
    // End-to-end behaviour tests — what used to live alongside the
    // deleted `verify` entry point, now driven through the typestate.
    // Each test builds an RFC 5322-compliant `SmtpRequest`, runs it
    // through stage 1 (well-formedness) → stage 2 (parse signatures)
    // → stage 3 (cryptographic + DMARC check), and asserts on the
    // `VerificationError.last_reason`. The new shape catches a wider
    // class of malformed inputs (wire-shape problems surface earlier
    // as SMTP 555 via stage 1) but the DKIM-specific reasons asserted
    // here flow through stage 3 unchanged.
    // ---------------------------------------------------------------

    use super::super::types::VerificationFailReason;
    use crate::email_recovery::typestate::{
        SignedSmtpRequestProjection, UnverifiedSmtpRequest, VerificationContext,
        VerificationError, VerifiedSmtpRequest,
    };
    use internet_identity_interface::internet_identity::types::smtp::{
        SmtpAddress, SmtpEnvelope, SmtpMessage, SmtpRequest, SmtpResponse,
        SMTP_ERR_SYNTAX_ERROR,
    };
    use serde_bytes::ByteBuf;

    fn addr(user: &str, domain: &str) -> SmtpAddress {
        SmtpAddress {
            user: user.into(),
            domain: domain.into(),
        }
    }

    fn header(name: &str, value: &str) -> SmtpHeader {
        SmtpHeader {
            name: name.into(),
            value: value.into(),
        }
    }

    /// Build an RFC 5322-compliant `SmtpRequest` (From, Date, Subject
    /// each exactly once, one DKIM-Signature) for the per-test
    /// fixture. `dkim_value` is the tag content placed in the
    /// DKIM-Signature header — caller chooses what to put there.
    fn request_with_dkim_signature(dkim_value: &str) -> SmtpRequest {
        SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("alice", "example.com"),
                to: vec![addr("recover", "id.ai")],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    header("From", "alice@example.com"),
                    header("Date", "Mon, 1 Jan 2024 00:00:00 +0000"),
                    header("Subject", "II-Recovery-deadbeefcafe1234"),
                    header("DKIM-Signature", dkim_value),
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        }
    }

    fn run_typestate(
        req: SmtpRequest,
        dkim_txt: &str,
        now_secs: u64,
    ) -> Result<VerifiedSmtpRequest, VerificationError> {
        let unverified = UnverifiedSmtpRequest::try_from(req).expect("stage 1 must pass");
        let projections: Vec<SignedSmtpRequestProjection> = unverified
            .try_into()
            .expect("stage 2 must yield a projection");
        let ctx = VerificationContext {
            dkim_txt,
            dmarc_txt: None,
            now_secs,
        };
        VerifiedSmtpRequest::try_from((projections, &ctx))
    }

    #[test]
    fn no_signature_surfaces_as_stage1_rfc_violation() {
        // Stage 1 catches "no DKIM-Signature" before any DKIM-layer
        // code runs. The user-visible meaning is unchanged ("no
        // signature on this message") but the verdict now flows
        // through `validate_smtp_request`'s SMTP 555 surface —
        // encoding the fact that this is a wire-shape problem the
        // canister boundary returns to the gateway without doing
        // any cryptographic work.
        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: addr("alice", "example.com"),
                to: vec![addr("recover", "id.ai")],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    header("From", "alice@example.com"),
                    header("Date", "Mon, 1 Jan 2024 00:00:00 +0000"),
                    header("Subject", "II-Recovery-deadbeefcafe1234"),
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        };
        match UnverifiedSmtpRequest::try_from(req).unwrap_err() {
            SmtpResponse::Err(e) => {
                assert_eq!(e.code, SMTP_ERR_SYNTAX_ERROR);
                assert!(e.message.contains("'DKIM-Signature'"));
                assert!(e.message.contains("at least once"));
            }
            other => panic!("expected SmtpResponse::Err(555), got {other:?}"),
        }
    }

    #[test]
    fn rejects_simple_header_canon() {
        // Synthesise a DKIM-Signature header that uses c=simple/simple.
        // The body hash and signature won't be valid, but stage 3 will
        // get to the canonicalisation check first.
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=simple/simple; h=From:Subject:Date; bh=MTIzNDU2; b=YWJj";
        let req = request_with_dkim_signature(dkim_value);
        let err = run_typestate(req, "v=DKIM1; p=YWJj", 1_700_000_000).unwrap_err();
        assert_eq!(
            err.last_reason,
            VerificationFailReason::UnsupportedCanonicalization,
        );
    }

    #[test]
    fn rejects_expired_signature() {
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject:Date; bh=MTIzNDU2; b=YWJj; \
                          x=1000";
        let req = request_with_dkim_signature(dkim_value);
        let err = run_typestate(req, "v=DKIM1; p=YWJj", 1_700_000_000).unwrap_err();
        assert_eq!(err.last_reason, VerificationFailReason::SignatureExpired);
    }

    #[test]
    fn rejects_misaligned_auid() {
        // i= claims a different domain than d=
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject:Date; \
                          i=alice@evil.com; bh=MTIzNDU2; b=YWJj";
        let req = request_with_dkim_signature(dkim_value);
        let err = run_typestate(req, "v=DKIM1; p=YWJj", 1_700_000_000).unwrap_err();
        assert_eq!(err.last_reason, VerificationFailReason::AuidMisaligned);
    }

    #[test]
    fn rejects_future_dated_signature() {
        // t= claims a signing time well past now + CLOCK_SKEW_SECS.
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject:Date; \
                          t=1700001000; bh=MTIzNDU2; b=YWJj";
        let req = request_with_dkim_signature(dkim_value);
        // now = 1_700_000_000, t = 1_700_001_000 → 1000 s in the
        // future, well beyond CLOCK_SKEW_SECS (60).
        let err = run_typestate(req, "v=DKIM1; p=YWJj", 1_700_000_000).unwrap_err();
        assert_eq!(
            err.last_reason,
            VerificationFailReason::SignatureFutureDated
        );
    }

    #[test]
    fn accepts_slightly_future_t_within_skew() {
        // A `t=` value inside the CLOCK_SKEW_SECS window must NOT be
        // rejected as future-dated — that would create a spurious
        // failure mode for senders whose clock runs a few seconds
        // ahead of the canister's. We assert the t= check passes by
        // checking that the recorded `SignatureNotFromFuture` check
        // status is `Pass` on whichever verdict we get (a later check
        // — body-hash mismatch — trips the overall verdict).
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject:Date; \
                          t=1700000030; bh=MTIzNDU2; b=YWJj";
        let req = request_with_dkim_signature(dkim_value);
        // now = 1_700_000_000, t = 1_700_000_030 → 30 s in the future,
        // inside the 60 s skew window.
        let err = run_typestate(req, "v=DKIM1; p=YWJj", 1_700_000_000).unwrap_err();
        let future_check = err
            .combined_checks
            .iter()
            .find(|c| c.name == super::super::types::DkimCheckName::SignatureNotFromFuture)
            .expect("SignatureNotFromFuture check must be emitted");
        assert_eq!(
            future_check.status,
            super::super::types::DkimCheckStatus::Pass,
            "t=now+30s with 60s skew must pass the future-dated check; got {future_check:?}"
        );
    }

    #[test]
    fn rejects_signature_without_subject_in_h() {
        // h= covers From but not Subject. The challenge nonce lives
        // in Subject, so a signature that doesn't cover it would let
        // a MITM rewrite the nonce on a legitimately-signed email
        // (design §5.4).
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From; bh=MTIzNDU2; b=YWJj";
        let req = request_with_dkim_signature(dkim_value);
        let err = run_typestate(req, "v=DKIM1; p=YWJj", 1_700_000_000).unwrap_err();
        assert_eq!(err.last_reason, VerificationFailReason::SubjectNotSigned);
    }
}
