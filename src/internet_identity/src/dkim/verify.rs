//! DKIM verification entry point — orchestrates parsing, canonicalisation,
//! body hash, and signature verification per RFC 6376 §6.1.
//!
//! Caller-facing API (re-exported as `crate::dkim::verify_dkim`):
//!
//! ```ignore
//! pub fn verify_dkim(
//!     email: &SmtpRequest,
//!     dkim_txt: &str,
//!     now_secs: u64,
//! ) -> DkimVerifyResult
//! ```
//!
//! `dkim_txt` is the (already-trusted) content of the DKIM TXT record
//! at `<selector>._domainkey.<domain>` — in production it comes from
//! the DNSSEC verifier (`VerifiedRecord`, cached on the pending
//! challenge at prepare time) for DNSSEC-signed domains, or from
//! `crate::doh::fetch_txt` at email-arrival time otherwise. This
//! module does no DNS work itself; the caller delivers the bytes.
//!
//! Multi-signature behaviour (RFC 6376 §5.5 / design §5.5): an email
//! may carry multiple `DKIM-Signature` headers. The verifier accepts
//! the email as soon as *any one* signature passes; if all fail, it
//! returns `Unverified` with a best-fit reason.
//!
//! Tag enforcement (design §5.4):
//! - `v=` must be `1` (parse layer rejects others).
//! - `a=` must be `rsa-sha256` or `ed25519-sha256` (parse rejects rest).
//! - `c=` header side must be `relaxed` (rejected here).
//! - `x=` if present must be in the future.
//! - `t=` if present must not be in the future beyond `CLOCK_SKEW_SECS`.
//! - `h=` must include `From` (parse-layer reject) **and** `Subject`
//!   (rejected here as `SubjectNotSigned`). Subject coverage is
//!   recovery-specific — the challenge nonce lives in `Subject:` and a
//!   signature that doesn't cover it would let a MITM rewrite the
//!   nonce on a legitimately-signed email.
//! - `i=` must end in `@d` or `.d`. Soft-failed when DNS record has
//!   `t=s` cleared (i.e. accept subdomains by default).
//! - DNS `k=` must match `a=`'s underlying key type.
//! - DNS `t=y` (testing mode) — emit Unverified with TestingMode reason.

use super::canonicalize::{relaxed_body, relaxed_header};
use super::dns_record::parse_dkim_txt;
use super::parse::{parse_dkim_signature, DkimSignature};
use super::signature::{body_hash_sha256, verify_signature, VerifyOutcome};
use super::tag_checks::{enforce_dns_record_tag_contract, enforce_signature_header_tag_contract};
use super::types::{
    DkimCheck, DkimCheckName, DkimCheckStatus, DkimVerifyResult, HeaderCanon, SignedSmtpMessage,
    VerificationFailReason,
};
use internet_identity_interface::internet_identity::types::smtp::{
    SmtpHeader, SmtpRequest, SINGLE_INSTANCE_HEADERS,
};

const DKIM_SIGNATURE_HEADER: &str = "DKIM-Signature";

/// Verify an `SmtpRequest` against an already-trusted DKIM TXT record.
///
/// `now_secs` is Unix seconds — passed in so unit tests can pin time.
pub fn verify(email: &SmtpRequest, dkim_txt: &str, now_secs: u64) -> DkimVerifyResult {
    let message = match email.message.as_ref() {
        Some(m) => m,
        None => {
            return Unverified(VerificationFailReason::NoSignature, vec![]);
        }
    };

    // RFC 5322 §3.6 + RFC 6376 §8.15: refuse messages with duplicated
    // single-instance headers BEFORE attempting signature verification.
    // The wire-shape `validate_message` check normally catches this
    // first with a 555; this is the architectural backstop that
    // survives any future weakening of the wire check, and the only
    // line of defence for code paths that invoke the verifier without
    // routing through `validate_message`.
    if let Err((name, count)) = find_duplicate_single_instance_header(&message.headers) {
        let detail =
            format!("header '{name}' appears {count} times; RFC 5322 §3.6 allows at most one");
        return Unverified(
            VerificationFailReason::MalformedMessage(detail.clone()),
            vec![check(
                DkimCheckName::DkimSignaturePresent,
                DkimCheckStatus::Fail,
                Some(detail),
            )],
        );
    }

    // Find every DKIM-Signature header (case-insensitive). RFC 6376
    // allows multiple; we try them in order and accept on first pass.
    let dkim_headers: Vec<&SmtpHeader> = message
        .headers
        .iter()
        .filter(|h| h.name.eq_ignore_ascii_case(DKIM_SIGNATURE_HEADER))
        .collect();

    if dkim_headers.is_empty() {
        return Unverified(
            VerificationFailReason::NoSignature,
            vec![check(
                DkimCheckName::DkimSignaturePresent,
                DkimCheckStatus::Fail,
                Some("no DKIM-Signature header in message".into()),
            )],
        );
    }

    let mut all_checks: Vec<DkimCheck> = Vec::new();
    let mut last_reason = VerificationFailReason::NoSignature;

    for dkim_header in &dkim_headers {
        match try_verify_signature(email, message, dkim_header, dkim_txt, now_secs) {
            Ok((dkim_domain, signed, checks)) => {
                // Per `DkimVerifyResult::Verified.checks` ("checks for
                // the winning signature"), surface only the successful
                // attempt's per-step trail — not the accumulated trail
                // of every failed signature before it. Callers (and the
                // DMARC layer) reason about the winning signature only.
                return DkimVerifyResult::Verified {
                    dkim_domain,
                    signed,
                    checks,
                };
            }
            Err((reason, mut checks)) => {
                last_reason = reason;
                all_checks.append(&mut checks);
            }
        }
    }

    Unverified(last_reason, all_checks)
}

/// Walk `headers` and return `Err((name, count))` for the first
/// RFC 5322 §3.6 single-instance header that appears more than once.
/// `Ok(())` means no §3.6 violation found.
fn find_duplicate_single_instance_header(
    headers: &[SmtpHeader],
) -> Result<(), (&'static str, usize)> {
    for name in SINGLE_INSTANCE_HEADERS {
        let count = headers
            .iter()
            .filter(|h| h.name.eq_ignore_ascii_case(name))
            .count();
        if count > 1 {
            return Err((name, count));
        }
    }
    Ok(())
}

#[allow(non_snake_case)]
fn Unverified(reason: VerificationFailReason, checks: Vec<DkimCheck>) -> DkimVerifyResult {
    DkimVerifyResult::Unverified { reason, checks }
}

/// `(d=, signed_view, checks)` on success.
type TryVerifyOk = (String, SignedSmtpMessage, Vec<DkimCheck>);
/// `(reason, partial-checks)` on failure.
type TryVerifyErr = (VerificationFailReason, Vec<DkimCheck>);

/// Try to verify one specific DKIM-Signature header.
fn try_verify_signature(
    _email: &SmtpRequest,
    message: &internet_identity_interface::internet_identity::types::smtp::SmtpMessage,
    dkim_header: &SmtpHeader,
    dkim_txt: &str,
    now_secs: u64,
) -> Result<TryVerifyOk, TryVerifyErr> {
    let mut checks: Vec<DkimCheck> = Vec::new();
    checks.push(check(
        DkimCheckName::DkimSignaturePresent,
        DkimCheckStatus::Pass,
        None,
    ));

    // Parse the DKIM-Signature header value.
    let sig = match parse_dkim_signature(&dkim_header.value) {
        Ok(s) => s,
        Err(e) => {
            checks.push(check(
                DkimCheckName::SignatureParsed,
                DkimCheckStatus::Fail,
                Some(e.clone()),
            ));
            return Err((VerificationFailReason::SignatureMalformed(e), checks));
        }
    };
    checks.push(check(
        DkimCheckName::SignatureParsed,
        DkimCheckStatus::Pass,
        None,
    ));

    // (a=) Algorithm support is enforced by the parse layer; if we got
    // here, the algorithm is one of our two. Record the check.
    checks.push(check(
        DkimCheckName::AlgorithmSupported,
        DkimCheckStatus::Pass,
        None,
    ));

    // (c=) Reject simple/* on the header side. See design §5.2.
    if sig.c_header != HeaderCanon::Relaxed {
        let detail = "header canonicalisation must be relaxed (c=relaxed/...)".into();
        checks.push(check(
            DkimCheckName::CanonicalizationSupported,
            DkimCheckStatus::Fail,
            Some(detail),
        ));
        return Err((VerificationFailReason::UnsupportedCanonicalization, checks));
    }
    checks.push(check(
        DkimCheckName::CanonicalizationSupported,
        DkimCheckStatus::Pass,
        None,
    ));

    // Signature-header-only tag contract: x= not expired, t= not
    // future-dated, Subject in h=. Both pipelines route through the
    // same umbrella so the contract can't drift; the trail the
    // umbrella builds is appended verbatim to our per-check
    // accumulator.
    match enforce_signature_header_tag_contract(&sig, now_secs) {
        Ok(mut trail) => checks.append(&mut trail),
        Err((reason, mut trail)) => {
            checks.append(&mut trail);
            return Err((reason, checks));
        }
    }

    // Parse the DNS record now — we need its t=s flag to know how
    // strict to be on i= alignment.
    //
    // SECURITY: `dkim_txt` is treated as trusted bytes. The caller is
    // responsible for sourcing it from a DNSSEC-validated chain (PR
    // #3838) or, for unsigned domains, a DoH outcall whose host is
    // pinned per the design doc §7.6 (PR #3879).
    let dns = match parse_dkim_txt(dkim_txt) {
        Ok(r) => r,
        Err(e) => {
            checks.push(check(
                DkimCheckName::DnsRecordParsed,
                DkimCheckStatus::Fail,
                Some(e.clone()),
            ));
            return Err((VerificationFailReason::DnsRecordMalformed(e), checks));
        }
    };
    checks.push(check(
        DkimCheckName::DnsRecordParsed,
        DkimCheckStatus::Pass,
        None,
    ));

    // DNS-record-dependent tag contract: i= AUID aligned with d= per
    // the record's t=s flag, and the record itself isn't t=y
    // (testing). Routed through the same umbrella the DNSSEC path's
    // submit step calls, so the contract stays in lock-step. Note
    // the ordering shift relative to historical DoH-path code: the
    // umbrella runs t=y first (a testing-flagged key invalidates a
    // signature regardless of other tag state, so surfacing
    // TestingMode first is more informative), then AUID. Pre-refactor
    // ordering put t=y last among the DNS-record checks; if a
    // signature triggered both TestingMode and AlgorithmKeyTypeMismatch
    // the prior code surfaced the latter, this code surfaces
    // TestingMode.
    match enforce_dns_record_tag_contract(&sig.i, &sig.d, &dns) {
        Ok(mut trail) => checks.append(&mut trail),
        Err((reason, mut trail)) => {
            checks.append(&mut trail);
            return Err((reason, checks));
        }
    }

    // DNS k= must match the signature's algorithm family. Not a tag
    // policy check (it's a wire-format compatibility check between
    // the key type and the signing algorithm) so it stays outside
    // the tag-contract umbrella.
    if !dns.key_type.matches_signature_alg(sig.algorithm) {
        checks.push(check(
            DkimCheckName::PublicKeyTypeMatches,
            DkimCheckStatus::Fail,
            Some(format!(
                "DNS k= {:?} does not match signature a= {:?}",
                dns.key_type, sig.algorithm
            )),
        ));
        return Err((VerificationFailReason::AlgorithmKeyTypeMismatch, checks));
    }
    checks.push(check(
        DkimCheckName::PublicKeyTypeMatches,
        DkimCheckStatus::Pass,
        None,
    ));

    // Body hash check (bh=) — canonicalise the body, hash, compare.
    let canonical_body = match sig.c_body {
        super::types::BodyCanon::Relaxed => relaxed_body(&message.body),
        super::types::BodyCanon::Simple => simple_body(&message.body),
    };
    let computed_bh = body_hash_sha256(&canonical_body, sig.l);
    if computed_bh.as_slice() != sig.bh.as_slice() {
        checks.push(check(
            DkimCheckName::BodyHashValid,
            DkimCheckStatus::Fail,
            Some("computed body hash does not match bh=".into()),
        ));
        return Err((VerificationFailReason::BodyHashMismatch, checks));
    }
    checks.push(check(
        DkimCheckName::BodyHashValid,
        DkimCheckStatus::Pass,
        None,
    ));

    // Build the signed-data: relaxed-canonicalise each listed header
    // from `h=`, then append the DKIM-Signature header itself with its
    // `b=` value blanked, *without* a trailing CRLF (RFC 6376 §3.7).
    // The same walk yields the `SignedSmtpMessage` we return on
    // success so downstream callers read only the bytes DKIM hashed.
    let (signed_data, signed_view) =
        build_header_hash_input(&message.headers, &sig, &dkim_header.value);

    // Cryptographic signature check.
    let outcome = verify_signature(
        sig.algorithm,
        dns.key_type,
        &dns.public_key,
        &signed_data,
        &sig.b,
    );
    match outcome {
        VerifyOutcome::Valid => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Pass,
                None,
            ));
            Ok((sig.d.clone(), signed_view, checks))
        }
        VerifyOutcome::BadSignature => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some("signature did not validate against public key".into()),
            ));
            Err((VerificationFailReason::SignatureInvalid, checks))
        }
        VerifyOutcome::MalformedKey(e) => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some(format!("malformed key: {e}")),
            ));
            Err((VerificationFailReason::DnsRecordMalformed(e), checks))
        }
        VerifyOutcome::MalformedSignature(e) => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some(format!("malformed signature: {e}")),
            ));
            Err((VerificationFailReason::SignatureMalformed(e), checks))
        }
        VerifyOutcome::AlgorithmMismatch => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some("algorithm/key-type mismatch".into()),
            ));
            Err((VerificationFailReason::AlgorithmKeyTypeMismatch, checks))
        }
        VerifyOutcome::RsaKeyTooSmall(bits) => {
            checks.push(check(
                DkimCheckName::SignatureValid,
                DkimCheckStatus::Fail,
                Some(format!("RSA key only {bits} bits")),
            ));
            Err((VerificationFailReason::RsaKeyTooSmall(bits), checks))
        }
    }
}

fn check(name: DkimCheckName, status: DkimCheckStatus, detail: Option<String>) -> DkimCheck {
    DkimCheck {
        name,
        status,
        detail,
    }
}

/// Construct the byte sequence the DKIM signature is computed over per
/// RFC 6376 §3.7, **and** a [`SignedSmtpMessage`] populated from the
/// same walk so post-verification code can read header values without
/// re-deriving the bottom-up choice (and without ever touching the
/// raw `SmtpMessage`):
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
/// doesn't appear in the message contributes the empty string to the
/// hash input and no entry to the view.
///
/// `dkim_header_value` is the original (gateway-supplied) value of the
/// DKIM-Signature header itself; we blank its `b=` content as the spec
/// requires before canonicalising.
pub(crate) fn build_header_hash_input(
    all_headers: &[SmtpHeader],
    sig: &DkimSignature,
    dkim_header_value: &str,
) -> (Vec<u8>, super::types::SignedSmtpMessage) {
    let mut out: Vec<u8> = Vec::new();
    // Headers in the order DKIM's walk picks them - fed to
    // `SignedSmtpMessage::from_signed_walk` at the end so the view
    // is constructed exactly once and immutable thereafter.
    let mut picked: Vec<SmtpHeader> = Vec::new();

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
            // Record the verbatim header (not the canonicalised one)
            // for the view: downstream callers want to read it as
            // text, and the canonicalisation is deterministic from
            // the original so the cryptographic binding still holds.
            picked.push(all_headers[idx].clone());
        }
        // Else: "header named in h= but not present" — contributes empty
        // string to the hash input per RFC 6376 §3.7, and nothing to
        // the view (callers reading an absent name see `None`).
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
    (
        out,
        super::types::SignedSmtpMessage::from_signed_walk(picked),
    )
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

    // Tests for `auid_aligns` live alongside the helper itself in
    // [`super::super::tag_checks::tests`], since it's now shared with
    // the DNSSEC verification path.

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

    #[test]
    fn no_signature_returns_no_signature() {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpAddress, SmtpEnvelope, SmtpMessage,
        };
        use serde_bytes::ByteBuf;

        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: vec![SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                }],
            }),
            message: Some(SmtpMessage {
                headers: vec![SmtpHeader {
                    name: "From".into(),
                    value: "alice@example.com".into(),
                }],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        };
        let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
        match result {
            DkimVerifyResult::Unverified { reason, .. } => {
                assert_eq!(reason, VerificationFailReason::NoSignature);
            }
            other => panic!("expected Unverified(NoSignature), got {:?}", other),
        }
    }

    #[test]
    fn rejects_simple_header_canon() {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpAddress, SmtpEnvelope, SmtpMessage,
        };
        use serde_bytes::ByteBuf;

        // Synthesise a DKIM-Signature header that uses c=simple/simple.
        // The body hash and signature won't be valid, but parsing will
        // get to the canonicalisation check first.
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=simple/simple; h=From; bh=MTIzNDU2; b=YWJj";
        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: vec![SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                }],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    SmtpHeader {
                        name: "From".into(),
                        value: "alice@example.com".into(),
                    },
                    SmtpHeader {
                        name: "DKIM-Signature".into(),
                        value: dkim_value.into(),
                    },
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        };
        let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
        match result {
            DkimVerifyResult::Unverified { reason, .. } => {
                assert_eq!(reason, VerificationFailReason::UnsupportedCanonicalization);
            }
            other => panic!(
                "expected Unverified(UnsupportedCanonicalization), got {:?}",
                other
            ),
        }
    }

    #[test]
    fn rejects_expired_signature() {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpAddress, SmtpEnvelope, SmtpMessage,
        };
        use serde_bytes::ByteBuf;

        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From; bh=MTIzNDU2; b=YWJj; \
                          x=1000";
        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: vec![SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                }],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    SmtpHeader {
                        name: "From".into(),
                        value: "alice@example.com".into(),
                    },
                    SmtpHeader {
                        name: "DKIM-Signature".into(),
                        value: dkim_value.into(),
                    },
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        };
        let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
        match result {
            DkimVerifyResult::Unverified { reason, .. } => {
                assert_eq!(reason, VerificationFailReason::SignatureExpired);
            }
            other => panic!("expected Unverified(SignatureExpired), got {:?}", other),
        }
    }

    #[test]
    fn rejects_misaligned_auid() {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpAddress, SmtpEnvelope, SmtpMessage,
        };
        use serde_bytes::ByteBuf;

        // i= claims a different domain than d=
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject; \
                          i=alice@evil.com; bh=MTIzNDU2; b=YWJj";
        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: vec![SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                }],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    SmtpHeader {
                        name: "From".into(),
                        value: "alice@example.com".into(),
                    },
                    SmtpHeader {
                        name: "DKIM-Signature".into(),
                        value: dkim_value.into(),
                    },
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        };
        let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
        match result {
            DkimVerifyResult::Unverified { reason, .. } => {
                assert_eq!(reason, VerificationFailReason::AuidMisaligned);
            }
            other => panic!("expected Unverified(AuidMisaligned), got {:?}", other),
        }
    }

    #[test]
    fn rejects_future_dated_signature() {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpAddress, SmtpEnvelope, SmtpMessage,
        };
        use serde_bytes::ByteBuf;

        // t= claims a signing time well past now + CLOCK_SKEW_SECS.
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject; \
                          t=1700001000; bh=MTIzNDU2; b=YWJj";
        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: vec![SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                }],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    SmtpHeader {
                        name: "From".into(),
                        value: "alice@example.com".into(),
                    },
                    SmtpHeader {
                        name: "DKIM-Signature".into(),
                        value: dkim_value.into(),
                    },
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        };
        // now = 1_700_000_000, t = 1_700_001_000 → 1000 s in the future,
        // well beyond CLOCK_SKEW_SECS (60).
        let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
        match result {
            DkimVerifyResult::Unverified { reason, .. } => {
                assert_eq!(reason, VerificationFailReason::SignatureFutureDated);
            }
            other => panic!("expected Unverified(SignatureFutureDated), got {:?}", other),
        }
    }

    #[test]
    fn accepts_slightly_future_t_within_skew() {
        // A `t=` value inside the CLOCK_SKEW_SECS window must NOT be
        // rejected as future-dated — that would create a spurious
        // failure mode for senders whose clock runs a few seconds
        // ahead of the canister's. We test that the t= check passes;
        // a later check (body-hash mismatch) trips the verdict.
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpAddress, SmtpEnvelope, SmtpMessage,
        };
        use serde_bytes::ByteBuf;

        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From:Subject; \
                          t=1700000030; bh=MTIzNDU2; b=YWJj";
        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: vec![SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                }],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    SmtpHeader {
                        name: "From".into(),
                        value: "alice@example.com".into(),
                    },
                    SmtpHeader {
                        name: "DKIM-Signature".into(),
                        value: dkim_value.into(),
                    },
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        };
        // now = 1_700_000_000, t = 1_700_000_030 → 30 s in the future,
        // inside the 60 s skew window. The t= check must pass; we
        // assert that directly on the `checks` vec (available on both
        // outcome variants) rather than via a fall-through `if let`
        // that would silently skip the assertion if a future refactor
        // ever made this fixture pass overall.
        let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
        let checks = match &result {
            DkimVerifyResult::Verified { checks, .. }
            | DkimVerifyResult::Unverified { checks, .. } => checks,
        };
        let future_check = checks
            .iter()
            .find(|c| c.name == DkimCheckName::SignatureNotFromFuture)
            .expect("SignatureNotFromFuture check must be emitted");
        assert_eq!(
            future_check.status,
            DkimCheckStatus::Pass,
            "t=now+30s with 60s skew must pass the future-dated check; got {future_check:?}"
        );
    }

    #[test]
    fn rejects_signature_without_subject_in_h() {
        use internet_identity_interface::internet_identity::types::smtp::{
            SmtpAddress, SmtpEnvelope, SmtpMessage,
        };
        use serde_bytes::ByteBuf;

        // h= covers From but not Subject. The challenge nonce lives in
        // Subject, so a signature that doesn't cover it would let a
        // MITM rewrite the nonce on a legitimately-signed email
        // (design §5.4).
        let dkim_value = "v=1; a=rsa-sha256; d=example.com; s=mail; \
                          c=relaxed/relaxed; h=From; bh=MTIzNDU2; b=YWJj";
        let req = SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: vec![SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                }],
            }),
            message: Some(SmtpMessage {
                headers: vec![
                    SmtpHeader {
                        name: "From".into(),
                        value: "alice@example.com".into(),
                    },
                    SmtpHeader {
                        name: "DKIM-Signature".into(),
                        value: dkim_value.into(),
                    },
                ],
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        };
        let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
        match result {
            DkimVerifyResult::Unverified { reason, .. } => {
                assert_eq!(reason, VerificationFailReason::SubjectNotSigned);
            }
            other => panic!("expected Unverified(SubjectNotSigned), got {:?}", other),
        }
    }

    // =====================================================================
    // SignedSmtpMessage + duplicate-header reject (RFC 5322 §3.6 +
    // RFC 6376 §8.15). Builds messages directly rather than going
    // through the full DKIM crypto pipeline - we're testing the walk
    // + dedupe logic, not the signature math (which the existing
    // round-trip tests in `tests/integration/email_recovery.rs`
    // already cover end-to-end).
    // =====================================================================

    use super::super::types::SignedSmtpMessage;
    use internet_identity_interface::internet_identity::types::smtp::{
        SmtpAddress, SmtpEnvelope, SmtpMessage,
    };
    use serde_bytes::ByteBuf;

    fn req_with_headers(headers: Vec<SmtpHeader>) -> SmtpRequest {
        SmtpRequest {
            envelope: Some(SmtpEnvelope {
                from: SmtpAddress {
                    user: "alice".into(),
                    domain: "example.com".into(),
                },
                to: vec![SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                }],
            }),
            message: Some(SmtpMessage {
                headers,
                body: ByteBuf::from(b"hi".to_vec()),
            }),
            gateway_flags: None,
        }
    }

    /// Duplicate Subject - the exact DKIM-bypass shape - is rejected
    /// with `MalformedMessage`, regardless of whether the message
    /// would otherwise have a valid signature. This is the
    /// architectural backstop that holds even if a future change
    /// weakens `validate_message`'s wire-shape 555 check.
    #[test]
    fn rejects_duplicate_subject_header() {
        let req = req_with_headers(vec![
            SmtpHeader {
                name: "Subject".into(),
                value: "II-Recovery-fresh-from-attacker".into(),
            },
            SmtpHeader {
                name: "Subject".into(),
                value: "Original signed subject".into(),
            },
            SmtpHeader {
                name: "From".into(),
                value: "alice@example.com".into(),
            },
            SmtpHeader {
                name: "DKIM-Signature".into(),
                value: "v=1; a=rsa-sha256; d=example.com; s=mail; \
                        c=relaxed/relaxed; h=From:Subject; bh=MTIzNDU2; b=YWJj"
                    .into(),
            },
        ]);
        let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
        match result {
            DkimVerifyResult::Unverified { reason, .. } => match reason {
                VerificationFailReason::MalformedMessage(detail) => {
                    assert!(
                        detail.to_ascii_lowercase().contains("subject"),
                        "detail should name the offending header: {detail}",
                    );
                }
                other => panic!("expected MalformedMessage, got {other:?}"),
            },
            other => panic!("expected Unverified(MalformedMessage), got {other:?}"),
        }
    }

    /// Every RFC 5322 §3.6 single-instance header is covered, not
    /// just Subject. Closes the entire class - a future reader
    /// against any of these names is automatically safe.
    #[test]
    fn rejects_duplicate_for_each_single_instance_header() {
        use internet_identity_interface::internet_identity::types::smtp::SINGLE_INSTANCE_HEADERS;
        for name in SINGLE_INSTANCE_HEADERS {
            let req = req_with_headers(vec![
                SmtpHeader {
                    name: (*name).into(),
                    value: "a".into(),
                },
                SmtpHeader {
                    name: (*name).into(),
                    value: "b".into(),
                },
                SmtpHeader {
                    name: "DKIM-Signature".into(),
                    value: "v=1; a=rsa-sha256; d=example.com; s=mail; \
                            c=relaxed/relaxed; h=From:Subject; bh=MTIzNDU2; b=YWJj"
                        .into(),
                },
            ]);
            let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
            assert!(
                matches!(
                    result,
                    DkimVerifyResult::Unverified {
                        reason: VerificationFailReason::MalformedMessage(_),
                        ..
                    }
                ),
                "name={name}: expected MalformedMessage, got {result:?}",
            );
        }
    }

    /// Multiple `DKIM-Signature` headers must NOT trigger
    /// `MalformedMessage` - RFC 6376 §5.5 explicitly allows that
    /// (original sender + forwarder re-sign). The duplicate-header
    /// reject must be limited to §3.6 single-instance names.
    #[test]
    fn allows_multiple_dkim_signature_headers() {
        let req = req_with_headers(vec![
            SmtpHeader {
                name: "DKIM-Signature".into(),
                value: "v=1; a=rsa-sha256; d=a.example; s=m; c=relaxed/relaxed; \
                        h=From:Subject; bh=MTIzNDU2; b=YWJj"
                    .into(),
            },
            SmtpHeader {
                name: "DKIM-Signature".into(),
                value: "v=1; a=rsa-sha256; d=b.example; s=m; c=relaxed/relaxed; \
                        h=From:Subject; bh=MTIzNDU2; b=YWJj"
                    .into(),
            },
            SmtpHeader {
                name: "From".into(),
                value: "alice@example.com".into(),
            },
            SmtpHeader {
                name: "Subject".into(),
                value: "ok".into(),
            },
        ]);
        let result = verify(&req, "v=DKIM1; p=YWJj", 1_700_000_000);
        // Both signatures will fail the cryptographic check (bogus
        // bh=/b=) - we don't care which reason surfaces, only that
        // it's NOT `MalformedMessage`.
        match result {
            DkimVerifyResult::Unverified { reason, .. } => assert!(
                !matches!(reason, VerificationFailReason::MalformedMessage(_)),
                "multi DKIM-Signature must not trigger MalformedMessage, got {reason:?}",
            ),
            DkimVerifyResult::Verified { .. } => {
                // Equally fine - happy-path is also non-malformed.
            }
        }
    }

    /// `build_header_hash_input` populates the view with the exact
    /// header instances DKIM's walk picked. For `h=Subject` against a
    /// message with one Subject, the view's `header("Subject")` is
    /// that value. This pins the bottom-up contract: any future
    /// reader through the view inherits "only the bytes DKIM hashed."
    #[test]
    fn signed_view_exposes_bottom_up_subject() {
        let sig = parse_dkim_signature(
            "v=1; a=rsa-sha256; c=relaxed/relaxed; d=example.com; s=mail; \
                                  h=From:Subject; bh=MTIzNDU2; b=YWJj",
        )
        .expect("parse signature");
        let headers = vec![
            SmtpHeader {
                name: "From".into(),
                value: "alice@example.com".into(),
            },
            SmtpHeader {
                name: "Subject".into(),
                value: "the only signed subject".into(),
            },
            SmtpHeader {
                name: "DKIM-Signature".into(),
                value: "...".into(),
            },
        ];
        let (_bytes, view) = build_header_hash_input(&headers, &sig, "...");
        assert_eq!(view.header("Subject"), Some("the only signed subject"));
        assert_eq!(view.header("From"), Some("alice@example.com"));
    }

    /// A header listed in `h=` but absent from the message contributes
    /// nothing to the view: `view.header("Date")` returns `None` even
    /// though the signer asked for it. This is the "DKIM didn't sign
    /// it" signal the recovery layer relies on.
    #[test]
    fn signed_view_returns_none_for_h_listed_but_message_absent() {
        let sig = parse_dkim_signature(
            "v=1; a=rsa-sha256; c=relaxed/relaxed; d=example.com; s=mail; \
                                  h=From:Subject:Date; bh=MTIzNDU2; b=YWJj",
        )
        .expect("parse signature");
        let headers = vec![
            SmtpHeader {
                name: "From".into(),
                value: "alice@example.com".into(),
            },
            SmtpHeader {
                name: "Subject".into(),
                value: "x".into(),
            },
            // No Date header.
        ];
        let (_bytes, view) = build_header_hash_input(&headers, &sig, "...");
        assert_eq!(view.header("Date"), None);
        assert_eq!(view.header("Subject"), Some("x"));
    }

    /// A header present in the message but NOT in `h=` is not in the
    /// view: `view.header("Reply-To")` returns `None` even though the
    /// raw message had one. An attacker who slips an unsigned header
    /// into the wire cannot make a downstream reader through the view
    /// pick it up.
    #[test]
    fn signed_view_returns_none_for_message_present_but_not_in_h() {
        let sig = parse_dkim_signature(
            "v=1; a=rsa-sha256; c=relaxed/relaxed; d=example.com; s=mail; \
                                  h=From:Subject; bh=MTIzNDU2; b=YWJj",
        )
        .expect("parse signature");
        let headers = vec![
            SmtpHeader {
                name: "From".into(),
                value: "alice@example.com".into(),
            },
            SmtpHeader {
                name: "Subject".into(),
                value: "x".into(),
            },
            SmtpHeader {
                name: "Reply-To".into(),
                value: "attacker@evil.example".into(),
            },
        ];
        let (_bytes, view) = build_header_hash_input(&headers, &sig, "...");
        assert_eq!(view.header("Reply-To"), None);
    }

    /// `SignedSmtpMessage::empty()` returns `None` for every header,
    /// matching the fail-closed contract: code that ends up with an
    /// empty view (e.g. early-return fallbacks) cannot accidentally
    /// pick up an attacker-controlled value.
    #[test]
    fn signed_view_empty_returns_none_for_everything() {
        let v = SignedSmtpMessage::empty();
        assert_eq!(v.header("Subject"), None);
        assert_eq!(v.header("From"), None);
        assert_eq!(v.header_values("From").count(), 0);
    }

    /// `header()` lookup is case-insensitive on the name, matching
    /// how every other header lookup in the codebase works.
    #[test]
    fn signed_view_header_lookup_is_case_insensitive() {
        use internet_identity_interface::internet_identity::types::smtp::SmtpHeader;
        let v = SignedSmtpMessage::from_signed_walk(vec![SmtpHeader {
            name: "Subject".into(),
            value: "x".into(),
        }]);
        assert_eq!(v.header("SUBJECT"), Some("x"));
        assert_eq!(v.header("subject"), Some("x"));
        assert_eq!(v.header("sUbJeCt"), Some("x"));
    }
}
