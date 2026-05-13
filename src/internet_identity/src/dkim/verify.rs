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
//! PR 1's DNSSEC verifier (`VerifiedRecord`) for DNSSEC-signed domains
//! or from PR 4's DoH fallback otherwise. PR 2 takes it as input and
//! does no DNS work itself.
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
//! - `i=` must end in `@d` or `.d`. Soft-failed when DNS record has
//!   `t=s` cleared (i.e. accept subdomains by default).
//! - DNS `k=` must match `a=`'s underlying key type.
//! - DNS `t=y` (testing mode) — emit Unverified with TestingMode reason.

use super::canonicalize::{relaxed_body, relaxed_header};
use super::dns_record::parse_dkim_txt;
use super::parse::{parse_dkim_signature, DkimSignature};
use super::signature::{body_hash_sha256, verify_signature, VerifyOutcome};
use super::types::{
    DkimCheck, DkimCheckName, DkimCheckStatus, DkimVerifyResult, HeaderCanon,
    VerificationFailReason,
};
use internet_identity_interface::internet_identity::types::smtp::{SmtpHeader, SmtpRequest};

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
            Ok((dkim_domain, checks)) => {
                // Per `DkimVerifyResult::Verified.checks` ("checks for
                // the winning signature"), surface only the successful
                // attempt's per-step trail — not the accumulated trail
                // of every failed signature before it. Callers (and the
                // DMARC layer) reason about the winning signature only.
                return DkimVerifyResult::Verified {
                    dkim_domain,
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

#[allow(non_snake_case)]
fn Unverified(reason: VerificationFailReason, checks: Vec<DkimCheck>) -> DkimVerifyResult {
    DkimVerifyResult::Unverified { reason, checks }
}

/// Try to verify one specific DKIM-Signature header. Returns the `d=`
/// domain on success, or a `(reason, partial-checks)` pair on failure.
fn try_verify_signature(
    _email: &SmtpRequest,
    message: &internet_identity_interface::internet_identity::types::smtp::SmtpMessage,
    dkim_header: &SmtpHeader,
    dkim_txt: &str,
    now_secs: u64,
) -> Result<(String, Vec<DkimCheck>), (VerificationFailReason, Vec<DkimCheck>)> {
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

    // (x=) Expiration. If the signer set it and we're past it, reject.
    if let Some(x) = sig.x {
        if now_secs > x {
            let detail = format!("signature expired at {x}, now {now_secs}");
            checks.push(check(
                DkimCheckName::SignatureNotExpired,
                DkimCheckStatus::Fail,
                Some(detail),
            ));
            return Err((VerificationFailReason::SignatureExpired, checks));
        }
    }
    checks.push(check(
        DkimCheckName::SignatureNotExpired,
        DkimCheckStatus::Pass,
        None,
    ));

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

    // (i=) AUID alignment. RFC 6376 §3.5: i= MUST refer to a domain that
    // is `d=` or a subdomain of `d=`. With t=s set in the DNS record,
    // subdomains are NOT allowed.
    if !auid_aligns(&sig.i, &sig.d, dns.strict_auid) {
        let detail = format!(
            "i={} does not align with d={} (t=s={})",
            sig.i, sig.d, dns.strict_auid
        );
        checks.push(check(
            DkimCheckName::AuidAlignsWithDomain,
            DkimCheckStatus::Fail,
            Some(detail),
        ));
        return Err((VerificationFailReason::AuidMisaligned, checks));
    }
    checks.push(check(
        DkimCheckName::AuidAlignsWithDomain,
        DkimCheckStatus::Pass,
        None,
    ));

    // DNS k= must match the signature's algorithm family.
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

    // DNS t=y testing mode. Treat as inconclusive — never `Verified`.
    if dns.testing {
        return Err((VerificationFailReason::TestingMode, checks));
    }

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
    let signed_data = build_header_hash_input(&message.headers, &sig, &dkim_header.value);

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
            Ok((sig.d.clone(), checks))
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

/// Check whether `i=` aligns with `d=` per RFC 6376 §3.5.
///
/// `i=` is `[<local-part>]@<domain>`. The right side must equal `d=`
/// or — when `t=s` is **clear** in the DNS record — be a subdomain of
/// `d=`. With `t=s` set, only exact match is permitted.
fn auid_aligns(i: &str, d: &str, strict: bool) -> bool {
    let i_domain = i.split_once('@').map_or(i, |(_, dom)| dom);
    if i_domain == d {
        return true;
    }
    if strict {
        return false;
    }
    // Allow strict subdomain match: i_domain ends with ".d=" — note we
    // check the dot prefix so `evilexample.com` cannot fool `example.com`.
    i_domain.len() > d.len()
        && i_domain.ends_with(d)
        && i_domain.as_bytes()[i_domain.len() - d.len() - 1] == b'.'
}

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
fn build_header_hash_input(
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
fn simple_body(body: &[u8]) -> Vec<u8> {
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

    #[test]
    fn auid_aligns_exact_match() {
        assert!(auid_aligns("alice@example.com", "example.com", false));
        assert!(auid_aligns("alice@example.com", "example.com", true));
    }

    #[test]
    fn auid_aligns_subdomain_when_not_strict() {
        assert!(auid_aligns("alice@mail.example.com", "example.com", false));
        assert!(!auid_aligns("alice@mail.example.com", "example.com", true));
    }

    #[test]
    fn auid_does_not_match_evil_suffix() {
        // evilexample.com must not match example.com via suffix.
        assert!(!auid_aligns("alice@evilexample.com", "example.com", false));
    }

    #[test]
    fn auid_handles_local_only() {
        // No '@' → treat the whole thing as the domain.
        assert!(auid_aligns("example.com", "example.com", false));
    }

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
                to: SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                },
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
                to: SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                },
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
                to: SmtpAddress {
                    user: "recover".into(),
                    domain: "id.ai".into(),
                },
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
                          c=relaxed/relaxed; h=From; \
                          i=alice@evil.com; bh=MTIzNDU2; b=YWJj";
        let req = SmtpRequest {
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
}
