//! Shared DKIM tag-enforcement checks and the **tag-contract facade**
//! both verification pipelines call into.
//!
//! The DKIM verifier runs as two different pipelines depending on
//! whether the sender's domain is DNSSEC-signed:
//!
//! - **DoH path** — [`super::verify::verify`] runs the full pipeline in
//!   one shot at email-arrival time, against a TXT record fetched via
//!   DoH (quorum-validated). All tag checks happen here.
//! - **DNSSEC path** — `email_recovery::smtp::prepare_partial_verification`
//!   runs the signature-header-only checks at email-arrival time
//!   (because the DNSSEC chain to the DKIM TXT is walked separately by
//!   the FE), then `email_recovery::submit_leaf::run_submit` runs the
//!   DNS-record-dependent checks once the TXT is verified.
//!
//! Without a shared check surface the two pipelines drift: an earlier
//! audit found the DNSSEC path silently bypassing `t=` (future-dated),
//! `x=` (expiration), `i=` (AUID alignment), and DNS `t=y` (testing
//! mode), all of which the DoH path enforces. To make that class of
//! drift impossible to reintroduce, both pipelines now go through the
//! *same two umbrella functions*:
//!
//! - [`enforce_signature_header_tag_contract`] — runs the three
//!   signature-header-only checks (`x=`, `t=`, Subject in `h=`).
//!   Called at email-arrival time on both paths.
//! - [`enforce_dns_record_tag_contract`] — runs the two DNS-record-
//!   dependent checks (`t=y` testing mode, `i=` AUID alignment under
//!   `t=s`). Called as soon as the DKIM TXT record is trusted.
//!
//! Adding a new tag policy means changing exactly one of those two
//! functions; both pipelines pick the change up automatically.
//! The individual `check_*` helpers stay public so the per-policy
//! decision logic is independently unit-testable and reachable from
//! the property-based parity tests, but no production caller should
//! invoke them directly — go through the umbrellas.
//!
//! ## Diagnostic trail
//!
//! The DoH pipeline surfaces a `Vec<DkimCheck>` to the FE so a UI can
//! render "which step failed". The DNSSEC pipeline doesn't need that
//! granularity (its failure is wrapped into a single
//! `EmailRecoveryError`). To keep one source of truth for *what the
//! trail looks like for each check*, the umbrellas build and return
//! the trail themselves; the DoH path appends it to its accumulator,
//! the DNSSEC path discards.

use super::dns_record::DkimDnsRecord;
use super::parse::DkimSignature;
use super::types::{DkimCheck, DkimCheckName, DkimCheckStatus, VerificationFailReason};

/// How far a signature's claimed signing time (`t=`) may sit in the
/// future relative to the canister's clock before we reject it as
/// future-dated. 60 seconds matches the DNSSEC verifier's
/// `CLOCK_SKEW_SECS` and absorbs the worst-case sender / canister
/// clock-skew while still rejecting signatures dated months ahead.
pub(crate) const CLOCK_SKEW_SECS: u64 = 60;

/// `x=` expiration check (RFC 6376 §3.5). Reject when the signer set
/// `x=` and the current time has moved past it. `Ok(())` when `x=` is
/// absent (signature is open-ended) or when the deadline is still in
/// the future.
pub(crate) fn check_signature_not_expired(
    sig: &DkimSignature,
    now_secs: u64,
) -> Result<(), VerificationFailReason> {
    if let Some(x) = sig.x {
        if now_secs > x {
            return Err(VerificationFailReason::SignatureExpired);
        }
    }
    Ok(())
}

/// `t=` future-dated check (design §5.4). RFC 6376 §3.5 describes `t=`
/// as "the time the signature was created"; one beyond
/// `now + CLOCK_SKEW_SECS` is either a misconfigured signer or a
/// manipulated replay. `Ok(())` when `t=` is absent or within skew.
pub(crate) fn check_signature_not_from_future(
    sig: &DkimSignature,
    now_secs: u64,
) -> Result<(), VerificationFailReason> {
    if let Some(t) = sig.t {
        if t > now_secs.saturating_add(CLOCK_SKEW_SECS) {
            return Err(VerificationFailReason::SignatureFutureDated);
        }
    }
    Ok(())
}

/// `h=` must include `Subject` (design §5.4). The challenge nonce
/// lives in `Subject:`; a signature that doesn't cover it would let a
/// man-in-the-middle rewrite the nonce on a legitimately-signed
/// email. RFC 6376 §5.4 already hard-requires `From` in `h=` (that
/// constraint is enforced by the parse layer); this helper adds the
/// recovery-specific tightening for `Subject`.
pub(crate) fn check_subject_signed(sig: &DkimSignature) -> Result<(), VerificationFailReason> {
    if sig.h.iter().any(|n| n.eq_ignore_ascii_case("subject")) {
        Ok(())
    } else {
        Err(VerificationFailReason::SubjectNotSigned)
    }
}

/// `i=` AUID alignment with `d=` (RFC 6376 §3.5). The domain after
/// `@` in `i=` MUST equal `d=`; when the DNS record's `t=s` flag is
/// **clear** (the default), a strict subdomain of `d=` is also
/// allowed. With `t=s` set, only exact match is permitted.
///
/// Takes `(i, d, dns_strict)` rather than `(&DkimSignature, …)` so
/// the DNSSEC submit path can call it without rehydrating a
/// `DkimSignature` from the stashed `PartialVerification`.
pub(crate) fn check_auid_aligned(
    i: &str,
    d: &str,
    dns_strict: bool,
) -> Result<(), VerificationFailReason> {
    if auid_aligns(i, d, dns_strict) {
        Ok(())
    } else {
        Err(VerificationFailReason::AuidMisaligned)
    }
}

/// DKIM DNS record `t=y` ("testing") flag (RFC 6376 §3.6.1). The
/// signer explicitly declared the key non-production; we treat any
/// signature under such a key as inconclusive — never accept it as
/// authoritative for binding an identity.
pub(crate) fn check_dns_not_testing(dns: &DkimDnsRecord) -> Result<(), VerificationFailReason> {
    if dns.testing {
        Err(VerificationFailReason::TestingMode)
    } else {
        Ok(())
    }
}

/// Whether `i=` aligns with `d=` per RFC 6376 §3.5.
///
/// `i=` is `[<local-part>]@<domain>`. The right side must equal `d=`
/// or — when `t=s` is **clear** in the DNS record — be a subdomain of
/// `d=`. With `t=s` set, only exact match is permitted.
pub(crate) fn auid_aligns(i: &str, d: &str, strict: bool) -> bool {
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

// =====================================================================
// Tag-contract facade.
//
// The two umbrella functions below are the *only* tag-enforcement
// entry points production code should use. Both pipelines call into
// them at the appropriate point in their flow; adding a new tag check
// means amending exactly one umbrella and both pipelines pick it up.
//
// Return shape:
//   Ok(trail)             — every check passed; trail has one
//                            `DkimCheck::Pass` entry per check that ran.
//   Err((reason, trail))  — `reason` is the first-failing
//                            `VerificationFailReason`; trail has the
//                            `Pass` entries from earlier checks plus
//                            the `Fail` entry of the one that broke.
// Symmetric with [`super::verify::try_verify_signature`]; the DoH
// pipeline appends the trail to its accumulator, the DNSSEC pipeline
// discards.
// =====================================================================

/// Run the three signature-header-only DKIM tag checks (design §5.4):
/// `x=` not expired, `t=` not future-dated, `Subject` ∈ `h=`. Called
/// as soon as the `DKIM-Signature` header has been parsed.
///
/// On the **DoH path** this runs inside
/// [`super::verify::try_verify_signature`] right after the
/// canonicalisation check. On the **DNSSEC path** it runs in
/// `email_recovery::smtp::prepare_partial_verification` so the
/// partial-verification record is only stashed if the email already
/// satisfies the signature-header-only contract.
pub(crate) fn enforce_signature_header_tag_contract(
    sig: &DkimSignature,
    now_secs: u64,
) -> Result<Vec<DkimCheck>, (VerificationFailReason, Vec<DkimCheck>)> {
    let mut trail: Vec<DkimCheck> = Vec::new();

    if let Err(reason) = check_signature_not_expired(sig, now_secs) {
        let detail = sig
            .x
            .map(|x| format!("signature expired at {x}, now {now_secs}"));
        trail.push(check(
            DkimCheckName::SignatureNotExpired,
            DkimCheckStatus::Fail,
            detail,
        ));
        return Err((reason, trail));
    }
    trail.push(check(
        DkimCheckName::SignatureNotExpired,
        DkimCheckStatus::Pass,
        None,
    ));

    if let Err(reason) = check_signature_not_from_future(sig, now_secs) {
        let detail = sig.t.map(|t| {
            format!("signature claims t={t}, now={now_secs}, beyond {CLOCK_SKEW_SECS}s skew")
        });
        trail.push(check(
            DkimCheckName::SignatureNotFromFuture,
            DkimCheckStatus::Fail,
            detail,
        ));
        return Err((reason, trail));
    }
    trail.push(check(
        DkimCheckName::SignatureNotFromFuture,
        DkimCheckStatus::Pass,
        None,
    ));

    if let Err(reason) = check_subject_signed(sig) {
        trail.push(check(
            DkimCheckName::SubjectSigned,
            DkimCheckStatus::Fail,
            Some("h= does not include the Subject header".to_string()),
        ));
        return Err((reason, trail));
    }
    trail.push(check(
        DkimCheckName::SubjectSigned,
        DkimCheckStatus::Pass,
        None,
    ));

    Ok(trail)
}

/// Run the two DNS-record-dependent DKIM tag checks (design §5.4):
/// reject if the record has `t=y` (testing mode) set, then require
/// `i=` to align with `d=` per the record's `t=s` flag. Called once
/// the DKIM TXT is trusted.
///
/// On the **DoH path** this runs inside
/// [`super::verify::try_verify_signature`] after `parse_dkim_txt`. On
/// the **DNSSEC path** it runs in
/// `email_recovery::submit_leaf::run_submit` after the chain
/// validator returns the verified record. Takes `(i, d)` rather than
/// the whole `DkimSignature` so the DNSSEC submit side — which only
/// has the cached `PartialVerification` — can call it without
/// rehydrating the full signature struct.
pub(crate) fn enforce_dns_record_tag_contract(
    i: &str,
    d: &str,
    dns: &DkimDnsRecord,
) -> Result<Vec<DkimCheck>, (VerificationFailReason, Vec<DkimCheck>)> {
    let mut trail: Vec<DkimCheck> = Vec::new();

    // `t=y` testing mode comes before AUID alignment because a
    // testing-flagged key invalidates any production signature
    // regardless of how `i=`/`d=` line up — the verdict is more
    // fundamental and worth surfacing first.
    if let Err(reason) = check_dns_not_testing(dns) {
        // No `DkimCheckName` variant for testing mode — RFC 6376
        // §3.6.1 treats `t=y` as a meta-flag on the record, not a
        // pass/fail-able step. The trail records the `Fail` against
        // the `DnsRecordParsed` step's downstream effect via the
        // surfaced `VerificationFailReason::TestingMode`.
        return Err((reason, trail));
    }

    if let Err(reason) = check_auid_aligned(i, d, dns.strict_auid) {
        let detail = format!("i={i} does not align with d={d} (t=s={})", dns.strict_auid);
        trail.push(check(
            DkimCheckName::AuidAlignsWithDomain,
            DkimCheckStatus::Fail,
            Some(detail),
        ));
        return Err((reason, trail));
    }
    trail.push(check(
        DkimCheckName::AuidAlignsWithDomain,
        DkimCheckStatus::Pass,
        None,
    ));

    Ok(trail)
}

fn check(name: DkimCheckName, status: DkimCheckStatus, detail: Option<String>) -> DkimCheck {
    DkimCheck {
        name,
        status,
        detail,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dkim::{Algorithm, BodyCanon, HeaderCanon, KeyType};

    fn sig_with(t: Option<u64>, x: Option<u64>, h: Vec<&str>, i: &str, d: &str) -> DkimSignature {
        DkimSignature {
            version: 1,
            algorithm: Algorithm::RsaSha256,
            d: d.into(),
            s: "sel".into(),
            c_header: HeaderCanon::Relaxed,
            c_body: BodyCanon::Relaxed,
            h: h.into_iter().map(String::from).collect(),
            l: None,
            bh: vec![0u8; 32],
            b: vec![0u8; 256],
            t,
            x,
            i: i.into(),
        }
    }

    fn dns(testing: bool, strict_auid: bool) -> DkimDnsRecord {
        DkimDnsRecord {
            key_type: KeyType::Rsa,
            public_key: vec![0u8; 32],
            testing,
            strict_auid,
        }
    }

    // ---- check_signature_not_expired ----

    #[test]
    fn x_absent_passes() {
        let s = sig_with(
            None,
            None,
            vec!["From", "Subject"],
            "@example.com",
            "example.com",
        );
        assert!(check_signature_not_expired(&s, 1_000_000).is_ok());
    }

    #[test]
    fn x_in_future_passes() {
        let s = sig_with(
            None,
            Some(2_000_000),
            vec!["From", "Subject"],
            "@example.com",
            "example.com",
        );
        assert!(check_signature_not_expired(&s, 1_000_000).is_ok());
    }

    #[test]
    fn x_in_past_fails_with_expired() {
        let s = sig_with(
            None,
            Some(900_000),
            vec!["From", "Subject"],
            "@example.com",
            "example.com",
        );
        assert_eq!(
            check_signature_not_expired(&s, 1_000_000).unwrap_err(),
            VerificationFailReason::SignatureExpired
        );
    }

    // ---- check_signature_not_from_future ----

    #[test]
    fn t_absent_passes() {
        let s = sig_with(
            None,
            None,
            vec!["From", "Subject"],
            "@example.com",
            "example.com",
        );
        assert!(check_signature_not_from_future(&s, 1_000_000).is_ok());
    }

    #[test]
    fn t_within_skew_window_passes() {
        // t = now + 30s, skew = 60s → fine.
        let s = sig_with(
            Some(1_000_030),
            None,
            vec!["From", "Subject"],
            "@example.com",
            "example.com",
        );
        assert!(check_signature_not_from_future(&s, 1_000_000).is_ok());
    }

    #[test]
    fn t_beyond_skew_window_fails_with_future_dated() {
        // t = now + 1000s, skew = 60s → reject.
        let s = sig_with(
            Some(1_001_000),
            None,
            vec!["From", "Subject"],
            "@example.com",
            "example.com",
        );
        assert_eq!(
            check_signature_not_from_future(&s, 1_000_000).unwrap_err(),
            VerificationFailReason::SignatureFutureDated
        );
    }

    // ---- check_subject_signed ----

    #[test]
    fn h_with_subject_passes() {
        let s = sig_with(
            None,
            None,
            vec!["From", "Subject"],
            "@example.com",
            "example.com",
        );
        assert!(check_subject_signed(&s).is_ok());
    }

    #[test]
    fn h_without_subject_fails() {
        let s = sig_with(None, None, vec!["From"], "@example.com", "example.com");
        assert_eq!(
            check_subject_signed(&s).unwrap_err(),
            VerificationFailReason::SubjectNotSigned
        );
    }

    #[test]
    fn subject_match_is_case_insensitive() {
        let s = sig_with(
            None,
            None,
            vec!["From", "sUbJeCt"],
            "@example.com",
            "example.com",
        );
        assert!(check_subject_signed(&s).is_ok());
    }

    // ---- check_auid_aligned ----

    #[test]
    fn auid_exact_match_passes() {
        assert!(check_auid_aligned("alice@example.com", "example.com", false).is_ok());
        assert!(check_auid_aligned("alice@example.com", "example.com", true).is_ok());
    }

    #[test]
    fn auid_subdomain_passes_in_loose_mode_but_not_strict() {
        assert!(check_auid_aligned("alice@mail.example.com", "example.com", false).is_ok());
        assert_eq!(
            check_auid_aligned("alice@mail.example.com", "example.com", true).unwrap_err(),
            VerificationFailReason::AuidMisaligned
        );
    }

    #[test]
    fn auid_evil_suffix_is_blocked() {
        // `evilexample.com` must not pass as inside `example.com`.
        assert_eq!(
            check_auid_aligned("alice@evilexample.com", "example.com", false).unwrap_err(),
            VerificationFailReason::AuidMisaligned
        );
    }

    #[test]
    fn auid_unrelated_domain_fails() {
        assert_eq!(
            check_auid_aligned("alice@evil.com", "example.com", false).unwrap_err(),
            VerificationFailReason::AuidMisaligned
        );
    }

    #[test]
    fn auid_aligns_handles_no_at_sign() {
        // A bare `i=` with no `@` is treated as the domain itself per
        // the existing parser default ("@d=") — exact match against
        // `d=` must still pass; anything else fails.
        assert!(auid_aligns("example.com", "example.com", false));
        assert!(!auid_aligns("evil.com", "example.com", false));
    }

    // ---- check_dns_not_testing ----

    #[test]
    fn dns_testing_flag_clear_passes() {
        assert!(check_dns_not_testing(&dns(false, false)).is_ok());
    }

    #[test]
    fn dns_testing_flag_set_fails_with_testing_mode() {
        assert_eq!(
            check_dns_not_testing(&dns(true, false)).unwrap_err(),
            VerificationFailReason::TestingMode
        );
    }

    // ---- Umbrella facade tests ----
    //
    // Each umbrella chains its constituent helpers and returns the
    // first failing reason along with a `Vec<DkimCheck>` trail that
    // the DoH path appends to its accumulator. Tests assert: (a) all
    // pass → `Ok(trail)` with every check `Pass`; (b) one failing
    // input → `Err((expected_reason, trail))` with the failed step
    // last; (c) the umbrella stops at the first failure (no later
    // checks contribute).

    use crate::dkim::DkimCheckStatus as Status;

    #[test]
    fn signature_header_umbrella_all_pass_returns_full_trail() {
        let s = sig_with(
            Some(1_000_000), // t= in the past
            Some(2_000_000), // x= in the future
            vec!["From", "Subject"],
            "alice@example.com",
            "example.com",
        );
        let trail = enforce_signature_header_tag_contract(&s, 1_500_000).expect("must pass");
        assert_eq!(trail.len(), 3);
        for c in &trail {
            assert_eq!(c.status, Status::Pass, "every check should pass; got {c:?}");
        }
        // Order is contract-defined: x= first, then t=, then Subject.
        assert_eq!(trail[0].name, DkimCheckName::SignatureNotExpired);
        assert_eq!(trail[1].name, DkimCheckName::SignatureNotFromFuture);
        assert_eq!(trail[2].name, DkimCheckName::SubjectSigned);
    }

    #[test]
    fn signature_header_umbrella_short_circuits_on_first_failure() {
        // x= expired (fails first) AND Subject missing AND t= future:
        // umbrella must surface SignatureExpired and stop walking.
        let s = sig_with(
            Some(9_999_999), // t= future-dated (later check)
            Some(900_000),   // x= already past
            vec!["From"],    // Subject missing (later check)
            "alice@example.com",
            "example.com",
        );
        match enforce_signature_header_tag_contract(&s, 1_000_000) {
            Err((VerificationFailReason::SignatureExpired, trail)) => {
                // Only the failing entry plus zero passes before it.
                assert_eq!(trail.len(), 1);
                assert_eq!(trail[0].name, DkimCheckName::SignatureNotExpired);
                assert_eq!(trail[0].status, Status::Fail);
            }
            other => panic!("expected SignatureExpired, got {other:?}"),
        }
    }

    #[test]
    fn signature_header_umbrella_subject_failure_surfaces_after_earlier_passes() {
        let s = sig_with(
            None,
            None,
            vec!["From"], // Subject missing
            "alice@example.com",
            "example.com",
        );
        match enforce_signature_header_tag_contract(&s, 1_000_000) {
            Err((VerificationFailReason::SubjectNotSigned, trail)) => {
                // Two preceding passes (x=, t=) plus the SubjectSigned Fail.
                assert_eq!(trail.len(), 3);
                assert_eq!(trail[2].name, DkimCheckName::SubjectSigned);
                assert_eq!(trail[2].status, Status::Fail);
            }
            other => panic!("expected SubjectNotSigned, got {other:?}"),
        }
    }

    #[test]
    fn dns_record_umbrella_all_pass_returns_alignment_pass_only() {
        // t=y clear, i=/d= aligned. The umbrella runs t=y first (no
        // dedicated DkimCheck variant — see doc on the umbrella) then
        // AuidAlignsWithDomain. So a clean pass yields exactly one
        // Pass entry: AuidAlignsWithDomain.
        let trail =
            enforce_dns_record_tag_contract("alice@example.com", "example.com", &dns(false, false))
                .expect("must pass");
        assert_eq!(trail.len(), 1);
        assert_eq!(trail[0].name, DkimCheckName::AuidAlignsWithDomain);
        assert_eq!(trail[0].status, Status::Pass);
    }

    #[test]
    fn dns_record_umbrella_testing_mode_surfaces_first() {
        // t=y set AND AUID misaligned: TestingMode wins because the
        // umbrella checks it first (a testing-flagged key invalidates
        // the signature regardless of `i=`/`d=` state). No trail
        // entries for testing mode (RFC 6376 §3.6.1 treats it as a
        // meta-flag).
        match enforce_dns_record_tag_contract(
            "alice@evil.com",
            "example.com",
            &dns(true, false), // testing=true
        ) {
            Err((VerificationFailReason::TestingMode, trail)) => {
                assert!(trail.is_empty(), "TestingMode emits no trail entry");
            }
            other => panic!("expected TestingMode, got {other:?}"),
        }
    }

    #[test]
    fn dns_record_umbrella_auid_failure_after_testing_pass() {
        match enforce_dns_record_tag_contract(
            "alice@evil.com",
            "example.com",
            &dns(false, false), // testing clear, AUID misaligned
        ) {
            Err((VerificationFailReason::AuidMisaligned, trail)) => {
                assert_eq!(trail.len(), 1);
                assert_eq!(trail[0].name, DkimCheckName::AuidAlignsWithDomain);
                assert_eq!(trail[0].status, Status::Fail);
            }
            other => panic!("expected AuidMisaligned, got {other:?}"),
        }
    }

    #[test]
    fn umbrellas_chain_to_match_individual_helpers() {
        // The umbrellas are nothing but ordered chains of the
        // individual `check_*` helpers. If for any input the umbrella
        // disagreed with the helpers about pass/fail, that would
        // mean a check silently slipped out of the chain — the exact
        // bug class this design is meant to make impossible.
        let s = sig_with(
            None,
            None,
            vec!["From", "Subject"],
            "alice@example.com",
            "example.com",
        );
        let r = dns(false, false);

        // Both individual helpers AND the umbrella accept this input.
        assert!(check_signature_not_expired(&s, 1_000_000).is_ok());
        assert!(check_signature_not_from_future(&s, 1_000_000).is_ok());
        assert!(check_subject_signed(&s).is_ok());
        assert!(check_dns_not_testing(&r).is_ok());
        assert!(check_auid_aligned(&s.i, &s.d, r.strict_auid).is_ok());

        assert!(enforce_signature_header_tag_contract(&s, 1_000_000).is_ok());
        assert!(enforce_dns_record_tag_contract(&s.i, &s.d, &r).is_ok());
    }
}
