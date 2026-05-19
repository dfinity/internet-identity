//! Shared DKIM tag-enforcement checks.
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
//! mode), all of which the DoH path enforces. These helpers are the
//! single source of truth that both pipelines call into.
//!
//! Helpers are split by *what data they need*, not by *which pipeline
//! calls them*:
//!
//! - Signature-header-only: [`check_signature_not_expired`],
//!   [`check_signature_not_from_future`], [`check_subject_signed`].
//!   Both pipelines have access to these inputs as soon as the
//!   `DKIM-Signature` header is parsed.
//! - DNS-record-dependent: [`check_auid_aligned`] (needs the DNS
//!   record's `t=s` flag), [`check_dns_not_testing`] (the `t=y` flag).
//!   The DoH pipeline has the DNS bytes up front; the DNSSEC pipeline
//!   gets them only after `submit_dkim_leaf`.

use super::dns_record::DkimDnsRecord;
use super::parse::DkimSignature;
use super::types::VerificationFailReason;

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
}
