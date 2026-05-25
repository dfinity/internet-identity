//! End-to-end tests for the combined DKIM + DMARC pipeline.
//!
//! Reuses the synthetic `.eml` fixtures from `crate::dkim::test_vectors`
//! — they all sign `d=test.example.com` with `From: alice@test.example.com`,
//! so we get exact-match alignment regardless of the published DMARC
//! mode. The corresponding DKIM TXT record is also reused; we
//! synthesise the DMARC TXT inline.
//!
//! Drives the same typestate pipeline the canister-side
//! `handle_smtp_request` uses in production: stage 1 (RFC 5322 §3.6
//! well-formedness) → stage 2 (parse signatures) → stage 3
//! (cryptographic check + DMARC alignment). Asserts against the
//! resulting [`VerifiedSmtpRequest`] / [`VerificationError`].

use super::types::{AlignmentMode, DmarcOutcome, DmarcPolicy};
use crate::dkim::test_vectors::parse_eml;
use crate::dkim::VerificationFailReason;
use crate::email_recovery::typestate::{
    SignedSmtpRequestProjection, UnverifiedSmtpRequest, VerificationContext, VerificationError,
    VerifiedSmtpRequest,
};

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

fn run(
    req: internet_identity_interface::internet_identity::types::smtp::SmtpRequest,
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
fn verifies_end_to_end_with_no_dmarc_record_when_dkim_equals_from() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let verified = run(req, SYNTH_RSA_TXT, None, frozen_now())
        .expect("synth-rsa-relaxed-relaxed.eml must verify");
    assert_eq!(verified.winning_dkim_domain, "test.example.com");
    assert_eq!(verified.from_domain, "test.example.com");
    assert_eq!(verified.dmarc_outcome, DmarcOutcome::NoRecord);
}

#[test]
fn verifies_end_to_end_with_aligned_dmarc_strict() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let dmarc_txt = "v=DMARC1; p=reject; adkim=s";
    let verified = run(req, SYNTH_RSA_TXT, Some(dmarc_txt), frozen_now())
        .expect("strict-aligned DMARC must verify");
    assert_eq!(
        verified.dmarc_outcome,
        DmarcOutcome::Aligned {
            policy: DmarcPolicy::Reject,
            alignment_mode: AlignmentMode::Strict,
        }
    );
}

#[test]
fn verifies_end_to_end_with_aligned_dmarc_relaxed_default() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let dmarc_txt = "v=DMARC1; p=quarantine"; // adkim defaults to relaxed
    let verified = run(req, SYNTH_RSA_TXT, Some(dmarc_txt), frozen_now())
        .expect("relaxed-aligned DMARC must verify");
    assert_eq!(
        verified.dmarc_outcome,
        DmarcOutcome::Aligned {
            policy: DmarcPolicy::Quarantine,
            alignment_mode: AlignmentMode::Relaxed,
        }
    );
}

#[test]
fn rejects_end_to_end_when_dmarc_record_is_malformed() {
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let bad_dmarc = "this is not a DMARC record";
    let err = run(req, SYNTH_RSA_TXT, Some(bad_dmarc), frozen_now()).unwrap_err();
    assert!(matches!(
        err.last_reason,
        VerificationFailReason::DmarcMalformed(_)
    ));
}

#[test]
fn verifies_end_to_end_when_dmarc_record_has_unknown_tags() {
    // RFC 7489 §6.3 requires verifiers to ignore tags they don't
    // understand. We exercise the full pipeline with a DMARC record
    // carrying reporting tags (`rua=`, `ruf=`, `fo=`), a vendor
    // extension we've never heard of, and a duplicate-looking-but-
    // different reporting tag — verification must still succeed.
    let req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    let dmarc_txt = "v=DMARC1; p=reject; rua=mailto:reports@example.com; \
                     ruf=mailto:forensics@example.com; fo=1; \
                     vendorext=experimental-value; adkim=s";
    let verified = run(req, SYNTH_RSA_TXT, Some(dmarc_txt), frozen_now())
        .expect("unknown-tag DMARC must still verify");
    assert_eq!(
        verified.dmarc_outcome,
        DmarcOutcome::Aligned {
            policy: DmarcPolicy::Reject,
            alignment_mode: AlignmentMode::Strict,
        }
    );
}

#[test]
fn rejects_end_to_end_when_from_header_is_address_list() {
    let mut req = parse_eml(SYNTH_RSA_RELAXED_RELAXED);
    // Replace From: with an address list. The recovery flow never
    // accepts mail with multiple From mailboxes — DKIM signs `From`,
    // so this either breaks the DKIM signature or surfaces as a
    // malformed-From verdict downstream. Either is `Unverified` /
    // stage-3 failure.
    let message = req.message.as_mut().unwrap();
    for h in message.headers.iter_mut() {
        if h.name.eq_ignore_ascii_case("From") {
            h.value = "alice@test.example.com, eve@test.example.com".into();
        }
    }
    assert!(
        run(req, SYNTH_RSA_TXT, None, frozen_now()).is_err(),
        "address-list From must fail verification end-to-end"
    );
}
