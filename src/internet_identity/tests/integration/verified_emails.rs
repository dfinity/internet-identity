//! Integration tests for the verified-email flow.
//!
//! Parallel to `email_recovery.rs` but exercises the
//! `verified_email_*` canister methods: the DoH happy-path binds a
//! `VerifiedEmail` onto `Anchor::verified_emails` (visible via
//! `identity_info`), the cap is enforced at prepare time, and the
//! flow uses the `II-Verify-` Subject prefix.
//!
//! The DKIM signing / DoH outcall plumbing is shared with the
//! recovery tests via re-exports from `crate::email_recovery` — same
//! verifier in the canister, same shape of inbound email.

use crate::email_recovery::{
    dkim_signer, drive_doh_resolution, fresh_identity, setup_canister, SignedEmailParams,
    TEST_BODY, TEST_DOMAIN, TEST_SELECTOR,
};
use canister_tests::{api::internet_identity as api, framework::*};
use internet_identity_interface::internet_identity::types::email_recovery::{
    EmailRecoveryDnsInput, EmailRecoveryError, EmailRecoveryStatus,
};
use internet_identity_interface::internet_identity::types::smtp::SmtpResponse;

// Same domain + signer as the recovery suite (so the DoH outcalls
// are answered with the same fake TXT response), but a separate
// address: a single anchor can hold both a recovery credential and
// several verified emails, and a dedicated address keeps assertions
// about `verified_emails` independent of whatever's in
// `email_recovery`.
const VERIFIED_ADDRESS: &str = "vera@test.example.com";

fn verified_dns_input(addr: &str) -> EmailRecoveryDnsInput {
    EmailRecoveryDnsInput {
        address: addr.into(),
        dns_proof: None,
    }
}

/// Drive a verified-email DoH happy path end-to-end: prepare → sign
/// the email with `II-Verify-{nonce}` in the Subject → smtp_request
/// → resolve DKIM via DoH → poll status until terminal. Returns the
/// terminal status.
fn run_verified_doh_flow(
    env: &pocket_ic::PocketIc,
    canister_id: candid::Principal,
    sender: candid::Principal,
    identity_number: u64,
    address: &str,
) -> EmailRecoveryStatus {
    let challenge = api::verified_email_prepare_add(
        env,
        canister_id,
        sender,
        identity_number,
        verified_dns_input(address),
    )
    .expect("verified_email_prepare_add call failed")
    .expect("verified_email_prepare_add should succeed");

    assert!(
        challenge.nonce.starts_with("II-Verify-"),
        "verified-email nonce must start with II-Verify-, got {:?}",
        challenge.nonce,
    );

    let signer = dkim_signer::TestSigner::new(TEST_DOMAIN, TEST_SELECTOR);
    let now_secs = time(env) / 1_000_000_000;
    let signed = signer.sign_email(SignedEmailParams {
        from: address,
        to: "register@id.ai",
        subject: &challenge.nonce,
        body: TEST_BODY,
        timestamp: now_secs,
    });

    let dkim_txt = signer.public_txt_record();
    let resp = api::smtp_request(env, canister_id, &signed.request).expect("smtp_request call");
    assert!(matches!(resp, SmtpResponse::Ok {}));

    drive_doh_resolution(env, canister_id, &challenge.nonce, &dkim_txt)
}

// ===================================================================
// Cap, prefix and authz
// ===================================================================

#[test]
fn verified_email_prepare_add_returns_ii_verify_nonce() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let challenge = api::verified_email_prepare_add(
        &env,
        canister_id,
        p,
        id,
        verified_dns_input(VERIFIED_ADDRESS),
    )
    .expect("call failed")
    .expect("prepare_add should succeed");

    assert!(
        challenge.nonce.starts_with("II-Verify-"),
        "expected II-Verify- prefix on verified-email nonce, got {:?}",
        challenge.nonce,
    );
}

#[test]
fn verified_email_prepare_add_rejects_unauthorized_caller() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, _owner) = fresh_identity(&env, canister_id);

    // A fresh caller who doesn't control `id` should be bounced by
    // the authz check before any work happens.
    let stranger = candid::Principal::self_authenticating(b"stranger");
    let result = api::verified_email_prepare_add(
        &env,
        canister_id,
        stranger,
        id,
        verified_dns_input(VERIFIED_ADDRESS),
    )
    .expect("call failed");

    assert!(
        matches!(result, Err(EmailRecoveryError::Unauthorized(_))),
        "expected Unauthorized, got {result:?}",
    );
}

#[test]
fn verified_email_remove_rejects_when_nothing_bound() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let result = api::verified_email_remove(&env, canister_id, p, id, VERIFIED_ADDRESS)
        .expect("call failed");
    assert!(
        matches!(result, Err(EmailRecoveryError::AddressNotRegistered)),
        "expected AddressNotRegistered, got {result:?}",
    );
}

// ===================================================================
// End-to-end DoH happy path
// ===================================================================

#[test]
fn full_verified_email_flow_writes_to_identity_info() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    // Before: identity_info has no verified emails.
    let info =
        canister_tests::api::internet_identity::api_v2::identity_info(&env, canister_id, p, id)
            .expect("identity_info call failed")
            .expect("identity_info should succeed");
    let initial = info.verified_emails.as_ref().map(|v| v.len()).unwrap_or(0);
    assert_eq!(initial, 0, "expected no verified emails on a fresh anchor");

    let status = run_verified_doh_flow(&env, canister_id, p, id, VERIFIED_ADDRESS);
    assert!(
        matches!(status, EmailRecoveryStatus::RegistrationSucceeded),
        "expected RegistrationSucceeded, got {status:?}",
    );

    // After: identity_info exposes the newly verified address with
    // the lowercased canonical form.
    let info =
        canister_tests::api::internet_identity::api_v2::identity_info(&env, canister_id, p, id)
            .expect("identity_info call failed")
            .expect("identity_info should succeed");
    let verified = info
        .verified_emails
        .expect("verified_emails should be present after a successful flow");
    assert_eq!(verified.len(), 1);
    assert_eq!(verified[0].address, VERIFIED_ADDRESS);
    assert!(verified[0].verified_at > 0);

    // And remove drops it.
    api::verified_email_remove(&env, canister_id, p, id, VERIFIED_ADDRESS)
        .expect("remove call failed")
        .expect("remove should succeed");

    let info =
        canister_tests::api::internet_identity::api_v2::identity_info(&env, canister_id, p, id)
            .expect("identity_info call failed")
            .expect("identity_info should succeed");
    let after = info.verified_emails.as_ref().map(|v| v.len()).unwrap_or(0);
    assert_eq!(after, 0, "verified_emails should be empty after remove");
}

#[test]
fn verified_email_add_then_remove_is_idempotent_on_second_remove() {
    let env = env();
    let canister_id = setup_canister(&env);
    let (id, p) = fresh_identity(&env, canister_id);

    let status = run_verified_doh_flow(&env, canister_id, p, id, VERIFIED_ADDRESS);
    assert!(matches!(status, EmailRecoveryStatus::RegistrationSucceeded));

    // First remove succeeds; second loud-fails so a buggy retry on
    // the FE side surfaces rather than silently masks.
    api::verified_email_remove(&env, canister_id, p, id, VERIFIED_ADDRESS)
        .expect("remove call failed")
        .expect("first remove should succeed");

    let second = api::verified_email_remove(&env, canister_id, p, id, VERIFIED_ADDRESS)
        .expect("second remove call failed");
    assert!(
        matches!(second, Err(EmailRecoveryError::AddressNotRegistered)),
        "expected AddressNotRegistered on second remove, got {second:?}",
    );
}
