//! Tests for the notification API, exercised through Candid against a canister.

use canister_tests::api::internet_identity::notifications::{
    consent_status, grant_consent, revoke_consent,
};
use canister_tests::flows;
use canister_tests::framework::{
    arg_with_captcha_disabled, arg_with_notifications_enabled, env, install_ii_canister_with_arg,
    principal_1, principal_2, upgrade_ii_canister, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{AnchorNumber, NotificationError};
use pocket_ic::{PocketIc, RejectResponse};
use pretty_assertions::assert_eq;

const ORIGIN: &str = "https://some-dapp.com";

/// A canister with the feature turned on, and one anchor registered to
/// `principal_1`.
fn install_with_anchor(env: &PocketIc) -> (CanisterId, AnchorNumber) {
    let canister_id =
        install_ii_canister_with_arg(env, II_WASM.clone(), arg_with_notifications_enabled());
    let anchor = flows::register_anchor(env, canister_id);
    (canister_id, anchor)
}

#[test]
fn should_refuse_every_entry_point_while_the_feature_is_off() -> Result<(), RejectResponse> {
    let env = env();
    // The default arg, which is what every deployment gets until one turns the
    // feature on.
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_captcha_disabled());
    let anchor = flows::register_anchor(&env, canister_id);

    assert_eq!(
        grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?,
        Err(NotificationError::Disabled)
    );
    assert_eq!(
        revoke_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?,
        Err(NotificationError::Disabled)
    );
    // The query answers rather than erroring, and must not leak that anything
    // exists: a disabled deployment looks exactly like one nobody consented on.
    assert!(!consent_status(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

#[test]
fn should_refuse_a_caller_that_does_not_own_the_anchor() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    let granted = grant_consent(&env, canister_id, principal_2(), anchor, ORIGIN.into())?;
    assert!(matches!(granted, Err(NotificationError::Unauthorized(_))));

    // And the refusal wrote nothing.
    assert!(!consent_status(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

#[test]
fn should_not_answer_a_query_for_an_anchor_the_caller_does_not_own() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("grant rejected");

    assert!(!consent_status(
        &env,
        canister_id,
        principal_2(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

#[test]
fn should_record_and_revoke_consent() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    assert!(!consent_status(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);

    grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("grant rejected");
    assert!(consent_status(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);

    revoke_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("revoke rejected");
    assert!(!consent_status(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

#[test]
fn should_fold_the_gateway_twins_of_one_app_into_one_consent() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    grant_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp0.io".into(),
    )?
    .expect("grant rejected");

    // The same canister named through another gateway is the same app, so the
    // grant is findable under it and does not add a second row.
    for twin in [
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app",
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp.net",
    ] {
        assert!(consent_status(
            &env,
            canister_id,
            principal_1(),
            anchor,
            twin.into()
        )?);
    }
    // Granting through a twin must land on that same row rather than adding one.
    grant_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app".into(),
    )?
    .expect("grant rejected");

    // So one revoke, through a third spelling, clears every spelling. A second row
    // would have survived it under one of them.
    revoke_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp.net".into(),
    )?
    .expect("revoke rejected");
    for spelling in [
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp0.io",
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app",
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp.net",
    ] {
        assert!(
            !consent_status(&env, canister_id, principal_1(), anchor, spelling.into())?,
            "{spelling} still reports consent after the single revoke"
        );
    }
    Ok(())
}

#[test]
fn should_reject_an_origin_that_is_not_a_bare_https_authority() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    for origin in [
        "http://some-dapp.com",
        "https://some-dapp.com/path",
        "https://some-dapp.com?query",
        "https://some-dapp.com#fragment",
        "https://",
        "",
        "some-dapp.com",
        "https://some-dapp.com:port",
    ] {
        let result = grant_consent(&env, canister_id, principal_1(), anchor, origin.into())?;
        assert!(
            matches!(result, Err(NotificationError::InvalidOrigin(_))),
            "{origin} was not rejected: {result:?}"
        );
    }
    // None of the refusals wrote a row under the origin they canonicalize to.
    assert!(!consent_status(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

/// A trap is a different answer from a refusal, so trapping on a number nobody
/// registered is how a caller learns which numbers are free. Only reachable through
/// Candid: the trapping read sits behind the authorization helpers.
#[test]
fn should_refuse_an_anchor_that_does_not_exist_without_trapping() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    let missing = anchor + 12_345;

    assert!(matches!(
        grant_consent(&env, canister_id, principal_1(), missing, ORIGIN.into())?,
        Err(NotificationError::Unauthorized(_))
    ));
    assert!(matches!(
        revoke_consent(&env, canister_id, principal_1(), missing, ORIGIN.into())?,
        Err(NotificationError::Unauthorized(_))
    ));
    assert!(!consent_status(
        &env,
        canister_id,
        principal_1(),
        missing,
        ORIGIN.into()
    )?);

    // And the refusal is the same one someone else's anchor gives, or the difference
    // between the two answers is itself the oracle.
    let absent = grant_consent(&env, canister_id, principal_2(), missing, ORIGIN.into())?;
    let not_yours = grant_consent(&env, canister_id, principal_2(), anchor, ORIGIN.into())?;
    assert_eq!(absent, not_yours);
    Ok(())
}

/// The one thing no unit test can see: the map lives in stable memory under an
/// index chosen for this feature, and an upgrade that lost it would be
/// unrecoverable after release.
#[test]
fn should_keep_consent_across_an_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("grant rejected");

    // No argument, which is what an upgrade that changes nothing sends: the
    // stored kill-switch value has to survive it too, or every endpoint would
    // come back refusing.
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    assert!(consent_status(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}
