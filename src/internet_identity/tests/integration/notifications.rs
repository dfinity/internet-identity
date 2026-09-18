//! Tests for the notification API, exercised through Candid against a canister.

use canister_tests::api::internet_identity::api_v2::prepare_account_session;
use canister_tests::api::internet_identity::notifications::{
    consent_status, grant_consent, revoke_consent,
};
use canister_tests::flows;
use canister_tests::framework::{
    arg_with_captcha_disabled, arg_with_notifications_enabled, env, install_ii_canister_with_arg,
    principal_1, principal_2, upgrade_ii_canister, BrowserKey, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserBrand, BrowserDescription, FormFactor, NotificationError, OperatingSystem,
    PrepareAccountSessionRequest,
};
use pocket_ic::{PocketIc, RejectResponse};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;

const ORIGIN: &str = "https://some-dapp.com";

/// A canister with the feature turned on, and one anchor registered to `principal_1`
/// which has signed in at `ORIGIN`.
fn install_with_anchor(env: &PocketIc) -> (CanisterId, AnchorNumber) {
    let canister_id =
        install_ii_canister_with_arg(env, II_WASM.clone(), arg_with_notifications_enabled());
    let anchor = flows::register_anchor(env, canister_id);
    sign_in_at(env, canister_id, anchor, ORIGIN, 1);
    (canister_id, anchor)
}

fn chrome_on_a_mac() -> BrowserDescription {
    BrowserDescription {
        brand: BrowserBrand::Chrome,
        os: OperatingSystem::Macos,
        form_factor: FormFactor::Desktop,
        model: None,
    }
}

/// Signs `anchor` in at `origin`, which puts the origin in the application registry.
/// `browser_seed` names the browser: a key rotates on every sign-in, so a repeat sign-in
/// needs a browser of its own.
fn sign_in_at(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor: AnchorNumber,
    origin: &str,
    browser_seed: u8,
) {
    let browser = BrowserKey::new(browser_seed);
    let session_key = ByteBuf::from(vec![1; 32]);
    let next_browser_key = browser.successor().public_key();
    prepare_account_session(
        env,
        canister_id,
        principal_1(),
        PrepareAccountSessionRequest {
            identity_number: anchor,
            origin: origin.to_string(),
            account_number: None,
            browser_description: chrome_on_a_mac(),
            current_browser_key: browser.public_key(),
            current_browser_key_signature: browser.sign(&session_key, &next_browser_key),
            next_browser_key_signature: browser
                .successor()
                .sign_as_successor(&session_key, &browser.public_key()),
            next_browser_key,
            session_key,
            permissions: None,
            valid_for: None,
            max_idle: None,
        },
    )
    .expect("prepare_account_session rejected")
    .expect("prepare_account_session refused");
}

#[test]
fn should_refuse_every_entry_point_while_the_feature_is_off() -> Result<(), RejectResponse> {
    let env = env();
    // The default arg, which leaves the feature off.
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
    // A disabled deployment answers like one nobody consented on.
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
    // Signed in through the legacy spelling, which is the one the frontend remaps to.
    sign_in_at(
        &env,
        canister_id,
        anchor,
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app",
        2,
    );

    grant_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp0.io".into(),
    )?
    .expect("grant rejected");

    // Another gateway for the same canister is the same app.
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

    // So one revoke, through a third spelling, clears them all.
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

/// Consent hangs off the application, which only a sign-in mints. Its own variant
/// because signing in at the app clears it.
#[test]
fn should_refuse_consent_for_an_app_the_identity_has_never_reached() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    assert_eq!(
        grant_consent(
            &env,
            canister_id,
            principal_1(),
            anchor,
            "https://never-visited.example".into()
        )?,
        Err(NotificationError::SessionMissing)
    );
    assert!(!consent_status(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://never-visited.example".into()
    )?);
    Ok(())
}

/// A trap is a different answer from a refusal, so trapping on an unregistered number
/// tells a caller which numbers are free. Only reachable through Candid.
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

    // Word for word what someone else's anchor gives, or the difference is the oracle.
    let absent = grant_consent(&env, canister_id, principal_2(), missing, ORIGIN.into())?;
    let not_yours = grant_consent(&env, canister_id, principal_2(), anchor, ORIGIN.into())?;
    assert_eq!(absent, not_yours);
    Ok(())
}

/// The map lives in stable memory under an index chosen for this feature, and an
/// upgrade that lost it would be unrecoverable after release.
#[test]
fn should_keep_consent_across_an_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("grant rejected");

    // No argument, which is what an upgrade that changes nothing sends.
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
