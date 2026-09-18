//! Tests for the notification API, exercised through Candid against a canister.

use canister_tests::api::internet_identity::api_v2::prepare_account_session;
use canister_tests::api::internet_identity::notifications::{
    consent_granted, grant_consent, revoke_consent,
};
use canister_tests::flows;
use canister_tests::framework::{
    arg_with_captcha_disabled, arg_with_notifications_enabled_for, env,
    install_ii_canister_with_arg, principal_1, principal_2, upgrade_ii_canister, BrowserKey,
    II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserBrand, BrowserDescription, FormFactor, NotificationGrantConsentError,
    NotificationRevokeConsentError, OperatingSystem, PrepareAccountSessionRequest,
};
use pocket_ic::{PocketIc, RejectResponse};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;

const ORIGIN: &str = "https://some-dapp.com";
const GATEWAY: &str = "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app";
const UNREACHED: &str = "https://never-visited.example";
/// Every origin the tests below expect to be notifiable. One that is not enabled is
/// refused before anything else, which `should_refuse_an_origin_that_is_not_enabled`
/// covers on its own.
const ENABLED: &[&str] = &[ORIGIN, GATEWAY, UNREACHED];

/// A canister notifying for `ENABLED`, and one anchor registered to `principal_1`
/// which has signed in at `ORIGIN`.
fn install_with_anchor(env: &PocketIc) -> (CanisterId, AnchorNumber) {
    let canister_id = install_ii_canister_with_arg(
        env,
        II_WASM.clone(),
        arg_with_notifications_enabled_for(ENABLED),
    );
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
fn should_refuse_every_entry_point_while_no_origin_is_enabled() -> Result<(), RejectResponse> {
    let env = env();
    // The default arg, which enables no origin.
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_captcha_disabled());
    let anchor = flows::register_anchor(&env, canister_id);

    assert!(matches!(
        grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?,
        Err(NotificationGrantConsentError::InternalCanisterError(_))
    ));
    assert!(matches!(
        revoke_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?,
        Err(NotificationRevokeConsentError::InternalCanisterError(_))
    ));
    // A disabled deployment answers like one nobody consented on.
    assert!(!consent_granted(
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
    assert!(matches!(
        granted,
        Err(NotificationGrantConsentError::Unauthorized(_))
    ));

    // And the refusal wrote nothing.
    assert!(!consent_granted(
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

    assert!(!consent_granted(
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

    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);

    grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("grant rejected");
    assert!(consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);

    revoke_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("revoke rejected");
    assert!(!consent_granted(
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
        assert!(consent_granted(
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
            !consent_granted(&env, canister_id, principal_1(), anchor, spelling.into())?,
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
            matches!(
                result,
                Err(NotificationGrantConsentError::InternalCanisterError(_))
            ),
            "{origin} was not rejected: {result:?}"
        );
    }
    // None of the refusals wrote a row under the origin they canonicalize to.
    assert!(!consent_granted(
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
        grant_consent(&env, canister_id, principal_1(), anchor, UNREACHED.into())?,
        Err(NotificationGrantConsentError::NoSuchSession)
    );
    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        UNREACHED.into()
    )?);
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

    assert!(consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

/// The allowlist is what rolls the feature out app by app, so an origin left off it is
/// refused even for an identity that has signed in there.
#[test]
fn should_refuse_an_origin_that_is_not_enabled() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_notifications_enabled_for(&[ORIGIN]),
    );
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, ORIGIN, 1);
    sign_in_at(&env, canister_id, anchor, "https://other-dapp.com", 2);

    assert!(matches!(
        grant_consent(
            &env,
            canister_id,
            principal_1(),
            anchor,
            "https://other-dapp.com".into()
        )?,
        Err(NotificationGrantConsentError::InternalCanisterError(_))
    ));
    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://other-dapp.com".into()
    )?);
    Ok(())
}
