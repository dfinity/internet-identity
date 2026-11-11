//! Tests for `list_applications` and `forget_applications`: the apps an identity decided a
//! permission for, and forgetting them.

use candid::Principal;
use canister_tests::api::internet_identity::api_v2::{
    app_prepare_delegation, create_account, forget_applications, get_accounts, list_applications,
    prepare_account_delegation, prepare_account_session, AccountDelegationParams,
};
use canister_tests::api::internet_identity::init_salt;
use canister_tests::api::internet_identity::notifications::{grant_consent, revoke_consent};
use canister_tests::flows;
use canister_tests::framework::{
    arg_with_notifications_enabled, env, install_ii_canister_with_arg, principal_1, principal_2,
    time, upgrade_ii_canister_with_arg, BrowserKey, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, AppPrepareDelegationRequest, AppSessionError, ApplicationInfo,
    BrowserBrand, BrowserDescription, ForgetApplicationsError, FormFactor, InternetIdentityInit,
    ListApplicationsError, OperatingSystem, PrepareAccountSessionRequest, Timestamp,
};
use pocket_ic::{PocketIc, RejectResponse};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;
use std::time::Duration;

const NOTIFYING: &str = "https://notifying.example";
const QUIET: &str = "https://quiet.example";

/// A canister that notifies, with the salt that account sign-ins need.
fn install(env: &PocketIc) -> CanisterId {
    let canister_id =
        install_ii_canister_with_arg(env, II_WASM.clone(), arg_with_notifications_enabled());
    init_salt(env, canister_id).expect("failed to initialize the salt");
    canister_id
}

/// Signs `anchor` in at `origin` and answers the window the sign-in was stamped in.
fn sign_in_at(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor: AnchorNumber,
    origin: &str,
    account_number: Option<AccountNumber>,
) -> (Timestamp, Timestamp) {
    let before = time(env);
    prepare_account_delegation(
        &AccountDelegationParams::new(
            env,
            canister_id,
            principal_1(),
            anchor,
            origin.to_string(),
            account_number,
            ByteBuf::from(vec![1; 32]),
        ),
        None,
    )
    .expect("sign-in call failed")
    .expect("sign-in refused");
    (before, time(env))
}

/// Signs `anchor` in at `origin` through a session, and answers the principal the
/// session's chain roots at.
fn start_session(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor: AnchorNumber,
    origin: &str,
) -> Principal {
    let browser = BrowserKey::new(1);
    let session_key = ByteBuf::from(vec![1; 32]);
    let next_browser_key = browser.successor().public_key();
    let prepared = prepare_account_session(
        env,
        canister_id,
        principal_1(),
        PrepareAccountSessionRequest {
            identity_number: anchor,
            origin: origin.to_string(),
            account_number: None,
            browser_description: BrowserDescription {
                brand: BrowserBrand::Chrome,
                os: OperatingSystem::Macos,
                form_factor: FormFactor::Desktop,
                model: None,
            },
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
    .expect("session call failed")
    .expect("session refused");
    Principal::self_authenticating(&prepared.user_key)
}

fn allow(env: &PocketIc, canister_id: CanisterId, anchor: AnchorNumber, origin: &str) {
    grant_consent(env, canister_id, principal_1(), anchor, origin.to_string())
        .expect("consent call failed")
        .expect("consent refused");
}

fn deny(env: &PocketIc, canister_id: CanisterId, anchor: AnchorNumber, origin: &str) {
    revoke_consent(env, canister_id, principal_1(), anchor, origin.to_string())
        .expect("revoke call failed")
        .expect("revoke refused");
}

fn forget(env: &PocketIc, canister_id: CanisterId, anchor: AnchorNumber, origins: &[&str]) {
    forget_applications(
        env,
        canister_id,
        principal_1(),
        anchor,
        origins.iter().map(|origin| origin.to_string()).collect(),
    )
    .expect("forget call failed")
    .expect("forget refused");
}

/// The identity's applications, in a fixed order: the canister lists them in the order
/// it stores them, which is not one the caller can rely on.
fn applications(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor: AnchorNumber,
) -> Vec<ApplicationInfo> {
    let mut applications = list_applications(env, canister_id, principal_1(), anchor)
        .expect("list_applications call failed")
        .expect("list_applications refused");
    applications.sort_by(|a, b| a.origin.cmp(&b.origin));
    applications
}

fn allowed_by_origin(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor: AnchorNumber,
) -> Vec<(String, bool)> {
    applications(env, canister_id, anchor)
        .into_iter()
        .map(|app| (app.origin, app.notifications_allowed))
        .collect()
}

/// A sign-in, a named account and a chosen default are not permission decisions.
#[test]
fn should_not_list_an_app_the_identity_decided_nothing_for() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);

    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        QUIET.to_string(),
        "Work".to_string(),
    )?
    .expect("account creation refused");

    assert_eq!(applications(&env, canister_id, anchor), vec![]);
    Ok(())
}

/// Allowed, and allowed then denied, are both decisions the user may want to change.
#[test]
fn should_list_an_allowed_app_and_one_denied_since() {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    sign_in_at(&env, canister_id, anchor, QUIET, None);

    allow(&env, canister_id, anchor, NOTIFYING);
    allow(&env, canister_id, anchor, QUIET);
    deny(&env, canister_id, anchor, QUIET);

    assert_eq!(
        allowed_by_origin(&env, canister_id, anchor),
        vec![(NOTIFYING.to_string(), true), (QUIET.to_string(), false)],
    );
    // Allowed is not notified: nothing was sent yet.
    assert!(applications(&env, canister_id, anchor)
        .iter()
        .all(|app| app.last_notified.is_none()));
}

/// Denying an app the identity never allowed decides nothing.
#[test]
fn should_not_list_an_app_denied_without_being_allowed() {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);

    deny(&env, canister_id, anchor, NOTIFYING);

    assert_eq!(applications(&env, canister_id, anchor), vec![]);
}

/// Each app at the latest sign-in to any of the identity's accounts there, whichever
/// account it was, and without one where no account was used.
#[test]
fn should_list_each_app_at_its_latest_sign_in() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);

    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    env.advance_time(Duration::from_secs(60));
    let named = create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        QUIET.to_string(),
        "Work".to_string(),
    )?
    .expect("account creation refused");
    sign_in_at(&env, canister_id, anchor, QUIET, named.account_number);
    env.advance_time(Duration::from_secs(60));
    let (notifying_from, notifying_to) = sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    allow(&env, canister_id, anchor, NOTIFYING);
    const NEVER_SIGNED_IN: &str = "https://never-signed-in.example";
    allow(&env, canister_id, anchor, NEVER_SIGNED_IN);

    let listed = applications(&env, canister_id, anchor);
    assert_eq!(
        listed
            .iter()
            .map(|app| app.origin.as_str())
            .collect::<Vec<_>>(),
        vec![NEVER_SIGNED_IN, NOTIFYING],
    );
    assert_eq!(listed[0].last_used, None);
    let last_used = listed[1].last_used.expect("signed in at");
    assert!((notifying_from..=notifying_to).contains(&last_used));
    Ok(())
}

/// Consent the deployment no longer acts on is not reported as allowed, as
/// `notification_consent_granted` does not report it either. The app stays listed: the
/// decision is still the user's.
#[test]
fn should_not_report_consent_once_notifications_are_disabled() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    allow(&env, canister_id, anchor, NOTIFYING);

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            notifications_enabled: Some(false),
            ..Default::default()
        }),
    )?;

    assert_eq!(
        allowed_by_origin(&env, canister_id, anchor),
        vec![(NOTIFYING.to_string(), false)],
    );
    Ok(())
}

#[test]
fn should_refuse_a_caller_that_does_not_own_the_identity() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);

    assert_eq!(
        list_applications(&env, canister_id, principal_2(), anchor)?,
        Err(ListApplicationsError::Unauthorized(principal_2())),
    );
    Ok(())
}

/// Forgetting takes the apps off the list in one call and leaves the others.
#[test]
fn should_forget_the_apps_named_and_no_others() {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    const OTHER: &str = "https://other.example";
    for origin in [NOTIFYING, QUIET, OTHER] {
        sign_in_at(&env, canister_id, anchor, origin, None);
        allow(&env, canister_id, anchor, origin);
    }
    deny(&env, canister_id, anchor, QUIET);

    forget(&env, canister_id, anchor, &[NOTIFYING, QUIET]);

    assert_eq!(
        allowed_by_origin(&env, canister_id, anchor),
        vec![(OTHER.to_string(), true)],
    );
}

/// A forgotten app may not notify, and is back on the list only once it is allowed again.
#[test]
fn should_undo_consent_when_forgetting() {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    allow(&env, canister_id, anchor, NOTIFYING);

    forget(&env, canister_id, anchor, &[NOTIFYING]);
    assert_eq!(applications(&env, canister_id, anchor), vec![]);

    allow(&env, canister_id, anchor, NOTIFYING);
    assert_eq!(
        allowed_by_origin(&env, canister_id, anchor),
        vec![(NOTIFYING.to_string(), true)],
    );
}

#[test]
fn should_sign_out_every_session_at_a_forgotten_app() {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    let session_principal = start_session(&env, canister_id, anchor, NOTIFYING);
    allow(&env, canister_id, anchor, NOTIFYING);

    let refresh = |env: &PocketIc| {
        app_prepare_delegation(
            env,
            canister_id,
            session_principal,
            AppPrepareDelegationRequest {
                session_key: ByteBuf::from(vec![7; 32]),
            },
        )
        .expect("refresh call failed")
    };
    assert!(refresh(&env).is_ok());

    forget(&env, canister_id, anchor, &[NOTIFYING]);

    assert_eq!(refresh(&env), Err(AppSessionError::NoSuchSession));
}

/// Signing in again after forgetting lands in the same accounts.
#[test]
fn should_keep_the_accounts_of_a_forgotten_app() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        NOTIFYING.to_string(),
        "Work".to_string(),
    )?
    .expect("account creation refused");
    allow(&env, canister_id, anchor, NOTIFYING);
    let accounts = || {
        get_accounts(
            &env,
            canister_id,
            principal_1(),
            anchor,
            NOTIFYING.to_string(),
        )
        .expect("get_accounts call failed")
        .expect("get_accounts refused")
    };
    let before = accounts();

    forget(&env, canister_id, anchor, &[NOTIFYING]);

    assert_eq!(accounts(), before);
    Ok(())
}

/// An origin the identity holds nothing at is already forgotten, so a retry never fails.
#[test]
fn should_treat_forgetting_an_unknown_app_as_success() {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    allow(&env, canister_id, anchor, NOTIFYING);

    forget(&env, canister_id, anchor, &["https://unknown.example"]);
    forget(&env, canister_id, anchor, &[NOTIFYING]);
    forget(&env, canister_id, anchor, &[NOTIFYING]);

    assert_eq!(applications(&env, canister_id, anchor), vec![]);
}

#[test]
fn should_refuse_to_forget_for_a_caller_that_does_not_own_the_identity(
) -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    allow(&env, canister_id, anchor, NOTIFYING);

    assert_eq!(
        forget_applications(
            &env,
            canister_id,
            principal_2(),
            anchor,
            vec![NOTIFYING.to_string()],
        )?,
        Err(ForgetApplicationsError::Unauthorized(principal_2())),
    );
    assert_eq!(applications(&env, canister_id, anchor).len(), 1);
    Ok(())
}
