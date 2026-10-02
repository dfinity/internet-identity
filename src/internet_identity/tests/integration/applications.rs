//! Tests for `list_applications`, the apps an identity has signed in to.

use canister_tests::api::internet_identity::api_v2::{
    create_account, list_applications, prepare_account_delegation, AccountDelegationParams,
};
use canister_tests::api::internet_identity::init_salt;
use canister_tests::api::internet_identity::notifications::grant_consent;
use canister_tests::flows;
use canister_tests::framework::{
    arg_with_notifications_enabled_for, env, install_ii_canister_with_arg, principal_1,
    principal_2, time, upgrade_ii_canister_with_arg, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AccountNumber, AnchorNumber, ApplicationInfo, ListApplicationsError, Timestamp,
};
use pocket_ic::{PocketIc, RejectResponse};
use pretty_assertions::assert_eq;
use serde_bytes::ByteBuf;
use std::time::Duration;

const NOTIFYING: &str = "https://notifying.example";
const QUIET: &str = "https://quiet.example";

/// A canister notifying for `NOTIFYING` alone, with the salt that account sign-ins need.
fn install(env: &PocketIc) -> CanisterId {
    let canister_id = install_ii_canister_with_arg(
        env,
        II_WASM.clone(),
        arg_with_notifications_enabled_for(&[NOTIFYING]),
    );
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

#[test]
fn should_list_nothing_for_an_identity_that_has_signed_in_nowhere() {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);

    assert_eq!(applications(&env, canister_id, anchor), vec![]);
}

/// Each app once, at the latest sign-in to any of the identity's accounts there,
/// whichever account it was.
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
    let (quiet_from, quiet_to) = sign_in_at(&env, canister_id, anchor, QUIET, named.account_number);
    env.advance_time(Duration::from_secs(60));
    let (notifying_from, notifying_to) = sign_in_at(&env, canister_id, anchor, NOTIFYING, None);

    let listed = applications(&env, canister_id, anchor);
    assert_eq!(
        listed
            .iter()
            .map(|app| app.origin.as_str())
            .collect::<Vec<_>>(),
        vec![NOTIFYING, QUIET],
    );
    assert!((notifying_from..=notifying_to).contains(&listed[0].last_used));
    assert!((quiet_from..=quiet_to).contains(&listed[1].last_used));
    Ok(())
}

/// Consent, a named account and a chosen default can each be stored at an app the
/// identity never signed in to, and none of them is a sign-in.
#[test]
fn should_not_list_an_app_the_identity_never_signed_in_to() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);

    grant_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        NOTIFYING.to_string(),
    )?
    .expect("consent refused");
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

#[test]
fn should_report_which_apps_may_notify() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    sign_in_at(&env, canister_id, anchor, QUIET, None);

    assert_eq!(
        applications(&env, canister_id, anchor)
            .iter()
            .map(|app| app.notifications_allowed)
            .collect::<Vec<_>>(),
        vec![false, false],
    );

    grant_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        NOTIFYING.to_string(),
    )?
    .expect("consent refused");

    assert_eq!(
        applications(&env, canister_id, anchor)
            .iter()
            .map(|app| (app.origin.as_str(), app.notifications_allowed))
            .collect::<Vec<_>>(),
        vec![(NOTIFYING, true), (QUIET, false)],
    );
    // Allowed is not notified: nothing was sent yet.
    assert!(applications(&env, canister_id, anchor)
        .iter()
        .all(|app| app.last_notified.is_none()));
    Ok(())
}

/// Consent the deployment no longer acts on is not reported as allowed, as
/// `notification_consent_granted` does not report it either.
#[test]
fn should_not_report_consent_for_an_app_the_deployment_stopped_notifying_for(
) -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install(&env);
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, NOTIFYING, None);
    grant_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        NOTIFYING.to_string(),
    )?
    .expect("consent refused");

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_notifications_enabled_for(&[QUIET]),
    )?;

    let listed = applications(&env, canister_id, anchor);
    assert_eq!(listed.len(), 1);
    assert!(!listed[0].notifications_allowed);
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
