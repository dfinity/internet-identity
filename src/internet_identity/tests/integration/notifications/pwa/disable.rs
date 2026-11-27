//! An app disabling its installed web app's subscription: nothing is sent to it, the
//! device it is on is not notified another way, and the browser that signs in to the app
//! on that device sees it on its installed apps path, where removing it unmutes the device.

use super::same_device::{
    installed_apps, linked_phone, path_status, phone, phone_with, probe,
    remove_unanswered_of_frontend, shared_entry_phone, Phone,
};
use super::*;
use canister_tests::api::internet_identity::notifications::{
    app_disable_pwa_webpush_subscription, app_prepare_pwa_webpush_subscription,
    app_probe_pwa_webpush_subscription, app_set_pwa_webpush_subscription,
};
use internet_identity_interface::internet_identity::types::{
    AppDisablePwaWebPushSubscriptionError, AppDisablePwaWebPushSubscriptionRequest,
    AppPreparePwaWebPushSubscriptionRequest, AppPreparePwaWebPushSubscriptionResult,
    AppProbePwaWebPushSubscriptionError, AppProbePwaWebPushSubscriptionRequest,
    AppSetPwaWebPushSubscriptionRequest, ProbeNotificationPathError,
};
use std::time::Duration;

/// `session` disables its installed web app of [`APP`]'s own origin.
fn disable_as(
    env: &PocketIc,
    canister_id: CanisterId,
    session: Principal,
) -> Result<Result<(), AppDisablePwaWebPushSubscriptionError>, RejectResponse> {
    app_disable_pwa_webpush_subscription(
        env,
        canister_id,
        session,
        AppDisablePwaWebPushSubscriptionRequest {
            frontend_origin: None,
        },
    )
}

/// `phone`'s installed web app disables its subscription.
fn disable(env: &PocketIc, phone: &Phone) -> Result<(), AppDisablePwaWebPushSubscriptionError> {
    disable_as(env, phone.canister_id, phone.pwa).expect("disable rejected")
}

/// Whether each subscription on `phone`'s installed apps path is disabled.
fn disabled_on_path(env: &PocketIc, phone: &Phone) -> Vec<Option<bool>> {
    path_status(env, phone, installed_apps(phone))
        .subscriptions
        .into_iter()
        .map(|subscription| subscription.is_disabled)
        .collect()
}

/// How many times [`prepare`] asks before giving up on a prepare that stays pending.
const PREPARE_ATTEMPTS: usize = 10;

/// `session` prepares a subscription of its installed web app of [`APP`]'s own origin,
/// asking again while the answer is pending, and leaves it awaiting its set.
fn prepare(env: &PocketIc, canister_id: CanisterId, session: Principal) {
    for _ in 0..PREPARE_ATTEMPTS {
        let prepare_result = app_prepare_pwa_webpush_subscription(
            env,
            canister_id,
            session,
            AppPreparePwaWebPushSubscriptionRequest {
                frontend_origin: None,
            },
        )
        .expect("prepare rejected");
        match prepare_result {
            AppPreparePwaWebPushSubscriptionResult::Ok(_) => return,
            AppPreparePwaWebPushSubscriptionResult::Pending => {}
            AppPreparePwaWebPushSubscriptionResult::Err(err) => {
                panic!("prepare refused: {err:?}")
            }
        }
    }
    panic!("prepare stayed pending");
}

/// A session of another account of `phone`'s identity at [`APP`], signed in on the
/// installed web app's browser entry.
fn another_accounts_installed_app_session(env: &PocketIc, phone: &Phone) -> Principal {
    let other_account_number = create_account(
        env,
        phone.canister_id,
        principal_1(),
        phone.anchor,
        APP.into(),
        "Other".to_string(),
    )
    .expect("creating the account rejected")
    .expect("creating the account refused")
    .account_number;
    session_of(&sign_in(
        env,
        phone.canister_id,
        principal_1(),
        PrepareAccountSessionRequest {
            account_number: other_account_number,
            ..session_request(phone.anchor, APP, &BrowserKey::new(5).successor())
        },
    ))
}

/// Longer than the default 7 days a session may go unused before it ends: the installed
/// app's session has ended once the clock moves this far.
const PAST_THE_DEFAULT_IDLE_TIME: Duration = Duration::from_secs(8 * 24 * 60 * 60);

/// Moves the clock past the end of `phone`'s installed web app's session, and signs Safari
/// in again, so only the installed app's session has ended.
fn end_the_installed_apps_session(env: &PocketIc, phone: &Phone) {
    env.advance_time(PAST_THE_DEFAULT_IDLE_TIME);
    sign_in(
        env,
        phone.canister_id,
        principal_1(),
        session_request(phone.anchor, APP, &BrowserKey::new(1).successor()),
    );
}

// ---- Who may call ----

#[test]
fn should_refuse_to_disable_for_every_caller_without_a_live_app_session(
) -> Result<(), RejectResponse> {
    let env = env();
    let phone = shared_entry_phone(&env);

    for stranger in [
        principal_1(),
        BrowserKey::new(1).successor().principal(),
        principal_2(),
        Principal::anonymous(),
    ] {
        assert_eq!(
            disable_as(&env, phone.canister_id, stranger)?,
            Err(AppDisablePwaWebPushSubscriptionError::NoSuchSession)
        );
    }
    // None of them disabled the subscription.
    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_leave_the_installed_app_of_another_identity_when_disabling_ones_own(
) -> Result<(), RejectResponse> {
    let env = env();
    let phone = shared_entry_phone(&env);
    let other_session = session_of(&second_identity_at(&env, phone.canister_id, APP));

    assert_eq!(disable_as(&env, phone.canister_id, other_session)?, Ok(()));

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_refuse_to_disable_while_notifications_are_disabled() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_captcha_disabled());
    let anchor = flows::register_anchor(&env, canister_id);
    let session = session_of(&sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &BrowserKey::new(1)),
    ));

    assert!(matches!(
        disable_as(&env, canister_id, session)?,
        Err(AppDisablePwaWebPushSubscriptionError::InternalCanisterError(_))
    ));
    Ok(())
}

// ---- Disabling ----

#[test]
fn should_answer_ok_and_leave_the_browser_woken_where_nothing_is_subscribed(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, _, account_session) = install_signed_in(&env, None, None);

    assert_eq!(
        disable_as(&env, canister_id, session_of(&account_session))?,
        Ok(())
    );

    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 1, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_keep_a_disabled_installed_app_disabled_when_it_is_disabled_again() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(disabled_on_path(&env, &phone), vec![Some(true)]);
}

// ---- Delivery ----

#[test]
fn should_send_nothing_to_a_disabled_installed_app_or_the_browser_sharing_its_entry() {
    let env = env();
    let phone = shared_entry_phone(&env);
    // Accepted while the installed app is enabled; the pass that posts it runs after the
    // disable.
    send_accepted(&env, phone.canister_id, phone.account_principal, 1, 201);

    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(answer_outcalls(&env, 201), Vec::<String>::new());
    assert_eq!(
        send_refusal(&env, phone.canister_id, phone.account_principal, 2),
        Some(NotAcceptedReason::NoChannel)
    );
}

#[test]
fn should_send_nothing_to_a_disabled_linked_installed_app_or_to_safari() {
    let env = env();
    let phone = linked_phone(&env);
    send_accepted(&env, phone.canister_id, phone.account_principal, 1, 201);

    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(answer_outcalls(&env, 201), Vec::<String>::new());
}

#[test]
fn should_still_wake_safari_where_the_disabled_installed_apps_entry_is_not_linked() {
    let env = env();
    let phone = phone(&env);

    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_wake_the_installed_app_again_once_it_subscribes_again() {
    let env = env();
    let phone = shared_entry_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    subscribe_pwa_webpush(
        &env,
        phone.canister_id,
        phone.pwa,
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_keep_the_installed_app_disabled_across_an_upgrade() {
    let env = env();
    let phone = shared_entry_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    upgrade_ii_canister(&env, phone.canister_id, II_WASM.clone());

    assert_eq!(disabled_on_path(&env, &phone), vec![Some(true)]);
    assert_eq!(
        send_refusal(&env, phone.canister_id, phone.account_principal, 1),
        Some(NotAcceptedReason::NoChannel)
    );
}

#[test]
fn should_keep_safari_muted_once_the_disabled_linked_installed_apps_session_has_ended() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    end_the_installed_apps_session(&env, &phone);

    assert_eq!(
        send_refusal(&env, phone.canister_id, phone.account_principal, 1),
        Some(NotAcceptedReason::NoChannel)
    );
}

#[test]
fn should_keep_a_disabled_installed_app_whose_session_ended_when_another_browser_subscribes() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));
    end_the_installed_apps_session(&env, &phone);

    // The same identity on another browser writes to the same config.
    let other_browser_session = session_of(&sign_in(
        &env,
        phone.canister_id,
        principal_1(),
        session_request(phone.anchor, APP, &BrowserKey::new(3)),
    ));
    subscribe_pwa_webpush(
        &env,
        phone.canister_id,
        other_browser_session,
        OTHER_PWA_ENDPOINT,
        p256dh(11),
        &OTHER_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");

    // Safari stays muted: only the other browser's installed app is woken.
    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![OTHER_PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_keep_a_disabled_installed_app_its_push_service_calls_gone() {
    let env = env();
    let phone = shared_entry_phone(&env);
    // Reads the sender list and probes the relay, so the next wake-up is posted at once.
    deliver(&env, phone.canister_id, phone.account_principal, 1, 201);
    send_accepted(&env, phone.canister_id, phone.account_principal, 2, 201);
    let wake_up = hold_wake_up(&env);
    assert_eq!(wake_up.url, PWA_ENDPOINT);
    assert_eq!(disable(&env, &phone), Ok(()));

    reply(&env, &wake_up, 410, vec![]);
    answer_outcalls(&env, 201);

    assert_eq!(disabled_on_path(&env, &phone), vec![Some(true)]);
    assert_eq!(
        send_refusal(&env, phone.canister_id, phone.account_principal, 3),
        Some(NotAcceptedReason::NoChannel)
    );
}

#[test]
fn should_keep_a_disabled_installed_app_disabled_when_it_removes_its_subscription(
) -> Result<(), RejectResponse> {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    // What signing out of the installed app sends.
    assert_eq!(
        app_set_pwa_webpush_subscription(
            &env,
            phone.canister_id,
            phone.pwa,
            AppSetPwaWebPushSubscriptionRequest {
                subscription: None,
                frontend_origin: None,
            },
        )?,
        Ok(())
    );

    assert_eq!(disabled_on_path(&env, &phone), vec![Some(true)]);
    assert_eq!(
        send_refusal(&env, phone.canister_id, phone.account_principal, 1),
        Some(NotAcceptedReason::NoChannel)
    );
    Ok(())
}

// ---- The installed app's status ----

#[test]
fn should_read_disabled_once_the_installed_app_disables() {
    let env = env();
    let phone = linked_phone(&env);
    assert!(matches!(
        pwa_status(&env, phone.canister_id, phone.pwa),
        PwaWebPushSubscriptionStatus::Subscribed(_)
    ));

    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(
        pwa_status(&env, phone.canister_id, phone.pwa),
        PwaWebPushSubscriptionStatus::Disabled
    );
}

#[test]
fn should_read_disabled_for_a_disabled_installed_app_that_removes_its_subscription(
) -> Result<(), RejectResponse> {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(
        app_set_pwa_webpush_subscription(
            &env,
            phone.canister_id,
            phone.pwa,
            AppSetPwaWebPushSubscriptionRequest {
                subscription: None,
                frontend_origin: None,
            },
        )?,
        Ok(())
    );

    assert_eq!(
        pwa_status(&env, phone.canister_id, phone.pwa),
        PwaWebPushSubscriptionStatus::Disabled
    );
    Ok(())
}

#[test]
fn should_read_disabled_for_an_installed_app_disabled_while_awaiting_its_set() {
    let env = env();
    let phone = phone_with(&env, prepare);
    assert_eq!(
        pwa_status(&env, phone.canister_id, phone.pwa),
        PwaWebPushSubscriptionStatus::NotSubscribed
    );

    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(
        pwa_status(&env, phone.canister_id, phone.pwa),
        PwaWebPushSubscriptionStatus::Disabled
    );
}

#[test]
fn should_read_not_subscribed_for_another_account_on_the_disabled_installed_apps_browser() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    let other_account_session = another_accounts_installed_app_session(&env, &phone);

    assert_eq!(
        pwa_status(&env, phone.canister_id, other_account_session),
        PwaWebPushSubscriptionStatus::NotSubscribed
    );
    assert_eq!(
        pwa_status(&env, phone.canister_id, phone.pwa),
        PwaWebPushSubscriptionStatus::Disabled
    );
}

#[test]
fn should_read_subscribed_once_the_disabled_installed_app_prepares_and_subscribes_again() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    prepare(&env, phone.canister_id, phone.pwa);
    assert_eq!(
        pwa_status(&env, phone.canister_id, phone.pwa),
        PwaWebPushSubscriptionStatus::NotSubscribed
    );

    subscribe_pwa_webpush(
        &env,
        phone.canister_id,
        phone.pwa,
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    assert_eq!(
        pwa_status(&env, phone.canister_id, phone.pwa),
        PwaWebPushSubscriptionStatus::Subscribed(WebPushSubscriptionStatus {
            endpoint: PWA_ENDPOINT.to_string(),
            unanswered_pushes: None,
            probe_sent_at_ns: None,
        })
    );
}

// ---- The installed apps path ----

#[test]
fn should_list_the_linked_installed_app_as_disabled_once_it_is() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disabled_on_path(&env, &phone), vec![Some(false)]);

    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(disabled_on_path(&env, &phone), vec![Some(true)]);
}

#[test]
fn should_list_a_disabled_installed_app_sharing_the_callers_entry() {
    let env = env();
    let phone = shared_entry_phone(&env);
    assert_eq!(disabled_on_path(&env, &phone), Vec::<Option<bool>>::new());

    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(disabled_on_path(&env, &phone), vec![Some(true)]);
}

#[test]
fn should_probe_no_disabled_installed_app() -> Result<(), RejectResponse> {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    assert_eq!(
        probe(&env, &phone, installed_apps(&phone)),
        Err(ProbeNotificationPathError::NotReachable)
    );
    assert_eq!(
        app_probe_pwa_webpush_subscription(
            &env,
            phone.canister_id,
            phone.pwa,
            AppProbePwaWebPushSubscriptionRequest {
                frontend_origin: None
            }
        )?,
        Err(AppProbePwaWebPushSubscriptionError::NotReachable)
    );
    assert_eq!(answer_outcalls(&env, 201), Vec::<String>::new());
    Ok(())
}

#[test]
fn should_remove_the_disabled_linked_installed_app_without_a_probe_and_wake_safari_again() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    remove_unanswered_of_frontend(&env, &phone, None);

    assert_eq!(disabled_on_path(&env, &phone), Vec::<Option<bool>>::new());
    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_remove_the_disabled_installed_app_sharing_the_callers_entry_and_wake_it_again() {
    let env = env();
    let phone = shared_entry_phone(&env);
    assert_eq!(disable(&env, &phone), Ok(()));

    remove_unanswered_of_frontend(&env, &phone, None);

    assert_eq!(disabled_on_path(&env, &phone), Vec::<Option<bool>>::new());
    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_keep_an_enabled_installed_app_sharing_the_callers_entry_when_removing() {
    let env = env();
    let phone = shared_entry_phone(&env);

    remove_unanswered_of_frontend(&env, &phone, None);

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}
