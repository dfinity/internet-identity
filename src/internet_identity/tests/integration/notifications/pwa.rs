//! An app's own installed web app: subscribing through the app's session, taking an
//! endpoint over, where a notification is woken, what clears a subscription, and which
//! inbox entries the app's session reaches.

use super::*;
use canister_tests::api::internet_identity::api_v2::{
    app_revoke_session, forget_applications, revoke_browser_sessions, update_account,
};
use canister_tests::api::internet_identity::notifications::{
    app_get_pwa_notification, app_prepare_pwa_webpush_subscription, app_remove_pwa_notification,
    app_send_notification, app_set_pwa_webpush_subscription, subscribe_pwa_webpush,
    subscribe_webpush, TEST_AUTH_SECRET,
};
use canister_tests::framework::{get_metrics, II_WASM_PREVIOUS};
use internet_identity_interface::internet_identity::types::{
    AccountUpdate, AppGetPwaNotificationError, AppGetPwaNotificationRequest,
    AppPreparePwaWebPushSubscriptionError, AppPreparePwaWebPushSubscriptionResult,
    AppRemovePwaNotificationError, AppRemovePwaNotificationRequest,
    AppSetPwaWebPushSubscriptionError, AppSetPwaWebPushSubscriptionRequest, NotAcceptedReason,
    Notification, PrepareAccountSessionResponse, PwaNotification, RevokeBrowserSessionsRequest,
    SendNotificationArg, SetWebPushSubscriptionRequest,
};
use pocket_ic::common::rest::CanisterHttpMethod;
use std::time::Duration;

/// An app whose sender list names [`app_sender`].
const APP: &str = "https://listed-app.example";
/// A second app whose sender list names [`app_sender`].
const OTHER_APP: &str = "https://listed-other.example";
const PWA_ENDPOINT: &str = "https://pwa.push.example/device";
const BROWSER_ENDPOINT: &str = "https://browser.push.example/device";
/// An endpoint on Firebase Cloud Messaging, which the delivery funnel labels `chrome`.
const FCM_ENDPOINT: &str = "https://fcm.googleapis.com/fcm/send/device";
const MINUTE_NS: u64 = 60 * 1_000_000_000;
/// The first entry of a browser's inbox, which every flow here wakes for first.
const FIRST_ENTRY: InboxNotificationId = 0;
/// The `auth` secret of a device other than the one [`TEST_AUTH_SECRET`] stands for.
const OTHER_AUTH_SECRET: [u8; 16] = [5u8; 16];
/// Rounds to answer outcalls for: enough for a sender list fetch, a relay probe, a VAPID
/// signature and the post that follows.
const OUTCALL_ROUNDS: usize = 40;
/// How far the clock moves per round: a dispatch pass runs at most once a second, and the
/// rounds together stay well inside the minute the routing tests measure against.
const ROUND_MILLIS: u64 = 250;
/// How long before a session ends the routing tests deliver: inside the minute the
/// installed app is no longer woken in, with room for the rounds of one delivery.
const BEFORE_END_NS: u64 = 45 * 1_000_000_000;

/// The canister both apps list as their sender.
fn app_sender() -> Principal {
    Principal::from_slice(&[0xab, 0x01])
}

/// A subscription's own public key, an uncompressed SEC1 point on P-256, minted from `seed`.
fn p256dh(seed: u8) -> ByteBuf {
    use p256::elliptic_curve::sec1::ToEncodedPoint;
    let key = p256::SecretKey::from_bytes(&[seed; 32].into()).expect("bad scalar");
    ByteBuf::from(key.public_key().to_encoded_point(false).as_bytes().to_vec())
}

/// What signing `anchor` in at `origin` from `browser` asks for, with the default
/// session bounds.
fn session_request(
    anchor: AnchorNumber,
    origin: &str,
    browser: &BrowserKey,
) -> PrepareAccountSessionRequest {
    let session_key = ByteBuf::from(vec![1; 32]);
    let next_browser_key = browser.successor().public_key();
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
    }
}

/// Signs in with `request`, as `owner`.
fn sign_in(
    env: &PocketIc,
    canister_id: CanisterId,
    owner: Principal,
    request: PrepareAccountSessionRequest,
) -> PrepareAccountSessionResponse {
    prepare_account_session(env, canister_id, owner, request)
        .expect("prepare_account_session rejected")
        .expect("prepare_account_session refused")
}

fn session_of(signed_in: &PrepareAccountSessionResponse) -> Principal {
    Principal::self_authenticating(&signed_in.user_key)
}

/// A canister that notifies, and `principal_1`'s identity signed in at [`APP`] from the
/// browser seed 1 names, which allowed the app and subscribed for itself at
/// [`BROWSER_ENDPOINT`]. `valid_for` and `max_idle` bound the session.
fn install_signed_in(
    env: &PocketIc,
    valid_for: Option<u64>,
    max_idle: Option<u64>,
) -> (CanisterId, AnchorNumber, PrepareAccountSessionResponse) {
    let canister_id =
        install_ii_canister_with_arg(env, II_WASM.clone(), arg_with_notifications_enabled());
    let anchor = flows::register_anchor(env, canister_id);
    let browser = BrowserKey::new(1);
    let signed_in = sign_in(
        env,
        canister_id,
        principal_1(),
        PrepareAccountSessionRequest {
            valid_for,
            max_idle,
            ..session_request(anchor, APP, &browser)
        },
    );
    grant_consent(env, canister_id, principal_1(), anchor, APP.into())
        .expect("grant rejected")
        .expect("grant refused");
    subscribe_webpush(
        env,
        canister_id,
        browser.successor().principal(),
        SetWebPushSubscriptionRequest {
            anchor_number: anchor,
            endpoint: BROWSER_ENDPOINT.to_string(),
            p256dh: p256dh(7),
            transport_key: p256dh(7),
            sealed_auth: ByteBuf::from(vec![0u8; 32]),
        },
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    (canister_id, anchor, signed_in)
}

/// [`install_signed_in`] with a session of the defaults, whose installed web app holds
/// [`PWA_ENDPOINT`] with `p256dh(9)` and [`TEST_AUTH_SECRET`].
fn install_subscribed(env: &PocketIc) -> (CanisterId, AnchorNumber, PrepareAccountSessionResponse) {
    let (canister_id, anchor, signed_in) = install_signed_in(env, None, None);
    subscribe_pwa_webpush(
        env,
        canister_id,
        session_of(&signed_in),
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    (canister_id, anchor, signed_in)
}

/// A second identity, `principal_2`'s, signed in at `origin` from the browser seed 2
/// names, allowing it.
fn second_identity_at(
    env: &PocketIc,
    canister_id: CanisterId,
    origin: &str,
) -> PrepareAccountSessionResponse {
    let anchor = flows::register_anchor_with_device(env, canister_id, &device_data_2());
    let signed_in = sign_in(
        env,
        canister_id,
        principal_2(),
        session_request(anchor, origin, &BrowserKey::new(2)),
    );
    grant_consent(env, canister_id, principal_2(), anchor, origin.into())
        .expect("grant rejected")
        .expect("grant refused");
    signed_in
}

/// Answers every outcall for [`OUTCALL_ROUNDS`] rounds: a sender list with
/// [`app_sender`], `push_status` to every push, and any other `GET`, a relay probe, with
/// `200`. Answers the URLs pushed to.
fn answer_outcalls(env: &PocketIc, push_status: u16) -> Vec<String> {
    let mut pushed = Vec::new();
    for _ in 0..OUTCALL_ROUNDS {
        env.advance_time(Duration::from_millis(ROUND_MILLIS));
        env.tick();
        for request in env.get_canister_http() {
            let (status, body) = if request.url.starts_with("https://listed") {
                (
                    200,
                    format!(r#"{{"senders":["{}"]}}"#, app_sender().to_text()).into_bytes(),
                )
            } else if request.http_method == CanisterHttpMethod::POST {
                pushed.push(request.url.clone());
                (push_status, vec![])
            } else {
                (200, vec![])
            };
            env.mock_canister_http_response(MockCanisterHttpResponse {
                subnet_id: request.subnet_id,
                request_id: request.request_id,
                response: CanisterHttpResponse::CanisterHttpReply(CanisterHttpReply {
                    status,
                    headers: vec![],
                    body,
                }),
                additional_responses: vec![],
            });
        }
    }
    pushed
}

/// Sends [`APP`]'s notification `id` to `recipient` and runs the dispatch, answering the
/// URLs it pushed to.
fn deliver(
    env: &PocketIc,
    canister_id: CanisterId,
    recipient: Principal,
    id: u64,
    push_status: u16,
) -> Vec<String> {
    let send = || {
        app_send_notification(
            env,
            canister_id,
            app_sender(),
            SendNotificationArg {
                origin: APP.to_string(),
                notifications: vec![Notification {
                    id,
                    recipient,
                    expires_at: None,
                    urgency: None,
                }],
            },
        )
        .expect("send rejected")
        .expect("send refused")
    };
    let first = send();
    let is_deferred = first
        .not_accepted
        .iter()
        .any(|refused| matches!(refused.reason, NotAcceptedReason::Deferred { .. }));
    if is_deferred {
        answer_outcalls(env, push_status);
        assert_eq!(send().not_accepted, vec![]);
    } else {
        assert_eq!(first.not_accepted, vec![]);
    }
    answer_outcalls(env, push_status)
}

/// Moves the clock on to `time_ns`.
fn advance_to(env: &PocketIc, time_ns: u64) {
    let now_ns = env.get_time().as_nanos_since_unix_epoch();
    assert!(
        now_ns < time_ns,
        "the deliveries took the clock past {time_ns}"
    );
    env.advance_time(Duration::from_nanos(time_ns - now_ns));
}

fn get_entry(
    env: &PocketIc,
    canister_id: CanisterId,
    session: Principal,
) -> Option<PwaNotification> {
    app_get_pwa_notification(
        env,
        canister_id,
        session,
        AppGetPwaNotificationRequest {
            inbox_notification_id: FIRST_ENTRY,
        },
    )
    .expect("get rejected")
    .expect("get refused")
    .notification
}

fn remove_entry(env: &PocketIc, canister_id: CanisterId, session: Principal) {
    app_remove_pwa_notification(
        env,
        canister_id,
        session,
        AppRemovePwaNotificationRequest {
            inbox_notification_id: FIRST_ENTRY,
            not_shown: None,
        },
    )
    .expect("remove rejected")
    .expect("remove refused");
}

/// Whether `session` of another identity can take [`PWA_ENDPOINT`] with other keys, which
/// only an endpoint no subscription holds lets it.
fn is_endpoint_free(env: &PocketIc, canister_id: CanisterId, session: Principal) -> bool {
    match subscribe_pwa_webpush(
        env,
        canister_id,
        session,
        PWA_ENDPOINT,
        p256dh(11),
        &OTHER_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    {
        Ok(()) => true,
        Err(AppSetPwaWebPushSubscriptionError::EndpointInUse) => false,
        Err(err) => panic!("subscribe refused: {err:?}"),
    }
}

#[test]
fn should_refuse_every_caller_without_a_live_app_session() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, _, signed_in) = install_subscribed(&env);

    // The identity's own principal, its browser key, a stranger and anonymous hold no app
    // session.
    for stranger in [
        principal_1(),
        BrowserKey::new(1).successor().principal(),
        principal_2(),
        Principal::anonymous(),
    ] {
        assert_eq!(
            app_prepare_pwa_webpush_subscription(&env, canister_id, stranger)?,
            AppPreparePwaWebPushSubscriptionResult::Err(
                AppPreparePwaWebPushSubscriptionError::NoSuchSession
            )
        );
        assert_eq!(
            app_set_pwa_webpush_subscription(
                &env,
                canister_id,
                stranger,
                AppSetPwaWebPushSubscriptionRequest { subscription: None }
            )?,
            Err(AppSetPwaWebPushSubscriptionError::NoSuchSession)
        );
        assert_eq!(
            app_get_pwa_notification(
                &env,
                canister_id,
                stranger,
                AppGetPwaNotificationRequest {
                    inbox_notification_id: FIRST_ENTRY
                }
            )?,
            Err(AppGetPwaNotificationError::NoSuchSession)
        );
        assert_eq!(
            app_remove_pwa_notification(
                &env,
                canister_id,
                stranger,
                AppRemovePwaNotificationRequest {
                    inbox_notification_id: FIRST_ENTRY,
                    not_shown: None
                }
            )?,
            Err(AppRemovePwaNotificationError::NoSuchSession)
        );
    }
    // None of them removed the subscription.
    assert_eq!(
        deliver(&env, canister_id, signed_in.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_refuse_every_method_while_notifications_are_disabled() -> Result<(), RejectResponse> {
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
        app_prepare_pwa_webpush_subscription(&env, canister_id, session)?,
        AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::InternalCanisterError(_)
        )
    ));
    assert!(matches!(
        app_set_pwa_webpush_subscription(
            &env,
            canister_id,
            session,
            AppSetPwaWebPushSubscriptionRequest { subscription: None }
        )?,
        Err(AppSetPwaWebPushSubscriptionError::InternalCanisterError(_))
    ));
    assert!(matches!(
        app_get_pwa_notification(
            &env,
            canister_id,
            session,
            AppGetPwaNotificationRequest {
                inbox_notification_id: FIRST_ENTRY
            }
        )?,
        Err(AppGetPwaNotificationError::InternalCanisterError(_))
    ));
    assert!(matches!(
        app_remove_pwa_notification(
            &env,
            canister_id,
            session,
            AppRemovePwaNotificationRequest {
                inbox_notification_id: FIRST_ENTRY,
                not_shown: None
            }
        )?,
        Err(AppRemovePwaNotificationError::InternalCanisterError(_))
    ));
    Ok(())
}

#[test]
fn should_refuse_to_subscribe_an_app_the_identity_has_not_allowed() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_notifications_enabled());
    let anchor = flows::register_anchor(&env, canister_id);
    let session = session_of(&sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &BrowserKey::new(1)),
    ));

    assert_eq!(
        app_prepare_pwa_webpush_subscription(&env, canister_id, session)?,
        AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::NotificationsNotAllowed
        )
    );
    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            session,
            PWA_ENDPOINT,
            p256dh(9),
            &TEST_AUTH_SECRET
        )?,
        Err(AppSetPwaWebPushSubscriptionError::NotificationsNotAllowed)
    );
    // Removing what is not there needs no consent and is already done.
    assert_eq!(
        app_set_pwa_webpush_subscription(
            &env,
            canister_id,
            session,
            AppSetPwaWebPushSubscriptionRequest { subscription: None }
        )?,
        Ok(())
    );
    Ok(())
}

#[test]
fn should_wake_the_installed_app_while_its_session_is_live() {
    let env = env();
    let (canister_id, _, signed_in) = install_subscribed(&env);

    assert_eq!(
        deliver(&env, canister_id, signed_in.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_wake_the_browser_instead_once_the_session_expires_within_a_minute() {
    let env = env();
    let signed_in_at_ns = env.get_time().as_nanos_since_unix_epoch();
    let (canister_id, _, signed_in) = install_signed_in(&env, Some(20 * MINUTE_NS), None);
    subscribe_pwa_webpush(
        &env,
        canister_id,
        session_of(&signed_in),
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    assert_eq!(
        deliver(&env, canister_id, signed_in.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );

    advance_to(&env, signed_in_at_ns + 20 * MINUTE_NS - BEFORE_END_NS);
    assert_eq!(
        deliver(&env, canister_id, signed_in.account_principal, 2, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_wake_the_browser_instead_once_the_session_goes_idle_within_a_minute() {
    let env = env();
    let signed_in_at_ns = env.get_time().as_nanos_since_unix_epoch();
    let (canister_id, _, signed_in) = install_signed_in(&env, None, Some(10 * MINUTE_NS));
    subscribe_pwa_webpush(
        &env,
        canister_id,
        session_of(&signed_in),
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    assert_eq!(
        deliver(&env, canister_id, signed_in.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );

    advance_to(&env, signed_in_at_ns + 10 * MINUTE_NS - BEFORE_END_NS);
    assert_eq!(
        deliver(&env, canister_id, signed_in.account_principal, 2, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_wake_the_browser_instead_once_the_app_signed_out() {
    let env = env();
    let (canister_id, _, signed_in) = install_subscribed(&env);

    app_revoke_session(&env, canister_id, session_of(&signed_in))
        .expect("sign-out rejected")
        .expect("sign-out refused");

    assert_eq!(
        deliver(&env, canister_id, signed_in.account_principal, 1, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_take_an_endpoint_over_with_the_same_keys_for_the_same_app() {
    let env = env();
    let (canister_id, _, first) = install_subscribed(&env);
    let second = second_identity_at(&env, canister_id, APP);

    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            session_of(&second),
            PWA_ENDPOINT,
            p256dh(9),
            &TEST_AUTH_SECRET
        ),
        Ok(Ok(()))
    );

    assert_eq!(
        deliver(&env, canister_id, second.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );
    // The identity it was taken from is woken where it was before it had an app.
    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 2, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_refuse_an_endpoint_held_with_other_keys_and_leave_its_holder() {
    let env = env();
    let (canister_id, _, first) = install_subscribed(&env);
    let second = second_identity_at(&env, canister_id, APP);

    // Another `auth`, then another `p256dh`.
    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            session_of(&second),
            PWA_ENDPOINT,
            p256dh(9),
            &OTHER_AUTH_SECRET
        ),
        Ok(Err(AppSetPwaWebPushSubscriptionError::EndpointInUse))
    );
    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            session_of(&second),
            PWA_ENDPOINT,
            p256dh(11),
            &TEST_AUTH_SECRET
        ),
        Ok(Err(AppSetPwaWebPushSubscriptionError::EndpointInUse))
    );

    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_refuse_an_endpoint_held_for_another_app_and_leave_its_holder() {
    let env = env();
    let (canister_id, _, first) = install_subscribed(&env);
    let other_app = second_identity_at(&env, canister_id, OTHER_APP);

    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            session_of(&other_app),
            PWA_ENDPOINT,
            p256dh(9),
            &TEST_AUTH_SECRET
        ),
        Ok(Err(AppSetPwaWebPushSubscriptionError::EndpointInUse))
    );

    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_free_the_endpoint_when_the_app_removes_its_subscription() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, _, first) = install_subscribed(&env);
    let second = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second));

    app_set_pwa_webpush_subscription(
        &env,
        canister_id,
        session_of(&first),
        AppSetPwaWebPushSubscriptionRequest { subscription: None },
    )?
    .expect("remove refused");

    assert!(is_endpoint_free(&env, canister_id, second));
    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 1, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_drop_the_subscription_when_notifications_are_denied() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first) = install_subscribed(&env);
    let second = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second));

    revoke_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("revoke refused");

    assert!(is_endpoint_free(&env, canister_id, second));
    // Allowed again, the app is woken in the browser until it subscribes again.
    grant_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("grant refused");
    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 1, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_drop_the_subscription_when_the_app_is_forgotten() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, _) = install_subscribed(&env);
    let second = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second));

    forget_applications(&env, canister_id, principal_1(), anchor, vec![APP.into()])?
        .expect("forget refused");

    assert!(is_endpoint_free(&env, canister_id, second));
    Ok(())
}

#[test]
fn should_drop_the_subscription_when_its_browser_is_signed_out() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first) = install_subscribed(&env);
    let second = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second));

    revoke_browser_sessions(
        &env,
        canister_id,
        principal_1(),
        RevokeBrowserSessionsRequest {
            identity_number: anchor,
            browser_id: first.browser_id,
        },
    )?
    .expect("sign-out refused");

    assert!(is_endpoint_free(&env, canister_id, second));
    Ok(())
}

#[test]
fn should_drop_the_subscription_when_its_browser_is_evicted() {
    let env = env();
    let (canister_id, anchor, _) = install_subscribed(&env);
    let second = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second));

    // Twenty browsers more than the one the app is installed on, which is the least
    // recently used and the one the registry gives up.
    for seed in 20..40 {
        env.advance_time(Duration::from_secs(1));
        sign_in(
            &env,
            canister_id,
            principal_1(),
            session_request(anchor, APP, &BrowserKey::new(seed)),
        );
    }

    assert!(is_endpoint_free(&env, canister_id, second));
}

#[test]
fn should_drop_the_subscription_its_push_service_calls_gone() {
    let env = env();
    let (canister_id, _, first) = install_subscribed(&env);
    let second = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second));

    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 1, 410),
        vec![PWA_ENDPOINT.to_string()]
    );

    assert!(is_endpoint_free(&env, canister_id, second));
}

#[test]
fn should_hand_the_entry_only_to_its_apps_session_on_its_browser() {
    let env = env();
    let (canister_id, anchor, first) = install_subscribed(&env);
    let session = session_of(&first);
    // The same identity at another app on the same browser, which has rotated its key.
    let other_app = second_app_on_the_same_browser(&env, canister_id, anchor);
    // The same identity at the same app on another browser.
    let other_browser = session_of(&sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &BrowserKey::new(3)),
    ));
    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );

    let expected = Some(PwaNotification {
        canister_id: app_sender(),
        id: 1,
    });
    assert_eq!(get_entry(&env, canister_id, session), expected);
    assert_eq!(get_entry(&env, canister_id, other_app), None);
    assert_eq!(get_entry(&env, canister_id, other_browser), None);

    // Removing from either of the others leaves the entry where it is.
    remove_entry(&env, canister_id, other_app);
    remove_entry(&env, canister_id, other_browser);
    assert_eq!(get_entry(&env, canister_id, session), expected);

    remove_entry(&env, canister_id, session);
    assert_eq!(get_entry(&env, canister_id, session), None);
}

/// `principal_1`'s identity signed in at [`OTHER_APP`] from the browser seed 1 names,
/// whose key rotated on its first sign-in.
fn second_app_on_the_same_browser(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor: AnchorNumber,
) -> Principal {
    session_of(&sign_in(
        env,
        canister_id,
        principal_1(),
        session_request(anchor, OTHER_APP, &BrowserKey::new(1).successor()),
    ))
}

#[test]
fn should_keep_the_subscription_and_its_endpoint_across_an_upgrade() {
    let env = env();
    let (canister_id, _, first) = install_subscribed(&env);

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let second = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second));
    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_drop_the_entry_a_failed_wake_up_announced() {
    let env = env();
    let (canister_id, _, first) = install_subscribed(&env);

    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 1, 500),
        vec![PWA_ENDPOINT.to_string()]
    );

    // No wake-up is coming for it, so nothing is left for the app to read.
    assert_eq!(get_entry(&env, canister_id, session_of(&first)), None);
    // The subscription stays: only a gone relay drops it.
    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 2, 200),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_refuse_an_endpoint_taken_over_from_a_slot_a_rollback_emptied(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first) = install_subscribed(&env);
    // The previous release decodes the config without its installed app subscriptions and
    // writes it back without them, which leaves the endpoint indexed for an empty slot.
    upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());
    revoke_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("revoke refused");
    grant_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("grant refused");
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    // Nothing holds the endpoint any more, so another identity takes it.
    let second = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(is_endpoint_free(&env, canister_id, second));

    // It is that identity's now, which keys other than its own cannot take.
    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            session_of(&first),
            PWA_ENDPOINT,
            p256dh(9),
            &TEST_AUTH_SECRET
        )?,
        Err(AppSetPwaWebPushSubscriptionError::EndpointInUse)
    );
    Ok(())
}

#[test]
fn should_keep_waking_the_installed_app_once_its_account_is_named() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first) = install_subscribed(&env);

    // Naming the unreserved default account gives it a number, under the same principal.
    update_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        APP.into(),
        None,
        AccountUpdate {
            name: Some("Main".to_string()),
        },
    )?
    .expect("naming the account refused");

    assert_eq!(
        deliver(&env, canister_id, first.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );
    // The endpoint is still held, under the number the account was given.
    let second = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second));
    Ok(())
}

#[test]
fn should_count_an_installed_app_delivery_under_the_path_it_was_posted_by() {
    let env = env();
    // The browser's own subscription posts by a relay counted as `other`, and the
    // installed app's by Firebase Cloud Messaging, counted as `chrome`.
    let (canister_id, _, signed_in) = install_signed_in(&env, None, None);
    let session = session_of(&signed_in);
    subscribe_pwa_webpush(
        &env,
        canister_id,
        session,
        FCM_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    assert_eq!(
        deliver(&env, canister_id, signed_in.account_principal, 1, 200),
        vec![FCM_ENDPOINT.to_string()]
    );

    remove_entry(&env, canister_id, session);

    assert_eq!(
        delivered_pushes(&get_metrics(&env, canister_id), "chrome"),
        1.0
    );
    assert_eq!(
        delivered_pushes(&get_metrics(&env, canister_id), "other"),
        0.0
    );
}

/// The pushes counted delivered on `path`, over every other label.
fn delivered_pushes(metrics: &str, path: &str) -> f64 {
    let path_label = format!(r#"path="{path}""#);
    metrics
        .lines()
        .filter(|line| {
            line.starts_with(r#"internet_identity_notification_pushes{stage="delivered""#)
        })
        .filter(|line| line.contains(&path_label))
        .filter_map(|line| line.split_whitespace().nth(1)?.parse::<f64>().ok())
        .sum()
}
