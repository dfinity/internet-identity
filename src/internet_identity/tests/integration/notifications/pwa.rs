//! An app's own installed web app: subscribing through the app's session, taking an
//! endpoint over, where a notification is woken, what clears a subscription, and which
//! inbox entries the app's session reaches.

use super::*;
use canister_tests::api::internet_identity::api_v2::{
    app_revoke_session, create_account, forget_applications, revoke_browser_sessions,
    update_account,
};
use canister_tests::api::internet_identity::notifications::{
    app_get_pwa_notification, app_get_pwa_webpush_subscription_status,
    app_prepare_pwa_webpush_subscription, app_remove_pwa_notification, app_send_notification,
    app_set_pwa_webpush_subscription, prepare_notification_delegation, subscribe_pwa_webpush,
    subscribe_webpush, TEST_AUTH_SECRET,
};
use canister_tests::framework::{get_metrics, II_WASM_PREVIOUS};
use internet_identity_interface::internet_identity::types::{
    AccountUpdate, AppGetPwaNotificationError, AppGetPwaNotificationRequest,
    AppGetPwaWebPushSubscriptionStatusError, AppPreparePwaWebPushSubscriptionError,
    AppPreparePwaWebPushSubscriptionResult, AppRemovePwaNotificationError,
    AppRemovePwaNotificationRequest, AppSetPwaWebPushSubscriptionError,
    AppSetPwaWebPushSubscriptionRequest, NotAcceptedReason, Notification,
    NotificationDelegationError, PrepareAccountSessionResponse,
    PrepareNotificationDelegationRequest, PwaNotification, RevokeBrowserSessionsRequest,
    SendNotificationArg, SetWebPushSubscriptionRequest, WebPushSubscriptionStatus,
};
use pocket_ic::common::rest::{CanisterHttpMethod, CanisterHttpRequest};
use std::time::Duration;

/// An app whose sender list names [`app_sender`].
const APP: &str = "https://listed-app.example";
/// A second app whose sender list names [`app_sender`].
const OTHER_APP: &str = "https://listed-other.example";
const PWA_ENDPOINT: &str = "https://pwa.push.example/device";
const BROWSER_ENDPOINT: &str = "https://browser.push.example/device";
/// An installed app endpoint at a relay no test posts to before it subscribes there.
const UNPROBED_PWA_ENDPOINT: &str = "https://unprobed.push.example/device";
/// An endpoint on Mozilla's push service, which the delivery funnel labels `firefox`.
const MOZILLA_ENDPOINT: &str = "https://updates.push.services.mozilla.com/wpush/v2/device";
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

fn session_of(account_session: &PrepareAccountSessionResponse) -> Principal {
    Principal::self_authenticating(&account_session.user_key)
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
    let account_session = sign_in(
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
    (canister_id, anchor, account_session)
}

/// [`install_signed_in`] with a session of the defaults, whose installed web app holds
/// [`PWA_ENDPOINT`] with `p256dh(9)` and [`TEST_AUTH_SECRET`].
fn install_subscribed(env: &PocketIc) -> (CanisterId, AnchorNumber, PrepareAccountSessionResponse) {
    let (canister_id, anchor, account_session) = install_signed_in(env, None, None);
    subscribe_pwa_webpush(
        env,
        canister_id,
        session_of(&account_session),
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    (canister_id, anchor, account_session)
}

/// A second identity, `principal_2`'s, signed in at `origin` from the browser seed 2
/// names, allowing it.
fn second_identity_at(
    env: &PocketIc,
    canister_id: CanisterId,
    origin: &str,
) -> PrepareAccountSessionResponse {
    let anchor = flows::register_anchor_with_device(env, canister_id, &device_data_2());
    let account_session = sign_in(
        env,
        canister_id,
        principal_2(),
        session_request(anchor, origin, &BrowserKey::new(2)),
    );
    grant_consent(env, canister_id, principal_2(), anchor, origin.into())
        .expect("grant rejected")
        .expect("grant refused");
    account_session
}

/// Answers every outcall for [`OUTCALL_ROUNDS`] rounds: a sender list with
/// [`app_sender`], `push_status` to every push, and any other `GET`, a relay probe, with
/// `200`. Answers the URLs pushed to.
fn answer_outcalls(env: &PocketIc, push_status: u16) -> Vec<String> {
    let mut pushed_urls = Vec::new();
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
                pushed_urls.push(request.url.clone());
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
    pushed_urls
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
    send_accepted(env, canister_id, recipient, id, push_status);
    answer_outcalls(env, push_status)
}

/// Sends [`APP`]'s notification `id` to `recipient` until it is accepted, answering the
/// sender list fetch that defers it.
fn send_accepted(
    env: &PocketIc,
    canister_id: CanisterId,
    recipient: Principal,
    id: u64,
    push_status: u16,
) {
    send_accepted_from(env, canister_id, APP, recipient, id, push_status);
}

/// [`send_accepted`] for the app at `origin`.
fn send_accepted_from(
    env: &PocketIc,
    canister_id: CanisterId,
    origin: &str,
    recipient: Principal,
    id: u64,
    push_status: u16,
) {
    let send = || {
        app_send_notification(
            env,
            canister_id,
            app_sender(),
            SendNotificationArg {
                origin: origin.to_string(),
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
    let first_response = send();
    let is_deferred = first_response
        .not_accepted
        .iter()
        .any(|refusal| matches!(refusal.reason, NotAcceptedReason::Deferred { .. }));
    if is_deferred {
        answer_outcalls(env, push_status);
        assert_eq!(send().not_accepted, vec![]);
    } else {
        assert_eq!(first_response.not_accepted, vec![]);
    }
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
    let (canister_id, _, account_session) = install_subscribed(&env);

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
        assert_eq!(
            app_get_pwa_webpush_subscription_status(&env, canister_id, stranger)?,
            Err(AppGetPwaWebPushSubscriptionStatusError::NoSuchSession)
        );
    }
    // None of them removed the subscription.
    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 1, 200),
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
    assert!(matches!(
        app_get_pwa_webpush_subscription_status(&env, canister_id, session)?,
        Err(AppGetPwaWebPushSubscriptionStatusError::InternalCanisterError(_))
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
    let (canister_id, _, account_session) = install_subscribed(&env);

    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_wake_the_browser_instead_once_the_session_expires_within_a_minute() {
    let env = env();
    let signed_in_at_ns = env.get_time().as_nanos_since_unix_epoch();
    let (canister_id, _, account_session) = install_signed_in(&env, Some(20 * MINUTE_NS), None);
    subscribe_pwa_webpush(
        &env,
        canister_id,
        session_of(&account_session),
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );

    advance_to(&env, signed_in_at_ns + 20 * MINUTE_NS - BEFORE_END_NS);
    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 2, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_wake_the_browser_instead_once_the_session_goes_idle_within_a_minute() {
    let env = env();
    let signed_in_at_ns = env.get_time().as_nanos_since_unix_epoch();
    let (canister_id, _, account_session) = install_signed_in(&env, None, Some(10 * MINUTE_NS));
    subscribe_pwa_webpush(
        &env,
        canister_id,
        session_of(&account_session),
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 1, 200),
        vec![PWA_ENDPOINT.to_string()]
    );

    advance_to(&env, signed_in_at_ns + 10 * MINUTE_NS - BEFORE_END_NS);
    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 2, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_wake_nothing_once_the_app_signed_out_of_the_browsers_last_session() {
    let env = env();
    let (canister_id, _, account_session) = install_subscribed(&env);

    app_revoke_session(&env, canister_id, session_of(&account_session))
        .expect("sign-out rejected")
        .expect("sign-out refused");

    // The browser's own subscription is still there, but a browser with no session left
    // is woken at nothing, so the recipient has no channel.
    assert_eq!(
        send_refusal(&env, canister_id, account_session.account_principal, 1),
        Some(NotAcceptedReason::NoChannel)
    );
}

#[test]
fn should_answer_no_channel_for_a_recipient_whose_only_browser_is_signed_out(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, account_session) = install_subscribed(&env);

    revoke_browser_sessions(
        &env,
        canister_id,
        principal_1(),
        RevokeBrowserSessionsRequest {
            identity_number: anchor,
            browser_id: account_session.browser_id,
        },
    )?
    .expect("sign-out refused");

    assert_eq!(
        send_refusal(&env, canister_id, account_session.account_principal, 1),
        Some(NotAcceptedReason::NoChannel)
    );
    Ok(())
}

/// Sends [`APP`]'s notification `id` to `recipient`, past the sender list fetch that defers
/// the first, and answers why it was not accepted, `None` where it was.
fn send_refusal(
    env: &PocketIc,
    canister_id: CanisterId,
    recipient: Principal,
    id: u64,
) -> Option<NotAcceptedReason> {
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
        .not_accepted
        .first()
        .map(|refusal| refusal.reason.clone())
    };
    match send() {
        Some(NotAcceptedReason::Deferred { .. }) => {
            answer_outcalls(env, 200);
            send()
        }
        response => response,
    }
}

#[test]
fn should_take_an_endpoint_over_with_the_same_keys_for_the_same_app() {
    let env = env();
    let (canister_id, _, first_account_session) = install_subscribed(&env);
    let second_account_session = second_identity_at(&env, canister_id, APP);

    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            session_of(&second_account_session),
            PWA_ENDPOINT,
            p256dh(9),
            &TEST_AUTH_SECRET
        ),
        Ok(Ok(()))
    );

    assert_eq!(
        deliver(
            &env,
            canister_id,
            second_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );
    // The identity it was taken from is woken where it was before it had an app.
    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            2,
            200
        ),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_refuse_an_endpoint_held_with_other_keys_and_leave_its_holder() {
    let env = env();
    let (canister_id, _, first_account_session) = install_subscribed(&env);
    let second_account_session = second_identity_at(&env, canister_id, APP);

    // Another `auth`, then another `p256dh`.
    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            session_of(&second_account_session),
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
            session_of(&second_account_session),
            PWA_ENDPOINT,
            p256dh(11),
            &TEST_AUTH_SECRET
        ),
        Ok(Err(AppSetPwaWebPushSubscriptionError::EndpointInUse))
    );

    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_refuse_an_endpoint_held_for_another_app_and_leave_its_holder() {
    let env = env();
    let (canister_id, _, first_account_session) = install_subscribed(&env);
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
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );
}

/// What `app_get_pwa_webpush_subscription_status` answers `session`.
fn pwa_status(
    env: &PocketIc,
    canister_id: CanisterId,
    session: Principal,
) -> Option<WebPushSubscriptionStatus> {
    app_get_pwa_webpush_subscription_status(env, canister_id, session)
        .expect("status rejected")
        .expect("status refused")
}

#[test]
fn should_report_the_endpoint_only_while_it_delivers_for_the_sessions_account(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_signed_in(&env, None, None);
    let first_session = session_of(&first_account_session);
    assert_eq!(pwa_status(&env, canister_id, first_session), None);

    // A prepared subscription awaiting its set is not one yet.
    let prepare_result = (0..10)
        .map(|_| app_prepare_pwa_webpush_subscription(&env, canister_id, first_session))
        .find(|prepare_result| {
            !matches!(
                prepare_result,
                Ok(AppPreparePwaWebPushSubscriptionResult::Pending)
            )
        })
        .expect("prepare stayed pending")?;
    assert!(matches!(
        prepare_result,
        AppPreparePwaWebPushSubscriptionResult::Ok(_)
    ));
    assert_eq!(pwa_status(&env, canister_id, first_session), None);

    subscribe_pwa_webpush(
        &env,
        canister_id,
        first_session,
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )?
    .expect("subscribe refused");
    assert_eq!(
        pwa_status(&env, canister_id, first_session),
        Some(WebPushSubscriptionStatus {
            endpoint: PWA_ENDPOINT.to_string(),
            unanswered_pushes: None,
        })
    );

    // The same installed app on the same browser, signed in to another account of the
    // identity, subscribes in its turn.
    let other_account = create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        APP.into(),
        "Other".to_string(),
    )?
    .expect("creating the account refused")
    .account_number;
    let other_session = session_of(&sign_in(
        &env,
        canister_id,
        principal_1(),
        PrepareAccountSessionRequest {
            account_number: other_account,
            ..session_request(anchor, APP, &BrowserKey::new(1).successor())
        },
    ));
    subscribe_pwa_webpush(
        &env,
        canister_id,
        other_session,
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )?
    .expect("subscribe refused");
    assert_eq!(pwa_status(&env, canister_id, first_session), None);
    assert_eq!(
        pwa_status(&env, canister_id, other_session),
        Some(WebPushSubscriptionStatus {
            endpoint: PWA_ENDPOINT.to_string(),
            unanswered_pushes: None,
        })
    );

    app_set_pwa_webpush_subscription(
        &env,
        canister_id,
        other_session,
        AppSetPwaWebPushSubscriptionRequest { subscription: None },
    )?
    .expect("remove refused");
    assert_eq!(pwa_status(&env, canister_id, other_session), None);
    Ok(())
}

#[test]
fn should_free_the_endpoint_when_the_app_removes_its_subscription() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, _, first_account_session) = install_subscribed(&env);
    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second_session));

    app_set_pwa_webpush_subscription(
        &env,
        canister_id,
        session_of(&first_account_session),
        AppSetPwaWebPushSubscriptionRequest { subscription: None },
    )?
    .expect("remove refused");

    assert!(is_endpoint_free(&env, canister_id, second_session));
    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![BROWSER_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_drop_the_subscription_when_notifications_are_denied() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second_session));

    revoke_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("revoke refused");

    assert!(is_endpoint_free(&env, canister_id, second_session));
    // Allowed again, the app is woken in the browser until it subscribes again.
    grant_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("grant refused");
    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![BROWSER_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_drop_the_subscription_when_the_app_is_forgotten() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, _) = install_subscribed(&env);
    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second_session));

    forget_applications(&env, canister_id, principal_1(), anchor, vec![APP.into()])?
        .expect("forget refused");

    assert!(is_endpoint_free(&env, canister_id, second_session));
    Ok(())
}

#[test]
fn should_drop_the_subscription_when_its_browser_is_signed_out() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second_session));

    revoke_browser_sessions(
        &env,
        canister_id,
        principal_1(),
        RevokeBrowserSessionsRequest {
            identity_number: anchor,
            browser_id: first_account_session.browser_id,
        },
    )?
    .expect("sign-out refused");

    assert!(is_endpoint_free(&env, canister_id, second_session));
    Ok(())
}

#[test]
fn should_drop_the_subscription_when_its_browser_is_evicted() {
    let env = env();
    let (canister_id, anchor, _) = install_subscribed(&env);
    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second_session));

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

    assert!(is_endpoint_free(&env, canister_id, second_session));
}

#[test]
fn should_drop_the_subscription_its_push_service_calls_gone() {
    let env = env();
    let (canister_id, _, first_account_session) = install_subscribed(&env);
    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second_session));

    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            410
        ),
        vec![PWA_ENDPOINT.to_string()]
    );

    // No wake-up reached the app, so the entry it announced goes too.
    assert_eq!(
        get_entry(&env, canister_id, session_of(&first_account_session)),
        None
    );
    assert!(is_endpoint_free(&env, canister_id, second_session));
}

#[test]
fn should_hand_the_entry_only_to_its_apps_session_on_its_browser() {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    let session = session_of(&first_account_session);
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
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );

    let expected_notification = Some(PwaNotification {
        canister_id: app_sender(),
        id: 1,
    });
    assert_eq!(get_entry(&env, canister_id, session), expected_notification);
    assert_eq!(get_entry(&env, canister_id, other_app), None);
    assert_eq!(get_entry(&env, canister_id, other_browser), None);

    // Removing from either of the others leaves the entry where it is.
    remove_entry(&env, canister_id, other_app);
    remove_entry(&env, canister_id, other_browser);
    assert_eq!(get_entry(&env, canister_id, session), expected_notification);

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
    let (canister_id, _, first_account_session) = install_subscribed(&env);

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second_session));
    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_drop_the_entry_a_failed_wake_up_announced() {
    let env = env();
    let (canister_id, _, first_account_session) = install_subscribed(&env);

    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            500
        ),
        vec![PWA_ENDPOINT.to_string()]
    );

    // No wake-up is coming for it, so nothing is left for the app to read.
    assert_eq!(
        get_entry(&env, canister_id, session_of(&first_account_session)),
        None
    );
    // The subscription stays: only a gone relay drops it.
    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            2,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_keep_the_entry_for_the_installed_app_when_the_browser_switches_delivery_off(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    // The browser entry the installed app shares also points at a Home Screen app.
    super::link_app(&env, canister_id, anchor, 1, 50);
    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );

    canister_tests::api::internet_identity::notifications::disable_notification_app(
        &env,
        canister_id,
        BrowserKey::new(1).successor().principal(),
        internet_identity_interface::internet_identity::types::DisableNotificationAppRequest {
            anchor_number: anchor,
        },
    )?
    .expect("switching delivery off refused");

    assert!(get_entry(&env, canister_id, session_of(&first_account_session)).is_some());
    Ok(())
}

/// The inbox moved to a layout of its own, under a key of its own: the previous release's
/// entries are skipped on the way up, and this release's on the way back down.
#[test]
fn should_read_the_identity_across_an_upgrade_and_a_rollback_with_entries_waiting(
) -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM_PREVIOUS.clone(),
        arg_with_notifications_enabled(),
    );
    let anchor = flows::register_anchor(&env, canister_id);
    let browser = BrowserKey::new(1);
    let account_session = sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &browser),
    );
    grant_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("grant refused");
    subscribe_webpush(
        &env,
        canister_id,
        browser.successor().principal(),
        SetWebPushSubscriptionRequest {
            anchor_number: anchor,
            endpoint: BROWSER_ENDPOINT.to_string(),
            p256dh: p256dh(7),
            transport_key: p256dh(7),
            sealed_auth: ByteBuf::from(vec![0u8; 32]),
        },
    )?
    .expect("subscribe refused");
    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 1, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    canister_tests::api::internet_identity::api_v2::identity_info(
        &env,
        canister_id,
        principal_1(),
        anchor,
    )?
    .expect("reading the identity after the upgrade");
    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 2, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );

    upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());
    canister_tests::api::internet_identity::api_v2::identity_info(
        &env,
        canister_id,
        principal_1(),
        anchor,
    )?
    .expect("reading the identity after the rollback");

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    canister_tests::api::internet_identity::api_v2::identity_info(
        &env,
        canister_id,
        principal_1(),
        anchor,
    )?
    .expect("reading the identity after upgrading again");
    Ok(())
}

#[test]
fn should_refuse_an_endpoint_taken_over_from_a_slot_a_rollback_emptied(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    // The previous release decodes the config without its installed app subscriptions and
    // writes it back without them, which leaves the endpoint indexed for an empty slot.
    upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());
    revoke_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("revoke refused");
    grant_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("grant refused");
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    // Nothing holds the endpoint any more, so another identity takes it.
    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(is_endpoint_free(&env, canister_id, second_session));

    // It is that identity's now, which keys other than its own cannot take.
    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            session_of(&first_account_session),
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
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);

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
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );
    // The endpoint is still held, under the number the account was given.
    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert!(!is_endpoint_free(&env, canister_id, second_session));
    Ok(())
}

#[test]
fn should_count_an_installed_app_delivery_under_the_path_it_was_posted_by() {
    let env = env();
    // The browser's own subscription posts by a relay counted as `other`, and the
    // installed app's by Firebase Cloud Messaging, counted as `chrome`.
    let (canister_id, _, account_session) = install_signed_in(&env, None, None);
    let session = session_of(&account_session);
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
        deliver(&env, canister_id, account_session.account_principal, 1, 200),
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

/// Names `principal_1`'s unreserved default account at [`APP`], which gives it a number.
fn name_default_account(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor: AnchorNumber,
) -> Result<(), RejectResponse> {
    update_account(
        env,
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
    Ok(())
}

#[test]
fn should_hand_over_an_entry_queued_before_its_account_was_named() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    let session = session_of(&first_account_session);
    // Another account of the same identity at the same app, on the same browser.
    let other_account = create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        APP.into(),
        "Other".to_string(),
    )?
    .expect("creating the account refused")
    .account_number;
    let other_session = session_of(&sign_in(
        &env,
        canister_id,
        principal_1(),
        PrepareAccountSessionRequest {
            account_number: other_account,
            ..session_request(anchor, APP, &BrowserKey::new(1).successor())
        },
    ));
    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );

    name_default_account(&env, canister_id, anchor)?;

    let expected_notification = Some(PwaNotification {
        canister_id: app_sender(),
        id: 1,
    });
    assert_eq!(get_entry(&env, canister_id, session), expected_notification);
    assert_eq!(get_entry(&env, canister_id, other_session), None);
    remove_entry(&env, canister_id, session);
    assert_eq!(get_entry(&env, canister_id, session), None);
    Ok(())
}

#[test]
fn should_wake_the_installed_app_for_a_notification_waiting_while_its_account_was_named(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    // Reads the sender list, so the next send is accepted at once.
    deliver(
        &env,
        canister_id,
        first_account_session.account_principal,
        1,
        200,
    );
    // A relay no wake-up was posted to yet, so the next one waits in the backlog for its
    // probe, which nothing answers until the account is named.
    subscribe_pwa_webpush(
        &env,
        canister_id,
        session_of(&first_account_session),
        UNPROBED_PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )?
    .expect("subscribe refused");
    send_accepted(
        &env,
        canister_id,
        first_account_session.account_principal,
        2,
        200,
    );

    name_default_account(&env, canister_id, anchor)?;

    assert_eq!(
        answer_outcalls(&env, 200),
        vec![UNPROBED_PWA_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_count_each_copy_under_the_path_it_was_posted_by() -> Result<(), RejectResponse> {
    let env = env();
    // The first browser holds the installed app on Firebase Cloud Messaging, counted as
    // `chrome`, and a subscription of its own counted as `other`.
    let (canister_id, anchor, account_session) = install_signed_in(&env, None, None);
    let session = session_of(&account_session);
    subscribe_pwa_webpush(
        &env,
        canister_id,
        session,
        FCM_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )?
    .expect("subscribe refused");
    // A second browser of the same identity, subscribed on Mozilla's push service,
    // counted as `firefox`.
    let other_browser = BrowserKey::new(3);
    let other_browser_id = sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &other_browser),
    )
    .browser_id;
    subscribe_webpush(
        &env,
        canister_id,
        other_browser.successor().principal(),
        SetWebPushSubscriptionRequest {
            anchor_number: anchor,
            endpoint: MOZILLA_ENDPOINT.to_string(),
            p256dh: p256dh(7),
            transport_key: p256dh(7),
            sealed_auth: ByteBuf::from(vec![0u8; 32]),
        },
    )?
    .expect("subscribe refused");
    let mut pushed_urls = deliver(&env, canister_id, account_session.account_principal, 1, 200);
    pushed_urls.sort();
    assert_eq!(
        pushed_urls,
        vec![FCM_ENDPOINT.to_string(), MOZILLA_ENDPOINT.to_string()]
    );

    remove_entry(&env, canister_id, session);
    browser_remove_notification(
        &env,
        canister_id,
        other_browser.successor().principal(),
        RemoveNotificationRequest {
            anchor_number: anchor,
            browser_id: other_browser_id,
            inbox_notification_id: FIRST_ENTRY,
            not_shown: None,
        },
    )?
    .expect("remove refused");

    let metrics = get_metrics(&env, canister_id);
    assert_eq!(delivered_pushes(&metrics, "chrome"), 1.0);
    assert_eq!(delivered_pushes(&metrics, "firefox"), 1.0);
    assert_eq!(delivered_pushes(&metrics, "other"), 0.0);
    Ok(())
}

/// Answers every outcall but the wake-ups, as [`answer_outcalls`] does, until one is
/// posted, and answers that one wake-up's request left unanswered.
fn hold_wake_up(env: &PocketIc) -> CanisterHttpRequest {
    for _ in 0..OUTCALL_ROUNDS {
        env.advance_time(Duration::from_millis(ROUND_MILLIS));
        env.tick();
        for request in env.get_canister_http() {
            if request.http_method == CanisterHttpMethod::POST {
                return request;
            }
            let body = match request.url.starts_with("https://listed") {
                true => format!(r#"{{"senders":["{}"]}}"#, app_sender().to_text()).into_bytes(),
                false => vec![],
            };
            reply(env, &request, 200, body);
        }
    }
    panic!("no wake-up was posted");
}

fn reply(env: &PocketIc, request: &CanisterHttpRequest, status: u16, body: Vec<u8>) {
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

#[test]
fn should_drop_the_subscription_a_wake_up_in_flight_while_its_account_was_named_finds_gone(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    let second_session = session_of(&second_identity_at(&env, canister_id, APP));
    // Reads the sender list and probes the relay, so the next wake-up is posted at once.
    deliver(
        &env,
        canister_id,
        first_account_session.account_principal,
        1,
        200,
    );
    send_accepted(
        &env,
        canister_id,
        first_account_session.account_principal,
        2,
        200,
    );
    let wake_up = hold_wake_up(&env);
    assert_eq!(wake_up.url, PWA_ENDPOINT);

    name_default_account(&env, canister_id, anchor)?;
    reply(&env, &wake_up, 404, vec![]);
    answer_outcalls(&env, 200);

    assert!(is_endpoint_free(&env, canister_id, second_session));
    Ok(())
}

#[test]
fn should_deliver_for_the_account_that_subscribed_the_installed_app_last(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    let other_account = create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        APP.into(),
        "Other".to_string(),
    )?
    .expect("creating the account refused")
    .account_number;
    // The same installed app on the same browser, signed in to the other account.
    let other_account_session = sign_in(
        &env,
        canister_id,
        principal_1(),
        PrepareAccountSessionRequest {
            account_number: other_account,
            ..session_request(anchor, APP, &BrowserKey::new(1).successor())
        },
    );
    subscribe_pwa_webpush(
        &env,
        canister_id,
        session_of(&other_account_session),
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )?
    .expect("subscribe refused");

    assert_eq!(
        deliver(
            &env,
            canister_id,
            other_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );
    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            2,
            200
        ),
        vec![BROWSER_ENDPOINT.to_string()]
    );
    // One subscription on the browser, the other account's: removing from the first
    // account's session leaves it.
    app_set_pwa_webpush_subscription(
        &env,
        canister_id,
        session_of(&first_account_session),
        AppSetPwaWebPushSubscriptionRequest { subscription: None },
    )?
    .expect("remove refused");
    assert_eq!(
        deliver(
            &env,
            canister_id,
            other_account_session.account_principal,
            3,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_drop_only_the_revoked_apps_entries() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, at_app) = install_signed_in(&env, None, None);
    // The same browser, signed in at the other app too, which it allows.
    let browser = BrowserKey::new(1).successor();
    let at_other_app = sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, OTHER_APP, &browser),
    );
    grant_consent(&env, canister_id, principal_1(), anchor, OTHER_APP.into())?
        .expect("grant refused");
    send_accepted_from(&env, canister_id, APP, at_app.account_principal, 1, 200);
    answer_outcalls(&env, 200);
    send_accepted_from(
        &env,
        canister_id,
        OTHER_APP,
        at_other_app.account_principal,
        2,
        200,
    );
    answer_outcalls(&env, 200);

    // Allowed again, so consent is not what keeps an entry from being read.
    revoke_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("revoke refused");
    grant_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("grant refused");

    let browser_key = browser.successor().principal();
    let read = |inbox_notification_id| {
        browser_get_notification(
            &env,
            canister_id,
            browser_key,
            GetNotificationRequest {
                anchor_number: anchor,
                browser_id: at_app.browser_id,
                inbox_notification_id,
            },
        )
        .expect("read rejected")
        .expect("read refused")
        .notification
        .map(|notification| notification.id)
    };
    assert_eq!(read(0), None);
    assert_eq!(read(1), Some(2));
    Ok(())
}

#[test]
fn should_leave_the_inbox_as_it_is_when_a_subscription_changes() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, first_account_session) = install_subscribed(&env);
    // The same browser at the other app, which has no installed app and is woken through
    // the browser's own subscription.
    let browser = BrowserKey::new(1).successor();
    let at_other_app = sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, OTHER_APP, &browser),
    );
    grant_consent(&env, canister_id, principal_1(), anchor, OTHER_APP.into())?
        .expect("grant refused");
    let browser_key = browser.successor().principal();
    assert_eq!(
        deliver(
            &env,
            canister_id,
            first_account_session.account_principal,
            1,
            200
        ),
        vec![PWA_ENDPOINT.to_string()]
    );
    send_accepted_from(
        &env,
        canister_id,
        OTHER_APP,
        at_other_app.account_principal,
        2,
        200,
    );
    assert_eq!(
        answer_outcalls(&env, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
    let read = |inbox_notification_id| {
        browser_get_notification(
            &env,
            canister_id,
            browser_key,
            GetNotificationRequest {
                anchor_number: anchor,
                browser_id: first_account_session.browser_id,
                inbox_notification_id,
            },
        )
        .expect("read rejected")
        .expect("read refused")
        .notification
        .map(|notification| notification.id)
    };
    let assert_both_entries_held = |read: &dyn Fn(InboxNotificationId) -> Option<u64>| {
        assert_eq!(read(0), Some(1), "the installed app's entry");
        assert_eq!(read(1), Some(2), "the browser's own entry");
    };

    // The browser subscribes again, somewhere else.
    subscribe_webpush(
        &env,
        canister_id,
        browser_key,
        SetWebPushSubscriptionRequest {
            anchor_number: anchor,
            endpoint: format!("{BROWSER_ENDPOINT}/moved"),
            p256dh: p256dh(7),
            transport_key: p256dh(7),
            sealed_auth: ByteBuf::from(vec![0u8; 32]),
        },
    )?
    .expect("subscribe refused");
    assert_both_entries_held(&read);
    assert_eq!(
        get_entry(&env, canister_id, session_of(&first_account_session)),
        Some(PwaNotification {
            canister_id: app_sender(),
            id: 1,
        })
    );

    // Its push service calls the new subscription gone.
    send_accepted_from(
        &env,
        canister_id,
        OTHER_APP,
        at_other_app.account_principal,
        3,
        404,
    );
    assert_eq!(
        answer_outcalls(&env, 404),
        vec![format!("{BROWSER_ENDPOINT}/moved")]
    );
    assert_both_entries_held(&read);

    // The identity signs the browser out, and it signs in again.
    revoke_browser_sessions(
        &env,
        canister_id,
        principal_1(),
        RevokeBrowserSessionsRequest {
            identity_number: anchor,
            browser_id: first_account_session.browser_id,
        },
    )?
    .expect("sign-out refused");
    let browser = browser.successor();
    sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &browser),
    );
    let browser_key = browser.successor().principal();
    let read_again = |inbox_notification_id| {
        browser_get_notification(
            &env,
            canister_id,
            browser_key,
            GetNotificationRequest {
                anchor_number: anchor,
                browser_id: first_account_session.browser_id,
                inbox_notification_id,
            },
        )
        .expect("read rejected")
        .expect("read refused")
        .notification
        .map(|notification| notification.id)
    };
    assert_both_entries_held(&read_again);
    Ok(())
}

#[test]
fn should_count_a_push_whose_entry_was_collected_before_its_answer() {
    let env = env();
    let (canister_id, _, first_account_session) = install_subscribed(&env);
    // Reads the sender list and probes the relay, so the next wake-up is posted at once.
    deliver(
        &env,
        canister_id,
        first_account_session.account_principal,
        1,
        200,
    );
    send_accepted(
        &env,
        canister_id,
        first_account_session.account_principal,
        2,
        200,
    );
    let wake_up = hold_wake_up(&env);

    // The app collects the entry before the push service has answered the post.
    app_remove_pwa_notification(
        &env,
        canister_id,
        session_of(&first_account_session),
        AppRemovePwaNotificationRequest {
            inbox_notification_id: 1,
            not_shown: None,
        },
    )
    .expect("remove rejected")
    .expect("remove refused");
    reply(&env, &wake_up, 200, vec![]);
    answer_outcalls(&env, 200);

    assert_eq!(pushes(&get_metrics(&env, canister_id), "pushed"), 2.0);
}

/// The pushes counted at `stage`, over every other label.
fn pushes(metrics: &str, stage: &str) -> f64 {
    let stage_label = format!(r#"stage="{stage}""#);
    metrics
        .lines()
        .filter(|line| line.starts_with("internet_identity_notification_pushes{"))
        .filter(|line| line.contains(&stage_label))
        .filter_map(|line| line.split_whitespace().nth(1)?.parse::<f64>().ok())
        .sum()
}

/// A canister that has just switched notifications on, so it is fetching its VAPID key and
/// does not hold it yet, with an identity signed in at [`APP`]: its anchor, its browser
/// key and its session there.
fn install_fetching_the_vapid_key(
    env: &PocketIc,
) -> (CanisterId, AnchorNumber, Principal, Principal) {
    // Notifications off, so nothing fetches the key yet.
    let canister_id =
        install_ii_canister_with_arg(env, II_WASM.clone(), arg_with_captcha_disabled());
    let anchor = flows::register_anchor(env, canister_id);
    let browser = BrowserKey::new(1);
    let session = session_of(&sign_in(
        env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &browser),
    ));
    // Upgraded without the helper's follow-up calls, whose rounds would let the key fetch
    // the upgrade starts come back before the test asks.
    env.upgrade_canister(
        canister_id,
        II_WASM.clone(),
        candid::encode_one(arg_with_notifications_enabled()).expect("encoding the arg"),
        None,
    )
    .expect("upgrading");
    (
        canister_id,
        anchor,
        browser.successor().principal(),
        session,
    )
}

#[test]
fn should_answer_pending_to_a_browser_until_the_vapid_key_is_held() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, browser_key, _) = install_fetching_the_vapid_key(&env);

    assert_eq!(
        prepare_webpush_subscription(&env, canister_id, browser_key, anchor)?,
        PrepareWebPushSubscriptionResult::Pending
    );

    let mut is_ready = false;
    for _ in 0..OUTCALL_ROUNDS {
        env.tick();
        if matches!(
            prepare_webpush_subscription(&env, canister_id, browser_key, anchor)?,
            PrepareWebPushSubscriptionResult::Ok(_)
        ) {
            is_ready = true;
            break;
        }
    }
    assert!(is_ready, "the keys are answered once the VAPID key is held");
    Ok(())
}

/// A subnet without the key answers the same however often it is asked, so a failed
/// fetch is reported rather than answered with `Pending` for good.
#[test]
fn should_answer_the_failure_once_the_vapid_key_fetch_failed() -> Result<(), RejectResponse> {
    let env = canister_tests::framework::env_without_signing_keys();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_notifications_enabled());
    let anchor = flows::register_anchor(&env, canister_id);
    let browser = BrowserKey::new(1);
    let session = session_of(&sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &browser),
    ));
    grant_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("grant refused");
    let browser_key = browser.successor().principal();

    let mut is_refused = false;
    for _ in 0..OUTCALL_ROUNDS {
        env.tick();
        if matches!(
            prepare_webpush_subscription(&env, canister_id, browser_key, anchor)?,
            PrepareWebPushSubscriptionResult::Err(
                PrepareWebPushSubscriptionError::InternalCanisterError(_)
            )
        ) {
            is_refused = true;
            break;
        }
    }
    assert!(is_refused, "the failed fetch is answered to a browser");
    assert!(matches!(
        app_prepare_pwa_webpush_subscription(&env, canister_id, session)?,
        AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::InternalCanisterError(_)
        )
    ));
    Ok(())
}

#[test]
fn should_answer_pending_to_an_installed_app_until_the_vapid_key_is_held(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, _, session) = install_fetching_the_vapid_key(&env);

    assert_eq!(
        app_prepare_pwa_webpush_subscription(&env, canister_id, session)?,
        AppPreparePwaWebPushSubscriptionResult::Pending
    );

    grant_consent(&env, canister_id, principal_1(), anchor, APP.into())?.expect("grant refused");
    let mut is_ready = false;
    for _ in 0..OUTCALL_ROUNDS {
        env.tick();
        if matches!(
            app_prepare_pwa_webpush_subscription(&env, canister_id, session)?,
            AppPreparePwaWebPushSubscriptionResult::Ok(_)
        ) {
            is_ready = true;
            break;
        }
    }
    assert!(is_ready, "the keys are answered once the VAPID key is held");
    Ok(())
}

#[test]
fn should_drop_only_the_entry_a_gone_wake_up_announced() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, account_session) = install_signed_in(&env, None, None);
    for id in 1..=2 {
        assert_eq!(
            deliver(
                &env,
                canister_id,
                account_session.account_principal,
                id,
                200
            ),
            vec![BROWSER_ENDPOINT.to_string()]
        );
    }

    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 3, 404),
        vec![BROWSER_ENDPOINT.to_string()]
    );

    let read = |inbox_notification_id| {
        browser_get_notification(
            &env,
            canister_id,
            BrowserKey::new(1).successor().principal(),
            GetNotificationRequest {
                anchor_number: anchor,
                browser_id: account_session.browser_id,
                inbox_notification_id,
            },
        )
        .expect("read rejected")
        .expect("read refused")
        .notification
        .map(|notification| notification.id)
    };
    assert_eq!(read(0), Some(1));
    assert_eq!(read(1), Some(2));
    assert_eq!(read(2), None);
    Ok(())
}

#[test]
fn should_refuse_a_signed_out_browser_its_inbox_until_it_signs_in_again(
) -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor, account_session) = install_signed_in(&env, None, None);
    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 1, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
    let read = |browser_key| {
        browser_get_notification(
            &env,
            canister_id,
            browser_key,
            GetNotificationRequest {
                anchor_number: anchor,
                browser_id: account_session.browser_id,
                inbox_notification_id: FIRST_ENTRY,
            },
        )
        .expect("read rejected")
        .map(|response| response.notification.map(|notification| notification.id))
    };
    let prepare = |browser_key| {
        prepare_notification_delegation(
            &env,
            canister_id,
            browser_key,
            PrepareNotificationDelegationRequest {
                anchor_number: anchor,
                origin: APP.to_string(),
                account_number: None,
                session_key: ByteBuf::from(vec![2; 32]),
            },
        )
        .expect("prepare rejected")
        .map(|_| ())
    };
    let browser = BrowserKey::new(1).successor();
    assert_eq!(read(browser.principal()), Ok(Some(1)));

    revoke_browser_sessions(
        &env,
        canister_id,
        principal_1(),
        RevokeBrowserSessionsRequest {
            identity_number: anchor,
            browser_id: account_session.browser_id,
        },
    )?
    .expect("sign-out refused");

    assert_eq!(
        read(browser.principal()),
        Err(GetNotificationError::InvalidBrowserKey)
    );
    assert_eq!(
        prepare(browser.principal()),
        Err(NotificationDelegationError::NoNotificationAccess)
    );

    // Signing in again on the same browser, whose key moves on as it does.
    sign_in(
        &env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &browser),
    );
    assert_eq!(read(browser.successor().principal()), Ok(Some(1)));
    // Signing out dropped its subscription, which content delegations also need, so it
    // subscribes again as a browser that signs in does.
    subscribe_webpush(
        &env,
        canister_id,
        browser.successor().principal(),
        SetWebPushSubscriptionRequest {
            anchor_number: anchor,
            endpoint: BROWSER_ENDPOINT.to_string(),
            p256dh: p256dh(7),
            transport_key: p256dh(7),
            sealed_auth: ByteBuf::from(vec![0u8; 32]),
        },
    )?
    .expect("subscribe refused");
    assert_eq!(prepare(browser.successor().principal()), Ok(()));
    Ok(())
}

#[test]
fn should_refuse_a_home_screen_app_once_its_browser_is_signed_out() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    let (app, _) = link_app(&env, canister_id, anchor, 1, 50);
    assert_eq!(
        browser_get_notification(&env, canister_id, app, get_request(anchor))?,
        Ok(GetNotificationResponse { notification: None })
    );

    revoke_browser_sessions(
        &env,
        canister_id,
        principal_1(),
        RevokeBrowserSessionsRequest {
            identity_number: anchor,
            browser_id: FIRST_BROWSER,
        },
    )?
    .expect("sign-out refused");

    assert_eq!(
        browser_get_notification(&env, canister_id, app, get_request(anchor))?,
        Err(GetNotificationError::InvalidBrowserKey)
    );
    Ok(())
}
