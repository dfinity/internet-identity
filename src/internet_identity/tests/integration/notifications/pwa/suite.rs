//! A suite: frontends that send on behalf of the application at their derivation origin,
//! which lists them among its alternative origins, each with an installed web app of its
//! own that only its own notifications wake.

use super::same_device::linked_phone;
use super::*;
use canister_tests::framework::assert_metric;
use canister_tests::api::internet_identity::notifications::subscribe_pwa_webpush_for_frontend;
use internet_identity_interface::internet_identity::types::{
    AppGetPwaWebPushSubscriptionStatusRequest, AppPreparePwaWebPushSubscriptionRequest,
    SendNotificationError, SendNotificationResponse,
};

/// A frontend [`APP`]'s alternative-origins list does not name.
const UNLISTED_FRONTEND: &str = "https://unlisted-mail.suite.example";

/// Where [`SUITE_FRONTEND`]'s installed web app is subscribed.
const SUITE_ENDPOINT: &str = "https://suite.push.example/device";

/// What `app_prepare_pwa_webpush_subscription` answers `session` for its installed web app
/// at `frontend_origin`.
fn prepare_for_frontend(
    env: &PocketIc,
    canister_id: CanisterId,
    session: Principal,
    frontend_origin: &str,
) -> AppPreparePwaWebPushSubscriptionResult {
    app_prepare_pwa_webpush_subscription(
        env,
        canister_id,
        session,
        Some(AppPreparePwaWebPushSubscriptionRequest {
            frontend_origin: Some(frontend_origin.to_string()),
        }),
    )
    .expect("prepare rejected")
}

/// Subscribes `session`'s installed web app of [`SUITE_FRONTEND`] at [`SUITE_ENDPOINT`],
/// past the fetch of [`APP`]'s alternative origins the first prepare starts.
fn subscribe_suite_frontend(env: &PocketIc, canister_id: CanisterId, session: Principal) {
    prepare_for_frontend(env, canister_id, session, SUITE_FRONTEND);
    answer_outcalls(env, 200);
    subscribe_pwa_webpush_for_frontend(
        env,
        canister_id,
        session,
        Some(SUITE_FRONTEND),
        SUITE_ENDPOINT,
        p256dh(11),
        &OTHER_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
}

/// The endpoint `session`'s installed web app at `frontend_origin`, the app's own origin
/// where `None`, is subscribed at.
fn frontend_endpoint(
    env: &PocketIc,
    canister_id: CanisterId,
    session: Principal,
    frontend_origin: Option<&str>,
) -> Option<String> {
    app_get_pwa_webpush_subscription_status(
        env,
        canister_id,
        session,
        Some(AppGetPwaWebPushSubscriptionStatusRequest {
            frontend_origin: frontend_origin.map(str::to_string),
        }),
    )
    .expect("status rejected")
    .expect("status refused")
    .map(|status| status.endpoint)
}

/// [`install_subscribed`], with the same session's installed web app of
/// [`SUITE_FRONTEND`] subscribed on the same browser entry at [`SUITE_ENDPOINT`].
fn install_suite_subscribed(env: &PocketIc) -> (CanisterId, PrepareAccountSessionResponse) {
    let (canister_id, _, account_session) = install_subscribed(env);
    subscribe_suite_frontend(env, canister_id, session_of(&account_session));
    (canister_id, account_session)
}

/// Sends [`APP`]'s notification `id` to `recipient` on behalf of the frontend at
/// `frontend_origin`, past the list fetches that defer the first send.
fn send_for_frontend(
    env: &PocketIc,
    canister_id: CanisterId,
    frontend_origin: &str,
    recipient: Principal,
    id: u64,
) -> Result<SendNotificationResponse, SendNotificationError> {
    for _ in 0..3 {
        let response = send_once_for_frontend(env, canister_id, frontend_origin, recipient, id);
        if !is_deferred(&response) {
            return response;
        }
        answer_outcalls(env, 200);
    }
    panic!("the send was still deferred after the lists were served")
}

#[test]
fn should_wake_the_browser_rather_than_another_frontends_installed_app() {
    let env = env();
    let (canister_id, _, account_session) = install_subscribed(&env);
    assert_eq!(
        frontend_endpoint(&env, canister_id, session_of(&account_session), None),
        Some(PWA_ENDPOINT.to_string())
    );

    let response = send_for_frontend(
        &env,
        canister_id,
        SUITE_FRONTEND,
        account_session.account_principal,
        1,
    );

    assert_eq!(
        response,
        Ok(SendNotificationResponse {
            not_accepted: vec![]
        })
    );
    assert_eq!(
        answer_outcalls(&env, 200),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_keep_a_subscription_per_frontend_on_one_browser_entry() {
    let env = env();
    let (canister_id, account_session) = install_suite_subscribed(&env);
    let session = session_of(&account_session);

    assert_eq!(
        frontend_endpoint(&env, canister_id, session, None),
        Some(PWA_ENDPOINT.to_string())
    );
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(APP)),
        Some(PWA_ENDPOINT.to_string())
    );
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(SUITE_FRONTEND)),
        Some(SUITE_ENDPOINT.to_string())
    );
}

#[test]
fn should_wake_only_the_installed_app_of_the_frontend_a_notification_is_for() {
    let env = env();
    let (canister_id, account_session) = install_suite_subscribed(&env);

    let response = send_for_frontend(
        &env,
        canister_id,
        SUITE_FRONTEND,
        account_session.account_principal,
        1,
    );
    assert!(matches!(response, Ok(response) if response.not_accepted.is_empty()));
    assert_eq!(answer_outcalls(&env, 201), vec![SUITE_ENDPOINT.to_string()]);

    assert_eq!(
        deliver(&env, canister_id, account_session.account_principal, 2, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_clear_only_the_subscription_of_the_frontend_its_push_service_calls_gone() {
    let env = env();
    let (canister_id, account_session) = install_suite_subscribed(&env);
    let session = session_of(&account_session);

    let response = send_for_frontend(
        &env,
        canister_id,
        SUITE_FRONTEND,
        account_session.account_principal,
        1,
    );
    assert!(matches!(response, Ok(response) if response.not_accepted.is_empty()));
    assert_eq!(answer_outcalls(&env, 410), vec![SUITE_ENDPOINT.to_string()]);

    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(SUITE_FRONTEND)),
        None
    );
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, None),
        Some(PWA_ENDPOINT.to_string())
    );
}

#[test]
fn should_answer_pending_then_keys_to_a_listed_frontend() {
    let env = env();
    let (canister_id, _, account_session) = install_signed_in(&env, None, None);
    let session = session_of(&account_session);

    assert_eq!(
        prepare_for_frontend(&env, canister_id, session, SUITE_FRONTEND),
        AppPreparePwaWebPushSubscriptionResult::Pending
    );
    answer_outcalls(&env, 200);
    assert!(matches!(
        prepare_for_frontend(&env, canister_id, session, SUITE_FRONTEND),
        AppPreparePwaWebPushSubscriptionResult::Ok(_)
    ));
}

#[test]
fn should_refuse_to_prepare_for_a_frontend_the_application_does_not_list() {
    let env = env();
    let (canister_id, _, account_session) = install_signed_in(&env, None, None);
    let session = session_of(&account_session);
    let unlisted_frontend = "https://evil-mail.suite.example";

    assert_eq!(
        prepare_for_frontend(&env, canister_id, session, unlisted_frontend),
        AppPreparePwaWebPushSubscriptionResult::Pending
    );
    answer_outcalls(&env, 200);
    assert_eq!(
        prepare_for_frontend(&env, canister_id, session, unlisted_frontend),
        AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::UnlistedFrontend
        )
    );
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(unlisted_frontend)),
        None
    );
}

#[test]
fn should_skip_a_linked_entry_only_for_the_frontend_whose_installed_app_is_linked() {
    let env = env();
    let phone = linked_phone(&env);

    // The installed app of the app's own origin delivers on the phone, so Safari is
    // skipped for the app's own notifications only.
    let response = send_for_frontend(
        &env,
        phone.canister_id,
        SUITE_FRONTEND,
        phone.account_principal,
        1,
    );
    assert!(matches!(response, Ok(response) if response.not_accepted.is_empty()));
    assert_eq!(
        answer_outcalls(&env, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 2, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_refuse_a_frontend_the_application_does_not_list() {
    let env = env();
    let (canister_id, _, account_session) = install_subscribed(&env);

    let response = send_for_frontend(
        &env,
        canister_id,
        "https://evil-mail.suite.example",
        account_session.account_principal,
        1,
    );

    assert_eq!(response, Err(SendNotificationError::NoSuchFrontend));
    assert_eq!(answer_outcalls(&env, 200), Vec::<String>::new());
}

#[test]
fn should_refuse_a_sender_the_application_does_not_list_before_reading_its_frontends() {
    let env = env();
    let (canister_id, _, account_session) = install_subscribed(&env);
    let send = || {
        app_send_notification(
            &env,
            canister_id,
            Principal::from_slice(&[0xcd, 0x01]),
            SendNotificationArg {
                origin: SUITE_FRONTEND.to_string(),
                notifications: vec![Notification {
                    id: 1,
                    recipient: account_session.account_principal,
                    expires_at: None,
                    urgency: None,
                }],
                derivation_origin: Some(APP.to_string()),
            },
        )
        .expect("send rejected")
    };

    let first_response = send();
    assert!(matches!(&first_response, Ok(response) if !response.not_accepted.is_empty()));
    let urls_fetched_before_refusal = fetched_urls(&env);
    assert_eq!(send(), Err(SendNotificationError::NoSuchSender));
    let urls_fetched_after_refusal = fetched_urls(&env);
    for urls in [&urls_fetched_before_refusal, &urls_fetched_after_refusal] {
        assert!(
            urls.iter()
                .all(|url| !url.ends_with(ALTERNATIVE_ORIGINS_PATH)),
            "an unlisted sender made II read the application's frontends: {urls:?}"
        );
    }
}

/// Sends [`APP`]'s notification `id` to `recipient` once, on behalf of the frontend at
/// `frontend_origin`, without answering the list fetches it starts.
fn send_once_for_frontend(
    env: &PocketIc,
    canister_id: CanisterId,
    frontend_origin: &str,
    recipient: Principal,
    id: u64,
) -> Result<SendNotificationResponse, SendNotificationError> {
    app_send_notification(
        env,
        canister_id,
        app_sender(),
        SendNotificationArg {
            origin: frontend_origin.to_string(),
            notifications: vec![Notification {
                id,
                recipient,
                expires_at: None,
                urgency: None,
            }],
            derivation_origin: Some(APP.to_string()),
        },
    )
    .expect("send rejected")
}

fn is_deferred(response: &Result<SendNotificationResponse, SendNotificationError>) -> bool {
    matches!(response, Ok(response) if response.not_accepted.iter().any(
        |refusal| matches!(refusal.reason, NotAcceptedReason::Deferred { .. })
    ))
}

/// Answers every outcall like [`answer_outcalls`], except that the alternative-origins
/// list fetch fails with a server error.
fn fail_alternative_origins_fetches(env: &PocketIc) {
    for _ in 0..OUTCALL_ROUNDS {
        env.advance_time(Duration::from_millis(ROUND_MILLIS));
        env.tick();
        for request in env.get_canister_http() {
            let (status, body) = if request.url.ends_with(ALTERNATIVE_ORIGINS_PATH) {
                (500, vec![])
            } else {
                (
                    200,
                    format!(r#"{{"senders":["{}"]}}"#, app_sender().to_text()).into_bytes(),
                )
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
}

/// A listed sender's refusals and deferrals over its application's frontends count under
/// the application's origin, and the frontend it named stays out of `/metrics`.
#[test]
fn should_count_a_listed_senders_frontend_refusals_under_its_application() {
    let env = env();
    let (canister_id, _, account_session) = install_subscribed(&env);
    let recipient = account_session.account_principal;

    assert!(is_deferred(&send_once_for_frontend(
        &env,
        canister_id,
        SUITE_FRONTEND,
        recipient,
        1
    )));
    fail_alternative_origins_fetches(&env);
    assert!(is_deferred(&send_once_for_frontend(
        &env,
        canister_id,
        SUITE_FRONTEND,
        recipient,
        1
    )));
    fail_alternative_origins_fetches(&env);
    assert!(is_deferred(&send_once_for_frontend(
        &env,
        canister_id,
        SUITE_FRONTEND,
        recipient,
        1
    )));

    env.advance_time(Duration::from_secs(10 * 60));
    assert_eq!(
        send_for_frontend(&env, canister_id, UNLISTED_FRONTEND, recipient, 1),
        Err(SendNotificationError::NoSuchFrontend)
    );

    let metrics = get_metrics(&env, canister_id);
    assert_metric(
        &metrics,
        &format!(
            r#"internet_identity_notification_losses{{drop="accept",reason="frontend_not_listed",source="apps",urgency="normal",origin="{APP}"}}"#
        ),
        1.0,
    );
    assert_metric(
        &metrics,
        &format!(
            r#"internet_identity_notifications_deferred{{cause="alternative_origins",source="apps",urgency="normal",origin="{APP}"}}"#
        ),
        3.0,
    );
    assert!(
        !metrics.contains(UNLISTED_FRONTEND),
        "the unlisted frontend's origin is in /metrics"
    );
}

/// Answers every outcall like [`answer_outcalls`], and returns the URLs fetched with `GET`.
fn fetched_urls(env: &PocketIc) -> Vec<String> {
    let mut urls = Vec::new();
    for _ in 0..OUTCALL_ROUNDS {
        env.advance_time(Duration::from_millis(ROUND_MILLIS));
        env.tick();
        for request in env.get_canister_http() {
            urls.push(request.url.clone());
            let body = if request.url.ends_with(ALTERNATIVE_ORIGINS_PATH) {
                format!(r#"{{"alternativeOrigins":["{SUITE_FRONTEND}"]}}"#).into_bytes()
            } else {
                format!(r#"{{"senders":["{}"]}}"#, app_sender().to_text()).into_bytes()
            };
            env.mock_canister_http_response(MockCanisterHttpResponse {
                subnet_id: request.subnet_id,
                request_id: request.request_id,
                response: CanisterHttpResponse::CanisterHttpReply(CanisterHttpReply {
                    status: 200,
                    headers: vec![],
                    body,
                }),
                additional_responses: vec![],
            });
        }
    }
    urls
}
