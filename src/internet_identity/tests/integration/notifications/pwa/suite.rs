//! A suite: frontends that send on behalf of the application at their derivation origin,
//! which lists them among its alternative origins.

use super::*;
use canister_tests::framework::assert_metric;
use internet_identity_interface::internet_identity::types::{
    SendNotificationError, SendNotificationResponse,
};

/// A frontend [`APP`]'s alternative-origins list does not name.
const UNLISTED_FRONTEND: &str = "https://unlisted-mail.suite.example";

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
fn should_deliver_what_a_listed_frontend_sends_for_its_application() {
    let env = env();
    let (canister_id, _, account_session) = install_subscribed(&env);

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
    assert_eq!(answer_outcalls(&env, 200), vec![PWA_ENDPOINT.to_string()]);
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
