//! A suite: frontends that send on behalf of the application at their derivation origin,
//! which lists them among its alternative origins.

use super::*;
use internet_identity_interface::internet_identity::types::{
    SendNotificationError, SendNotificationResponse,
};

/// Sends [`APP`]'s notification `id` to `recipient` on behalf of the frontend at
/// `frontend_origin`, past the list fetches that defer the first send.
fn send_for_frontend(
    env: &PocketIc,
    canister_id: CanisterId,
    frontend_origin: &str,
    recipient: Principal,
    id: u64,
) -> Result<SendNotificationResponse, SendNotificationError> {
    let send = || {
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
    };
    for _ in 0..3 {
        let response = send()?;
        let is_deferred = response
            .not_accepted
            .iter()
            .any(|refusal| matches!(refusal.reason, NotAcceptedReason::Deferred { .. }));
        if !is_deferred {
            return Ok(response);
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

    assert_eq!(response, Err(SendNotificationError::UnlistedFrontend));
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
    let fetched_urls = fetched_urls(&env);
    assert_eq!(send(), Err(SendNotificationError::NoSuchSender));
    assert!(
        fetched_urls
            .iter()
            .all(|url| !url.ends_with(ALTERNATIVE_ORIGINS_PATH)),
        "an unlisted sender made II read the application's frontends: {fetched_urls:?}"
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
