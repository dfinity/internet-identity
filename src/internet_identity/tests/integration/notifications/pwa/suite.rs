//! A suite: frontends that send on behalf of the application at their derivation origin,
//! which lists them among its alternative origins, each with an installed web app of its
//! own that only its own notifications wake.

use super::same_device::{
    another_installed_app_entry, claim_with, installed_apps_of_frontend, linked_phone,
    linked_phone_with, path_status, phone_with, probe, register_key, remove_pwa_entry_of_frontend,
    remove_unanswered_of_frontend, Phone, PROBE_ANSWER_TIME,
};
use super::*;
use canister_tests::api::internet_identity::notifications::seal_auth;
use canister_tests::api::internet_identity::notifications::subscribe_pwa_webpush_for_frontend;
use canister_tests::framework::assert_metric;
use internet_identity_interface::internet_identity::types::{
    AppGetPwaWebPushSubscriptionStatusRequest, AppPreparePwaWebPushSubscriptionRequest,
    AppSetPwaWebPushSubscriptionRequest, ClaimSameDeviceLinkError, NotificationPath,
    PwaWebPushSubscription, SendNotificationError, SendNotificationResponse,
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
            AppPreparePwaWebPushSubscriptionError::NoSuchFrontend
        )
    );
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(unlisted_frontend)),
        None
    );
}

/// An application at a plain `http` origin, which II reads no list from.
const INSECURE_APP: &str = "http://insecure-app.example";

#[test]
fn should_refuse_to_prepare_for_a_frontend_of_an_application_at_an_http_origin() {
    let env = env();
    let (canister_id, _, _) = install_signed_in(&env, None, None);
    let session = session_of(&second_identity_at(&env, canister_id, INSECURE_APP));

    let mut urls = Vec::new();
    let mut response = prepare_for_frontend(&env, canister_id, session, SUITE_FRONTEND);
    for _ in 0..3 {
        if response != AppPreparePwaWebPushSubscriptionResult::Pending {
            break;
        }
        urls.extend(fetched_urls(&env));
        response = prepare_for_frontend(&env, canister_id, session, SUITE_FRONTEND);
    }
    urls.extend(fetched_urls(&env));

    assert_eq!(
        response,
        AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::InternalCanisterError(
                "origin must be https".to_string()
            )
        )
    );
    assert!(
        urls.iter()
            .all(|url| !url.ends_with(ALTERNATIVE_ORIGINS_PATH)),
        "II read the alternative origins of an http application: {urls:?}"
    );
}

/// An application at a plain `http` loopback origin, which II reads no list from while
/// insecure lists are not allowed.
const LOOPBACK_APP: &str = "http://localhost:5173";

#[test]
fn should_answer_keys_to_an_http_application_that_names_its_own_origin() {
    let env = env();
    let (canister_id, _, _) = install_signed_in(&env, None, None);
    let session = session_of(&second_identity_at(&env, canister_id, LOOPBACK_APP));

    let mut urls = Vec::new();
    let mut response = prepare_for_frontend(&env, canister_id, session, LOOPBACK_APP);
    for _ in 0..3 {
        if response != AppPreparePwaWebPushSubscriptionResult::Pending {
            break;
        }
        urls.extend(fetched_urls(&env));
        response = prepare_for_frontend(&env, canister_id, session, LOOPBACK_APP);
    }
    urls.extend(fetched_urls(&env));

    assert!(
        matches!(response, AppPreparePwaWebPushSubscriptionResult::Ok(_)),
        "{response:?}"
    );
    assert!(
        urls.iter()
            .all(|url| !url.ends_with(ALTERNATIVE_ORIGINS_PATH)),
        "II read the alternative origins for the application's own origin: {urls:?}"
    );
}

/// Another frontend of [`APP`].
const CALENDAR_FRONTEND: &str = "https://calendar.suite.example";
/// Where [`CALENDAR_FRONTEND`]'s installed web app is subscribed.
const CALENDAR_ENDPOINT: &str = "https://suite.push.example/calendar";
/// How long II reads an application's alternative origins from its cache before it
/// fetches them again: an hour.
const ALTERNATIVE_ORIGINS_FRESH_FOR: Duration = Duration::from_secs(60 * 60);

/// Answers every outcall for [`OUTCALL_ROUNDS`] rounds like [`answer_outcalls`], except
/// that the alternative origins list names `alternative_origins`.
fn answer_outcalls_listing(env: &PocketIc, alternative_origins: &[&str]) {
    let alternative_origins_body = serde_json::to_vec(&serde_json::json!({
        "alternativeOrigins": alternative_origins
    }))
    .expect("a list");
    for _ in 0..OUTCALL_ROUNDS {
        env.advance_time(Duration::from_millis(ROUND_MILLIS));
        env.tick();
        for request in env.get_canister_http() {
            let body = if request.url.ends_with(ALTERNATIVE_ORIGINS_PATH) {
                alternative_origins_body.clone()
            } else if request.url.starts_with("https://listed") {
                format!(r#"{{"senders":["{}"]}}"#, app_sender().to_text()).into_bytes()
            } else {
                vec![]
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
}

#[test]
fn should_drop_the_slot_of_a_frontend_the_application_stopped_listing_on_the_next_prepare() {
    let env = env();
    let (canister_id, account_session) = install_suite_subscribed(&env);
    let session = session_of(&account_session);
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(SUITE_FRONTEND)),
        Some(SUITE_ENDPOINT.to_string())
    );

    // The cached list still names only the suite frontend, and refreshes in the background.
    env.advance_time(ALTERNATIVE_ORIGINS_FRESH_FOR + Duration::from_secs(1));
    assert_eq!(
        prepare_for_frontend(&env, canister_id, session, CALENDAR_FRONTEND),
        AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::NoSuchFrontend
        )
    );
    answer_outcalls_listing(&env, &[CALENDAR_FRONTEND]);
    subscribe_pwa_webpush_for_frontend(
        &env,
        canister_id,
        session,
        Some(CALENDAR_FRONTEND),
        CALENDAR_ENDPOINT,
        p256dh(12),
        &OTHER_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");

    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(SUITE_FRONTEND)),
        None
    );
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(CALENDAR_FRONTEND)),
        Some(CALENDAR_ENDPOINT.to_string())
    );
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, None),
        Some(PWA_ENDPOINT.to_string())
    );
}

/// Prepares `session`'s installed web app of [`APP`]'s own origin until II answers keys,
/// answering every outcall between tries like [`answer_outcalls_listing`] with
/// `alternative_origins`.
fn prepare_own_origin_listing(
    env: &PocketIc,
    canister_id: CanisterId,
    session: Principal,
    alternative_origins: &[&str],
) {
    let mut response = prepare_for_frontend(env, canister_id, session, APP);
    for _ in 0..3 {
        if response != AppPreparePwaWebPushSubscriptionResult::Pending {
            break;
        }
        answer_outcalls_listing(env, alternative_origins);
        response = prepare_for_frontend(env, canister_id, session, APP);
    }
    assert!(
        matches!(response, AppPreparePwaWebPushSubscriptionResult::Ok(_)),
        "{response:?}"
    );
}

#[test]
fn should_drop_the_slot_of_the_last_frontend_the_application_stopped_listing_on_the_next_write() {
    let env = env();
    let (canister_id, account_session) = install_suite_subscribed(&env);
    let session = session_of(&account_session);

    // The refresh this refusal starts caches a list that names no frontend at all.
    env.advance_time(ALTERNATIVE_ORIGINS_FRESH_FOR + Duration::from_secs(1));
    assert_eq!(
        prepare_for_frontend(&env, canister_id, session, CALENDAR_FRONTEND),
        AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::NoSuchFrontend
        )
    );
    answer_outcalls_listing(&env, &[]);
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(SUITE_FRONTEND)),
        Some(SUITE_ENDPOINT.to_string())
    );

    prepare_own_origin_listing(&env, canister_id, session, &[]);

    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(SUITE_FRONTEND)),
        None
    );
}

#[test]
fn should_refuse_a_set_for_a_frontend_the_application_stopped_listing_after_its_prepare() {
    let env = env();
    let (canister_id, _, account_session) = install_signed_in(&env, None, None);
    let session = session_of(&account_session);
    assert_eq!(
        prepare_for_frontend(&env, canister_id, session, SUITE_FRONTEND),
        AppPreparePwaWebPushSubscriptionResult::Pending
    );
    answer_outcalls(&env, 200);

    // Prepared while the cached list still names the frontend, a few minutes before it
    // is refreshed.
    env.advance_time(ALTERNATIVE_ORIGINS_FRESH_FOR - Duration::from_secs(120));
    let mut response = prepare_for_frontend(&env, canister_id, session, SUITE_FRONTEND);
    for _ in 0..3 {
        if response != AppPreparePwaWebPushSubscriptionResult::Pending {
            break;
        }
        answer_outcalls(&env, 200);
        response = prepare_for_frontend(&env, canister_id, session, SUITE_FRONTEND);
    }
    let AppPreparePwaWebPushSubscriptionResult::Ok(keys) = response else {
        panic!("prepare answered {response:?}");
    };
    env.advance_time(Duration::from_secs(180));
    assert_eq!(
        prepare_for_frontend(&env, canister_id, session, CALENDAR_FRONTEND),
        AppPreparePwaWebPushSubscriptionResult::Err(
            AppPreparePwaWebPushSubscriptionError::NoSuchFrontend
        )
    );
    answer_outcalls_listing(&env, &[]);

    let (transport_key, sealed_auth) = seal_auth(&keys.transport_public_key, &OTHER_AUTH_SECRET);
    assert_eq!(
        app_set_pwa_webpush_subscription(
            &env,
            canister_id,
            session,
            AppSetPwaWebPushSubscriptionRequest {
                subscription: Some(PwaWebPushSubscription {
                    endpoint: SUITE_ENDPOINT.to_string(),
                    p256dh: p256dh(11),
                    transport_key,
                    sealed_auth,
                }),
                frontend_origin: Some(SUITE_FRONTEND.to_string()),
            },
        )
        .expect("set rejected"),
        Err(AppSetPwaWebPushSubscriptionError::TransportKeyExpired)
    );

    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(SUITE_FRONTEND)),
        None
    );
    let other_session = session_of(&second_identity_at(&env, canister_id, APP));
    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            other_session,
            SUITE_ENDPOINT,
            p256dh(12),
            &OTHER_AUTH_SECRET
        ),
        Ok(Ok(()))
    );
}

#[test]
fn should_let_another_account_take_over_a_browsers_slots_once_the_first_accounts_sessions_expired()
{
    let env = env();
    let (canister_id, anchor, first_account_session) =
        install_signed_in(&env, Some(20 * MINUTE_NS), None);
    let first_session = session_of(&first_account_session);
    subscribe_suite_frontend(&env, canister_id, first_session);
    subscribe_pwa_webpush(
        &env,
        canister_id,
        first_session,
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    env.advance_time(Duration::from_nanos(21 * MINUTE_NS));

    let other_account = create_account(
        &env,
        canister_id,
        principal_1(),
        anchor,
        APP.into(),
        "Other".to_string(),
    )
    .expect("create rejected")
    .expect("create refused")
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
    let other_suite_endpoint = "https://suite.push.example/other-account";
    let other_app_endpoint = "https://pwa.push.example/other-account";
    assert_eq!(
        subscribe_pwa_webpush_for_frontend(
            &env,
            canister_id,
            other_session,
            Some(SUITE_FRONTEND),
            other_suite_endpoint,
            p256dh(13),
            &OTHER_AUTH_SECRET,
        ),
        Ok(Ok(()))
    );
    assert_eq!(
        subscribe_pwa_webpush(
            &env,
            canister_id,
            other_session,
            other_app_endpoint,
            p256dh(14),
            &TEST_AUTH_SECRET,
        ),
        Ok(Ok(()))
    );

    assert_eq!(
        frontend_endpoint(&env, canister_id, other_session, Some(SUITE_FRONTEND)),
        Some(other_suite_endpoint.to_string())
    );
    assert_eq!(
        frontend_endpoint(&env, canister_id, other_session, None),
        Some(other_app_endpoint.to_string())
    );
}

#[test]
fn should_keep_a_frontends_slot_and_fetch_no_list_on_a_write_while_no_list_is_cached() {
    let env = env();
    let (canister_id, account_session) = install_suite_subscribed(&env);
    let session = session_of(&account_session);
    // The cache lives on the heap, so an upgrade leaves no list cached.
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let mut urls = Vec::new();
    let mut response = prepare_for_frontend(&env, canister_id, session, APP);
    for _ in 0..3 {
        if response != AppPreparePwaWebPushSubscriptionResult::Pending {
            break;
        }
        urls.extend(fetched_urls(&env));
        response = prepare_for_frontend(&env, canister_id, session, APP);
    }
    urls.extend(fetched_urls(&env));

    assert!(
        matches!(response, AppPreparePwaWebPushSubscriptionResult::Ok(_)),
        "{response:?}"
    );
    assert_eq!(
        frontend_endpoint(&env, canister_id, session, Some(SUITE_FRONTEND)),
        Some(SUITE_ENDPOINT.to_string())
    );
    assert!(
        urls.iter()
            .all(|url| !url.ends_with(ALTERNATIVE_ORIGINS_PATH)),
        "a write fetched the alternative origins: {urls:?}"
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

/// On a phone whose installed web app of [`APP`]'s own origin is linked to Safari, a
/// sign-in at [`SUITE_FRONTEND`] checks that frontend's installed web apps, of which there
/// are none, so Safari's own subscription is the path its notifications take.
#[test]
fn should_check_only_the_installed_apps_of_the_frontend_signed_in_at() {
    let env = env();
    let phone = linked_phone(&env);

    assert_eq!(
        path_status(&env, &phone, installed_apps_of_frontend(&phone, None))
            .subscriptions
            .len(),
        1
    );
    assert_eq!(
        path_status(
            &env,
            &phone,
            installed_apps_of_frontend(&phone, Some(SUITE_FRONTEND))
        )
        .subscriptions,
        vec![]
    );
    assert_eq!(
        path_status(&env, &phone, NotificationPath::ThisBrowser)
            .subscriptions
            .len(),
        1
    );
}

/// Subscribes the installed web app `session` signs in to on its browser entry for
/// [`SUITE_FRONTEND`] at [`SUITE_ENDPOINT`] and for [`CALENDAR_FRONTEND`] at
/// [`CALENDAR_ENDPOINT`], with [`APP`]'s list naming both, and none for [`APP`] itself.
fn subscribe_suite_and_calendar_frontends(
    env: &PocketIc,
    canister_id: CanisterId,
    session: Principal,
) {
    for (frontend_origin, endpoint, seed) in [
        (SUITE_FRONTEND, SUITE_ENDPOINT, 11),
        (CALENDAR_FRONTEND, CALENDAR_ENDPOINT, 12),
    ] {
        prepare_for_frontend(env, canister_id, session, frontend_origin);
        answer_outcalls_listing(env, &[SUITE_FRONTEND, CALENDAR_FRONTEND]);
        subscribe_pwa_webpush_for_frontend(
            env,
            canister_id,
            session,
            Some(frontend_origin),
            endpoint,
            p256dh(seed),
            &OTHER_AUTH_SECRET,
        )
        .expect("subscribe rejected")
        .expect("subscribe refused");
    }
    assert_eq!(frontend_endpoint(env, canister_id, session, None), None);
}

/// A phone whose installed web app entry holds [`SUITE_FRONTEND`]'s and
/// [`CALENDAR_FRONTEND`]'s installed web apps, linked to Safari, with one push to each
/// waiting for an answer.
fn linked_suite_phone_with_a_push_to_each_frontend(env: &PocketIc) -> Phone {
    let phone = linked_phone_with(env, subscribe_suite_and_calendar_frontends);
    for (frontend_origin, endpoint, id) in [
        (SUITE_FRONTEND, SUITE_ENDPOINT, 1),
        (CALENDAR_FRONTEND, CALENDAR_ENDPOINT, 2),
    ] {
        let response = send_for_frontend(
            env,
            phone.canister_id,
            frontend_origin,
            phone.account_principal,
            id,
        );
        assert!(matches!(response, Ok(response) if response.not_accepted.is_empty()));
        assert_eq!(answer_outcalls(env, 201), vec![endpoint.to_string()]);
    }
    phone
}

/// The unanswered push count and whether a probe waits, of the one installed web app of
/// `frontend_origin` on `phone`'s linked entry.
fn frontend_slot_state(
    env: &PocketIc,
    phone: &Phone,
    frontend_origin: &str,
) -> Vec<(Option<u32>, bool)> {
    path_status(
        env,
        phone,
        installed_apps_of_frontend(phone, Some(frontend_origin)),
    )
    .subscriptions
    .into_iter()
    .map(|subscription| {
        (
            subscription.unanswered_pushes.map(|pushes| pushes.count),
            subscription.probe_sent_at_ns.is_some(),
        )
    })
    .collect()
}

#[test]
fn should_probe_and_reset_only_the_installed_app_of_the_frontend_asked_about() {
    let env = env();
    let phone = linked_suite_phone_with_a_push_to_each_frontend(&env);

    assert_eq!(
        probe(
            &env,
            &phone,
            installed_apps_of_frontend(&phone, Some(SUITE_FRONTEND))
        ),
        Ok(())
    );
    assert_eq!(answer_outcalls(&env, 201), vec![SUITE_ENDPOINT.to_string()]);
    assert_eq!(
        frontend_slot_state(&env, &phone, SUITE_FRONTEND),
        vec![(Some(2), true)]
    );
    assert_eq!(
        frontend_slot_state(&env, &phone, CALENDAR_FRONTEND),
        vec![(Some(1), false)]
    );

    remove_pwa_entry_of_frontend(&env, &phone, FIRST_ENTRY, Some(SUITE_FRONTEND));

    assert_eq!(
        frontend_slot_state(&env, &phone, SUITE_FRONTEND),
        vec![(None, false)]
    );
    assert_eq!(
        frontend_slot_state(&env, &phone, CALENDAR_FRONTEND),
        vec![(Some(1), false)]
    );
}

#[test]
fn should_remove_only_the_unanswered_installed_app_of_the_frontend_asked_about() {
    let env = env();
    let phone = linked_suite_phone_with_a_push_to_each_frontend(&env);
    for frontend_origin in [SUITE_FRONTEND, CALENDAR_FRONTEND] {
        assert_eq!(
            probe(
                &env,
                &phone,
                installed_apps_of_frontend(&phone, Some(frontend_origin))
            ),
            Ok(())
        );
        answer_outcalls(&env, 201);
    }
    env.advance_time(PROBE_ANSWER_TIME);

    remove_unanswered_of_frontend(&env, &phone, Some(SUITE_FRONTEND));

    assert_eq!(frontend_slot_state(&env, &phone, SUITE_FRONTEND), vec![]);
    assert_eq!(
        frontend_slot_state(&env, &phone, CALENDAR_FRONTEND),
        vec![(Some(2), true)]
    );
}

/// An entry whose only installed web app is [`SUITE_FRONTEND`]'s, an origin other than the
/// derivation origin, holds one of its own, so it cannot link to another entry.
#[test]
fn should_refuse_a_claim_from_an_entry_holding_only_another_origins_installed_app() {
    let env = env();
    let phone = phone_with(&env, subscribe_suite_frontend);
    assert_eq!(
        frontend_endpoint(&env, phone.canister_id, phone.pwa, None),
        None
    );
    let third = another_installed_app_entry(&env, &phone);
    register_key(&env, &phone, third, &BrowserKey::new(42));

    let installed_app_browser = BrowserKey::new(5).successor().principal();
    assert_eq!(
        claim_with(&env, &phone, installed_app_browser, &BrowserKey::new(42)),
        Err(ClaimSameDeviceLinkError::InternalCanisterError(
            "this browser holds an installed web app subscription of its own".to_string()
        ))
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
