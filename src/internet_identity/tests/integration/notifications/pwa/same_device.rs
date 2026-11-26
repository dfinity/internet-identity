//! One delivery per device, and repairing a path that stopped answering: linking an
//! installed web app's entry to another entry on the same device, skipping that entry for
//! the app's notifications, counting unanswered pushes, probing a path and removing an
//! installed web app that did not answer.

use super::*;
use canister_tests::api::internet_identity::notifications::{
    app_get_pwa_webpush_subscription_status, app_get_same_device_link_status,
    app_prepare_same_device_link, app_probe_pwa_webpush_subscription, browser_remove_notification,
    claim_same_device_link, get_notification_path_status, probe_notification_path,
    remove_unanswered_pwa_webpush_subscriptions,
};
use internet_identity_interface::internet_identity::types::{
    AppGetPwaNotificationResponse, AppGetSameDeviceLinkStatusError, AppPrepareSameDeviceLinkError,
    AppPrepareSameDeviceLinkRequest, AppProbePwaWebPushSubscriptionError, ClaimSameDeviceLinkError,
    ClaimSameDeviceLinkRequest, GetNotificationPathStatusError, GetNotificationPathStatusRequest,
    InstalledAppsPath, NotificationPath, NotificationPathStatus, ProbeNotificationPathError,
    ProbeNotificationPathRequest, PwaNotificationEvent, RemoveNotificationRequest,
    RemoveUnansweredPwaWebPushSubscriptionsError, RemoveUnansweredPwaWebPushSubscriptionsRequest,
    SameDeviceLinkStatus, UnansweredPushes,
};

/// The entry [`install_signed_in`] signs in from: the first of the identity's.
const SAFARI_BROWSER_ID: BrowserId = 0;
const HOUR_NS: u64 = 60 * MINUTE_NS;

/// How long a probe has to be answered before its silence is evidence: the canister's
/// `MIN_PROBE_ANSWER_TIME_NS`, written out.
const PROBE_ANSWER_TIME: std::time::Duration = std::time::Duration::from_secs(10);

/// The key calls from the entry [`install_signed_in`] signs in from are signed with.
fn safari() -> Principal {
    BrowserKey::new(1).successor().principal()
}

/// The one-shot key an installed web app registers to link its entry.
fn link_key() -> BrowserKey {
    BrowserKey::new(40)
}

/// `principal_1`'s identity at [`APP`] in two entries of one phone: Safari, subscribed for
/// itself at [`BROWSER_ENDPOINT`], and the installed web app, signed in from the browser
/// seed 5 names and subscribed at [`PWA_ENDPOINT`]. Not linked yet.
struct Phone {
    canister_id: CanisterId,
    anchor: AnchorNumber,
    account_principal: Principal,
    /// The installed web app's session.
    pwa: Principal,
    /// Safari's session at [`APP`].
    safari_session: Principal,
}

fn phone(env: &PocketIc) -> Phone {
    let (canister_id, anchor, safari_session) = install_signed_in(env, None, None);
    let pwa = sign_in(
        env,
        canister_id,
        principal_1(),
        session_request(anchor, APP, &BrowserKey::new(5)),
    );
    subscribe_pwa_webpush(
        env,
        canister_id,
        session_of(&pwa),
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    assert_eq!(pwa.account_principal, safari_session.account_principal);
    Phone {
        canister_id,
        anchor,
        account_principal: pwa.account_principal,
        pwa: session_of(&pwa),
        safari_session: session_of(&safari_session),
    }
}

/// [`phone`], with the installed web app's entry linked to Safari's.
fn linked_phone(env: &PocketIc) -> Phone {
    let phone = phone(env);
    prepare_link(env, &phone);
    assert_eq!(claim(env, &phone, safari(), &link_key()), Ok(()));
    phone
}

fn prepare_link(env: &PocketIc, phone: &Phone) {
    app_prepare_same_device_link(
        env,
        phone.canister_id,
        phone.pwa,
        AppPrepareSameDeviceLinkRequest {
            link_public_key: link_key().public_key(),
        },
    )
    .expect("prepare rejected")
    .expect("prepare refused");
}

/// `caller` claims the link registered with [`link_key`], signing with `signer`.
fn claim(
    env: &PocketIc,
    phone: &Phone,
    caller: Principal,
    signer: &BrowserKey,
) -> Result<(), ClaimSameDeviceLinkError> {
    claim_same_device_link(
        env,
        phone.canister_id,
        caller,
        ClaimSameDeviceLinkRequest {
            anchor_number: phone.anchor,
            link_public_key: link_key().public_key(),
            signature: signer.sign_same_device_link_claim(phone.anchor, caller),
        },
    )
    .expect("claim rejected")
}

fn link_status(env: &PocketIc, phone: &Phone) -> SameDeviceLinkStatus {
    app_get_same_device_link_status(env, phone.canister_id, phone.pwa)
        .expect("status rejected")
        .expect("status refused")
}

fn path_status(env: &PocketIc, phone: &Phone, path: NotificationPath) -> NotificationPathStatus {
    get_notification_path_status(
        env,
        phone.canister_id,
        safari(),
        GetNotificationPathStatusRequest {
            anchor_number: phone.anchor,
            path,
        },
    )
    .expect("status rejected")
    .expect("status refused")
}

/// The unanswered pushes of each subscription on `path`.
fn unanswered(
    env: &PocketIc,
    phone: &Phone,
    path: NotificationPath,
) -> Vec<Option<UnansweredPushes>> {
    path_status(env, phone, path)
        .subscriptions
        .into_iter()
        .map(|subscription| subscription.unanswered_pushes)
        .collect()
}

fn installed_apps(phone: &Phone) -> NotificationPath {
    NotificationPath::InstalledApps(InstalledAppsPath {
        origin: APP.to_string(),
        account_principal: phone.account_principal,
    })
}

fn probe(
    env: &PocketIc,
    phone: &Phone,
    path: NotificationPath,
) -> Result<(), ProbeNotificationPathError> {
    probe_notification_path(
        env,
        phone.canister_id,
        safari(),
        ProbeNotificationPathRequest {
            anchor_number: phone.anchor,
            path,
        },
    )
    .expect("probe rejected")
}

fn remove_unanswered(env: &PocketIc, phone: &Phone) {
    remove_unanswered_pwa_webpush_subscriptions(
        env,
        phone.canister_id,
        safari(),
        RemoveUnansweredPwaWebPushSubscriptionsRequest {
            anchor_number: phone.anchor,
            origin: APP.to_string(),
            account_principal: phone.account_principal,
        },
    )
    .expect("remove rejected")
    .expect("remove refused");
}

fn pwa_entry(
    env: &PocketIc,
    phone: &Phone,
    inbox_notification_id: InboxNotificationId,
) -> AppGetPwaNotificationResponse {
    app_get_pwa_notification(
        env,
        phone.canister_id,
        phone.pwa,
        AppGetPwaNotificationRequest {
            inbox_notification_id,
        },
    )
    .expect("get rejected")
    .expect("get refused")
}

fn remove_pwa_entry(env: &PocketIc, phone: &Phone, inbox_notification_id: InboxNotificationId) {
    app_remove_pwa_notification(
        env,
        phone.canister_id,
        phone.pwa,
        AppRemovePwaNotificationRequest {
            inbox_notification_id,
            not_shown: None,
        },
    )
    .expect("remove rejected")
    .expect("remove refused");
}

fn count(unanswered: &[Option<UnansweredPushes>]) -> Vec<Option<(u32, bool)>> {
    unanswered
        .iter()
        .map(|one| one.as_ref().map(|one| (one.count, one.is_past_limit)))
        .collect()
}

// ---- Linking ----

#[test]
fn should_link_the_installed_apps_entry_to_the_safari_entry_that_claims_its_key() {
    let env = env();
    let phone = phone(&env);
    assert_eq!(
        link_status(&env, &phone),
        SameDeviceLinkStatus::OtherPathMayDeliver
    );

    prepare_link(&env, &phone);
    assert_eq!(claim(&env, &phone, safari(), &link_key()), Ok(()));

    assert_eq!(link_status(&env, &phone), SameDeviceLinkStatus::Linked);
    // Spent: a second claim finds nothing awaiting the key.
    assert_eq!(
        claim(&env, &phone, safari(), &link_key()),
        Err(ClaimSameDeviceLinkError::NoPendingLink)
    );
}

#[test]
fn should_answer_no_other_path_where_no_other_entry_can_be_woken() -> Result<(), RejectResponse> {
    let env = env();
    let phone = phone(&env);
    remove_webpush(&env, &phone)?;

    assert_eq!(link_status(&env, &phone), SameDeviceLinkStatus::NoOtherPath);
    Ok(())
}

fn remove_webpush(env: &PocketIc, phone: &Phone) -> Result<(), RejectResponse> {
    canister_tests::api::internet_identity::notifications::remove_webpush_subscription(
        env,
        phone.canister_id,
        principal_1(),
        phone.anchor,
        SAFARI_BROWSER_ID,
    )?
    .expect("remove refused");
    Ok(())
}

#[test]
fn should_refuse_a_claim_signed_for_another_caller() {
    let env = env();
    let phone = phone(&env);
    prepare_link(&env, &phone);

    let signature_for_another = link_key().sign_same_device_link_claim(phone.anchor, principal_2());
    let refused = claim_same_device_link(
        &env,
        phone.canister_id,
        safari(),
        ClaimSameDeviceLinkRequest {
            anchor_number: phone.anchor,
            link_public_key: link_key().public_key(),
            signature: signature_for_another,
        },
    )
    .expect("claim rejected");

    assert_eq!(
        refused,
        Err(ClaimSameDeviceLinkError::InternalCanisterError(
            "the signature is not the link key's over this claim".to_string()
        ))
    );
    assert_eq!(
        link_status(&env, &phone),
        SameDeviceLinkStatus::OtherPathMayDeliver
    );
}

#[test]
fn should_refuse_a_claim_signed_by_another_key() {
    let env = env();
    let phone = phone(&env);
    prepare_link(&env, &phone);

    assert_eq!(
        claim(&env, &phone, safari(), &BrowserKey::new(41)),
        Err(ClaimSameDeviceLinkError::InternalCanisterError(
            "the signature is not the link key's over this claim".to_string()
        ))
    );
}

#[test]
fn should_refuse_a_claim_once_its_thirty_minutes_are_up() {
    let env = env();
    let phone = phone(&env);
    prepare_link(&env, &phone);

    env.advance_time(Duration::from_secs(30 * 60));

    assert_eq!(
        claim(&env, &phone, safari(), &link_key()),
        Err(ClaimSameDeviceLinkError::NoPendingLink)
    );
}

#[test]
fn should_accept_a_claim_a_second_before_its_thirty_minutes_are_up() {
    let env = env();
    let phone = phone(&env);
    prepare_link(&env, &phone);

    env.advance_time(Duration::from_secs(30 * 60 - 1));

    assert_eq!(claim(&env, &phone, safari(), &link_key()), Ok(()));
}

#[test]
fn should_tell_an_app_the_identity_has_not_allowed_nothing_about_its_other_devices(
) -> Result<(), RejectResponse> {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(link_status(&env, &phone), SameDeviceLinkStatus::Linked);

    revoke_consent(
        &env,
        phone.canister_id,
        principal_1(),
        phone.anchor,
        APP.into(),
    )?
    .expect("revoke refused");

    assert_eq!(link_status(&env, &phone), SameDeviceLinkStatus::NoOtherPath);
    Ok(())
}

#[test]
fn should_refuse_a_claim_from_the_entry_that_registered_the_key() {
    let env = env();
    let phone = phone(&env);
    prepare_link(&env, &phone);

    let pwa_browser = BrowserKey::new(5).successor().principal();
    assert_eq!(
        claim(&env, &phone, pwa_browser, &link_key()),
        Err(ClaimSameDeviceLinkError::NoPendingLink)
    );
}

#[test]
fn should_refuse_a_claim_from_a_browser_of_another_identity() {
    let env = env();
    let phone = phone(&env);
    prepare_link(&env, &phone);
    let other_anchor =
        flows::register_anchor_with_device(&env, phone.canister_id, &device_data_2());
    sign_in(
        &env,
        phone.canister_id,
        principal_2(),
        session_request(other_anchor, APP, &BrowserKey::new(2)),
    );
    let other_browser = BrowserKey::new(2).successor().principal();

    // Naming its own identity, it finds no key there.
    let own_identity = claim_same_device_link(
        &env,
        phone.canister_id,
        other_browser,
        ClaimSameDeviceLinkRequest {
            anchor_number: other_anchor,
            link_public_key: link_key().public_key(),
            signature: link_key().sign_same_device_link_claim(other_anchor, other_browser),
        },
    )
    .expect("claim rejected");
    assert_eq!(own_identity, Err(ClaimSameDeviceLinkError::NoPendingLink));
    // Naming the first identity, it is no browser of it.
    assert_eq!(
        claim(&env, &phone, other_browser, &link_key()),
        Err(ClaimSameDeviceLinkError::InvalidBrowserKey)
    );
    assert_eq!(
        link_status(&env, &phone),
        SameDeviceLinkStatus::OtherPathMayDeliver
    );
}

#[test]
fn should_refuse_a_link_key_that_is_not_a_p256_key() -> Result<(), RejectResponse> {
    let env = env();
    let phone = phone(&env);

    assert!(matches!(
        app_prepare_same_device_link(
            &env,
            phone.canister_id,
            phone.pwa,
            AppPrepareSameDeviceLinkRequest {
                link_public_key: ByteBuf::from(vec![7; 91]),
            },
        )?,
        Err(AppPrepareSameDeviceLinkError::InternalCanisterError(_))
    ));
    Ok(())
}

// ---- Skipping the linked entry ----

#[test]
fn should_wake_both_entries_of_an_unlinked_phone() {
    let env = env();
    let phone = phone(&env);

    let mut pushed = deliver(&env, phone.canister_id, phone.account_principal, 1, 201);
    pushed.sort();

    assert_eq!(
        pushed,
        vec![BROWSER_ENDPOINT.to_string(), PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_wake_only_the_installed_app_of_a_linked_phone() {
    let env = env();
    let phone = linked_phone(&env);

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_wake_safari_again_once_the_installed_app_signs_out() -> Result<(), RejectResponse> {
    let env = env();
    let phone = linked_phone(&env);

    app_revoke_session(&env, phone.canister_id, phone.pwa)?.expect("sign-out refused");

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );
    Ok(())
}

#[test]
fn should_skip_the_linked_entrys_own_installed_app_too() {
    let env = env();
    let phone = linked_phone(&env);
    // Safari's own session registers an installed app of its own after the link.
    subscribe_pwa_webpush(
        &env,
        phone.canister_id,
        phone.safari_session,
        UNPROBED_PWA_ENDPOINT,
        p256dh(10),
        &OTHER_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_still_wake_safari_for_another_app() {
    let env = env();
    let phone = linked_phone(&env);
    let other = sign_in(
        &env,
        phone.canister_id,
        principal_1(),
        session_request(phone.anchor, OTHER_APP, &BrowserKey::new(1).successor()),
    );
    grant_consent(
        &env,
        phone.canister_id,
        principal_1(),
        phone.anchor,
        OTHER_APP.into(),
    )
    .expect("grant rejected")
    .expect("grant refused");

    send_accepted_from(
        &env,
        phone.canister_id,
        OTHER_APP,
        other.account_principal,
        1,
        201,
    );

    assert_eq!(
        answer_outcalls(&env, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

// ---- Counting unanswered pushes ----

#[test]
fn should_count_unanswered_pushes_until_one_is_fetched() {
    let env = env();
    let phone = phone(&env);
    assert_eq!(
        count(&unanswered(&env, &phone, NotificationPath::ThisBrowser)),
        vec![None]
    );

    for id in 1..=3 {
        deliver(&env, phone.canister_id, phone.account_principal, id, 201);
    }
    assert_eq!(
        count(&unanswered(&env, &phone, NotificationPath::ThisBrowser)),
        vec![Some((3, false))]
    );
    assert_eq!(pwa_unanswered(&env, &phone), Some((3, false)));

    browser_remove_notification(
        &env,
        phone.canister_id,
        safari(),
        RemoveNotificationRequest {
            anchor_number: phone.anchor,
            browser_id: SAFARI_BROWSER_ID,
            inbox_notification_id: FIRST_ENTRY,
            not_shown: None,
        },
    )
    .expect("remove rejected")
    .expect("remove refused");
    remove_pwa_entry(&env, &phone, FIRST_ENTRY);

    assert_eq!(
        count(&unanswered(&env, &phone, NotificationPath::ThisBrowser)),
        vec![None]
    );
    assert_eq!(pwa_unanswered(&env, &phone), None);
}

/// The installed web app's unanswered pushes, as its own session reads them.
fn pwa_unanswered(env: &PocketIc, phone: &Phone) -> Option<(u32, bool)> {
    app_get_pwa_webpush_subscription_status(env, phone.canister_id, phone.pwa)
        .expect("status rejected")
        .expect("status refused")
        .expect("subscribed")
        .unanswered_pushes
        .map(|unanswered| (unanswered.count, unanswered.is_past_limit))
}

#[test]
fn should_count_no_push_the_push_service_refused() {
    let env = env();
    let phone = phone(&env);

    deliver(&env, phone.canister_id, phone.account_principal, 1, 500);

    assert_eq!(
        count(&unanswered(&env, &phone, NotificationPath::ThisBrowser)),
        vec![None]
    );
}

#[test]
fn should_pass_the_limit_at_three_pushes_the_first_an_hour_old() {
    let env = env();
    let phone = phone(&env);

    deliver(&env, phone.canister_id, phone.account_principal, 1, 201);
    deliver(&env, phone.canister_id, phone.account_principal, 2, 201);
    env.advance_time(Duration::from_nanos(HOUR_NS));
    env.tick();
    // Two, however old, are not enough.
    assert_eq!(
        count(&unanswered(&env, &phone, NotificationPath::ThisBrowser)),
        vec![Some((2, false))]
    );

    deliver(&env, phone.canister_id, phone.account_principal, 3, 201);

    assert_eq!(
        count(&unanswered(&env, &phone, NotificationPath::ThisBrowser)),
        vec![Some((3, true))]
    );
}

#[test]
fn should_not_pass_the_limit_before_the_first_push_is_an_hour_old() {
    let env = env();
    let phone = phone(&env);
    let first_sent_at_ns = env.get_time().as_nanos_since_unix_epoch();
    for id in 1..=3 {
        deliver(&env, phone.canister_id, phone.account_principal, id, 201);
    }
    let first = unanswered(&env, &phone, NotificationPath::ThisBrowser)
        .pop()
        .flatten()
        .expect("unanswered pushes");
    assert!(first.first_sent_at_ns > first_sent_at_ns);

    // A round may move the clock on by a nanosecond, so the query reads it a nanosecond
    // or two short of the hour, then at or past it.
    advance_to(&env, first.first_sent_at_ns + HOUR_NS - 2);
    env.tick();
    assert_eq!(
        count(&unanswered(&env, &phone, NotificationPath::ThisBrowser)),
        vec![Some((3, false))]
    );

    env.advance_time(Duration::from_nanos(2));
    env.tick();
    assert_eq!(
        count(&unanswered(&env, &phone, NotificationPath::ThisBrowser)),
        vec![Some((3, true))]
    );
}

// ---- Probing ----

#[test]
fn should_probe_the_linked_installed_app_and_be_answered_by_its_fetch() {
    let env = env();
    let phone = linked_phone(&env);

    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    assert_eq!(answer_outcalls(&env, 201), vec![PWA_ENDPOINT.to_string()]);
    assert_eq!(
        count(&unanswered(&env, &phone, installed_apps(&phone))),
        vec![Some((1, false))]
    );
    assert!(is_probe_waiting(&env, &phone));

    assert_eq!(
        pwa_entry(&env, &phone, FIRST_ENTRY),
        AppGetPwaNotificationResponse {
            notification: None,
            event: Some(PwaNotificationEvent::NotificationsOn),
        }
    );
    remove_pwa_entry(&env, &phone, FIRST_ENTRY);

    assert_eq!(
        count(&unanswered(&env, &phone, installed_apps(&phone))),
        vec![None]
    );
    assert!(!is_probe_waiting(&env, &phone));
}

/// A probe the push service refused never reached the device, so it is no evidence
/// either way: nothing is recorded, and the repair removes nothing.
#[test]
fn should_record_no_probe_the_push_service_refused() {
    let env = env();
    let phone = linked_phone(&env);

    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    answer_outcalls(&env, 503);
    assert!(!is_probe_waiting(&env, &phone));

    remove_unanswered(&env, &phone);

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

/// A probe still waiting for its dispatch pass has reached nobody yet, so it is recorded
/// only once the push service accepts it.
#[test]
fn should_record_no_probe_before_the_push_service_accepts_it() {
    let env = env();
    let phone = linked_phone(&env);

    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    assert!(!is_probe_waiting(&env, &phone));

    remove_unanswered(&env, &phone);

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 1, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

/// A probe the push service accepted a moment ago has not had the time a device needs to
/// answer it, so its silence is no evidence yet.
#[test]
fn should_keep_an_installed_app_whose_probe_is_younger_than_its_answer_time() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    answer_outcalls(&env, 201);
    assert!(is_probe_waiting(&env, &phone));

    remove_unanswered(&env, &phone);
    assert_eq!(
        path_status(&env, &phone, installed_apps(&phone))
            .subscriptions
            .len(),
        1
    );

    env.advance_time(PROBE_ANSWER_TIME);
    remove_unanswered(&env, &phone);
    assert_eq!(
        path_status(&env, &phone, installed_apps(&phone)).subscriptions,
        vec![]
    );
}

/// Whether each subscription on `phone`'s installed apps path has had its probe's answer
/// time, as the canister reports it.
fn has_had_answer_time(env: &PocketIc, phone: &Phone) -> Vec<bool> {
    path_status(env, phone, installed_apps(phone))
        .subscriptions
        .iter()
        .map(|subscription| subscription.has_probe_had_its_answer_time)
        .collect()
}

/// Moves `env`'s clock to `at_ns` and runs a round there.
fn advance_to(env: &PocketIc, at_ns: u64) {
    let now_ns = env.get_time().as_nanos_since_unix_epoch();
    env.advance_time(Duration::from_nanos(at_ns - now_ns));
    env.tick();
}

/// The canister, not the browser's clock, says when a probe has had its 10 seconds.
#[test]
fn should_report_a_probe_has_had_its_answer_time_once_ten_seconds_have_passed() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(has_had_answer_time(&env, &phone), vec![false]);
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    answer_outcalls(&env, 201);
    assert_eq!(has_had_answer_time(&env, &phone), vec![false]);
    let sent_at_ns = path_status(&env, &phone, installed_apps(&phone))
        .subscriptions
        .first()
        .and_then(|subscription| subscription.probe_sent_at_ns)
        .expect("no probe recorded");

    // A query reads the time of the last executed round, so each step is ticked in.
    advance_to(&env, sent_at_ns + 9_999_000_000);
    assert_eq!(has_had_answer_time(&env, &phone), vec![false]);

    advance_to(&env, sent_at_ns + 10_000_000_000);
    assert_eq!(has_had_answer_time(&env, &phone), vec![true]);
}

/// A push service's answer belongs to the registration it was posted to: one made since
/// at the same endpoint was never sent the probe, so the answer records nothing on it.
#[test]
fn should_record_no_probe_on_a_registration_replaced_while_it_was_posted() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    // The probe is posted, and the installed app subscribes again before the push service
    // answers.
    let mut is_posted = false;
    for _ in 0..40 {
        env.advance_time(std::time::Duration::from_millis(250));
        env.tick();
        for request in env.get_canister_http() {
            if request.http_method == CanisterHttpMethod::POST {
                is_posted = true;
                continue;
            }
            // Whatever the dispatch asks before it posts is answered as it would be.
            env.mock_canister_http_response(MockCanisterHttpResponse {
                subnet_id: request.subnet_id,
                request_id: request.request_id,
                response: CanisterHttpResponse::CanisterHttpReply(CanisterHttpReply {
                    status: 200,
                    headers: vec![],
                    body: vec![],
                }),
                additional_responses: vec![],
            });
        }
        if is_posted {
            break;
        }
    }
    assert!(is_posted, "the probe was never posted");
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

    answer_outcalls(&env, 201);

    assert!(!is_probe_waiting(&env, &phone));
}

/// Whether the installed app's subscription has a probe nobody fetched yet.
fn is_probe_waiting(env: &PocketIc, phone: &Phone) -> bool {
    path_status(env, phone, installed_apps(phone))
        .subscriptions
        .iter()
        .any(|subscription| subscription.probe_sent_at_ns.is_some())
}

/// Another account at [`APP`] of `phone`'s identity, signed in to the installed app on
/// its entry and subscribed there, so the installed app now delivers for it. Answers
/// that account's session and principal.
fn switch_the_installed_app_to_another_account(
    env: &PocketIc,
    phone: &Phone,
) -> (Principal, Principal) {
    let other_account = create_account(
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
    let other = sign_in(
        env,
        phone.canister_id,
        principal_1(),
        PrepareAccountSessionRequest {
            account_number: other_account,
            ..session_request(phone.anchor, APP, &BrowserKey::new(5).successor())
        },
    );
    subscribe_pwa_webpush(
        env,
        phone.canister_id,
        session_of(&other),
        PWA_ENDPOINT,
        p256dh(9),
        &TEST_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    (session_of(&other), other.account_principal)
}

#[test]
fn should_leave_the_count_of_another_accounts_installed_app_when_one_account_removes() {
    let env = env();
    let phone = phone(&env);
    let (other_session, other_account) = switch_the_installed_app_to_another_account(&env, &phone);
    deliver(&env, phone.canister_id, other_account, 1, 201);

    // The first account's session removes an entry of its own, which is no answer for the
    // installed app that now delivers for the other account.
    remove_pwa_entry(&env, &phone, FIRST_ENTRY);

    assert_eq!(
        app_get_pwa_webpush_subscription_status(&env, phone.canister_id, other_session)
            .expect("status rejected")
            .expect("status refused")
            .and_then(|status| status.unanswered_pushes)
            .map(|unanswered| unanswered.count),
        Some(1)
    );
}

#[test]
fn should_see_only_the_installed_apps_of_the_account_asked_about() {
    let env = env();
    let phone = linked_phone(&env);
    let (_, other_account) = switch_the_installed_app_to_another_account(&env, &phone);
    deliver(&env, phone.canister_id, other_account, 1, 201);

    // The first account's view of the installed apps, which deliver for the other one.
    assert_eq!(
        path_status(&env, &phone, installed_apps(&phone)).subscriptions,
        vec![]
    );
    remove_unanswered(&env, &phone);

    // Still delivering for the other account, which the first one's repair did not touch.
    assert_eq!(
        deliver(&env, phone.canister_id, other_account, 2, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_refuse_a_claim_from_an_entry_already_linked() {
    let env = env();
    let phone = linked_phone(&env);
    // Without a subscription of its own, so only the link it already has stands in the way.
    app_set_pwa_webpush_subscription(
        &env,
        phone.canister_id,
        phone.pwa,
        AppSetPwaWebPushSubscriptionRequest { subscription: None },
    )
    .expect("remove rejected")
    .expect("remove refused");
    let third = another_installed_app_entry(&env, &phone);
    register_key(&env, &phone, third, &BrowserKey::new(42));

    // The installed app's entry already points at Safari's, so it cannot point again.
    let installed_app_browser = BrowserKey::new(5).successor().principal();
    assert_eq!(
        claim_with(&env, &phone, installed_app_browser, &BrowserKey::new(42)),
        Err(ClaimSameDeviceLinkError::InternalCanisterError(
            "this browser is already linked to another".to_string()
        ))
    );
}

#[test]
fn should_refuse_a_claim_of_a_key_whose_entry_is_pointed_at() {
    let env = env();
    let phone = linked_phone(&env);
    let third_browser = BrowserKey::new(6).successor().principal();
    another_installed_app_entry(&env, &phone);
    // Safari's own session registers a key: its entry is the one the installed app points at.
    register_key(&env, &phone, phone.safari_session, &BrowserKey::new(42));

    assert_eq!(
        claim_with(&env, &phone, third_browser, &BrowserKey::new(42)),
        Err(ClaimSameDeviceLinkError::InternalCanisterError(
            "the browser that registered the key is already linked to".to_string()
        ))
    );
}

#[test]
fn should_refuse_a_claim_from_an_entry_holding_an_installed_app_of_its_own() {
    let env = env();
    let phone = phone(&env);
    let third = another_installed_app_entry(&env, &phone);
    register_key(&env, &phone, third, &BrowserKey::new(42));

    // The installed app's entry is not linked, but it holds the app's subscription.
    let installed_app_browser = BrowserKey::new(5).successor().principal();
    assert_eq!(
        claim_with(&env, &phone, installed_app_browser, &BrowserKey::new(42)),
        Err(ClaimSameDeviceLinkError::InternalCanisterError(
            "this browser holds an installed web app subscription of its own".to_string()
        ))
    );
}

/// A third entry of `phone`'s identity, signed in to [`APP`] from the browser seed 6
/// names, holding nothing. Answers its session.
fn another_installed_app_entry(env: &PocketIc, phone: &Phone) -> Principal {
    session_of(&sign_in(
        env,
        phone.canister_id,
        principal_1(),
        session_request(phone.anchor, APP, &BrowserKey::new(6)),
    ))
}

/// `session` registers `key` for its entry's same-device link.
fn register_key(env: &PocketIc, phone: &Phone, session: Principal, key: &BrowserKey) {
    app_prepare_same_device_link(
        env,
        phone.canister_id,
        session,
        AppPrepareSameDeviceLinkRequest {
            link_public_key: key.public_key(),
        },
    )
    .expect("prepare rejected")
    .expect("prepare refused");
}

/// `caller` claims the link registered with `key`.
fn claim_with(
    env: &PocketIc,
    phone: &Phone,
    caller: Principal,
    key: &BrowserKey,
) -> Result<(), ClaimSameDeviceLinkError> {
    claim_same_device_link(
        env,
        phone.canister_id,
        caller,
        ClaimSameDeviceLinkRequest {
            anchor_number: phone.anchor,
            link_public_key: key.public_key(),
            signature: key.sign_same_device_link_claim(phone.anchor, caller),
        },
    )
    .expect("claim rejected")
}

#[test]
fn should_keep_the_installed_apps_probe_from_internet_identitys_own_worker(
) -> Result<(), RejectResponse> {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    answer_outcalls(&env, 201);
    let pwa_browser = BrowserKey::new(5).successor().principal();
    let pwa_browser_id: BrowserId = 1;

    let read = browser_get_notification(
        &env,
        phone.canister_id,
        pwa_browser,
        GetNotificationRequest {
            anchor_number: phone.anchor,
            browser_id: pwa_browser_id,
            inbox_notification_id: FIRST_ENTRY,
        },
    )?
    .expect("get refused");
    assert_eq!(read.notification, None);
    browser_remove_notification(
        &env,
        phone.canister_id,
        pwa_browser,
        RemoveNotificationRequest {
            anchor_number: phone.anchor,
            browser_id: pwa_browser_id,
            inbox_notification_id: FIRST_ENTRY,
            not_shown: None,
        },
    )?
    .expect("remove refused");

    assert_eq!(
        pwa_entry(&env, &phone, FIRST_ENTRY).event,
        Some(PwaNotificationEvent::NotificationsOn)
    );
    Ok(())
}

#[test]
fn should_probe_this_browser_through_its_own_subscription() {
    let env = env();
    let phone = phone(&env);

    assert_eq!(probe(&env, &phone, NotificationPath::ThisBrowser), Ok(()));

    assert_eq!(
        answer_outcalls(&env, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_send_no_second_probe_while_the_first_waits_to_be_fetched() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    assert_eq!(answer_outcalls(&env, 201), vec![PWA_ENDPOINT.to_string()]);

    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    assert_eq!(
        app_probe_pwa_webpush_subscription(&env, phone.canister_id, phone.pwa)
            .expect("probe rejected"),
        Ok(())
    );
    assert_eq!(answer_outcalls(&env, 201), Vec::<String>::new());

    env.advance_time(Duration::from_secs(60));
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    assert_eq!(answer_outcalls(&env, 201), vec![PWA_ENDPOINT.to_string()]);
}

#[test]
fn should_probe_again_once_the_probe_was_fetched() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    answer_outcalls(&env, 201);
    remove_pwa_entry(&env, &phone, FIRST_ENTRY);

    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));

    assert_eq!(answer_outcalls(&env, 201), vec![PWA_ENDPOINT.to_string()]);
}

#[test]
fn should_answer_not_reachable_for_a_path_with_nothing_on_it() {
    let env = env();
    let phone = phone(&env);

    // Not linked, so Safari's entry reaches no installed app.
    assert_eq!(
        probe(&env, &phone, installed_apps(&phone)),
        Err(ProbeNotificationPathError::NotReachable)
    );
    assert_eq!(
        path_status(&env, &phone, installed_apps(&phone)).subscriptions,
        vec![]
    );
}

#[test]
fn should_let_the_installed_app_probe_its_own_subscription() {
    let env = env();
    let phone = phone(&env);

    assert_eq!(
        app_probe_pwa_webpush_subscription(&env, phone.canister_id, phone.pwa)
            .expect("probe rejected"),
        Ok(())
    );
    assert_eq!(answer_outcalls(&env, 201), vec![PWA_ENDPOINT.to_string()]);
    assert_eq!(pwa_unanswered(&env, &phone), Some((1, false)));
    assert_eq!(
        pwa_entry(&env, &phone, FIRST_ENTRY).event,
        Some(PwaNotificationEvent::NotificationsOn)
    );

    remove_pwa_entry(&env, &phone, FIRST_ENTRY);

    assert_eq!(pwa_unanswered(&env, &phone), None);
}

// ---- Removing an installed app that did not answer ----

#[test]
fn should_remove_the_linked_installed_app_that_did_not_answer() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    answer_outcalls(&env, 201);
    env.advance_time(PROBE_ANSWER_TIME);

    remove_unanswered(&env, &phone);

    assert_eq!(
        path_status(&env, &phone, installed_apps(&phone)).subscriptions,
        vec![]
    );
    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 2, 201),
        vec![BROWSER_ENDPOINT.to_string()]
    );
}

#[test]
fn should_keep_the_linked_installed_app_that_answered() {
    let env = env();
    let phone = linked_phone(&env);
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    answer_outcalls(&env, 201);
    remove_pwa_entry(&env, &phone, FIRST_ENTRY);

    remove_unanswered(&env, &phone);

    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 2, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_keep_an_installed_app_that_answered_its_probe_though_a_later_push_waits() {
    let env = env();
    let phone = linked_phone(&env);
    // A second installed app of the same account on the same phone, at an address of its own.
    let second = another_installed_app_entry(&env, &phone);
    subscribe_pwa_webpush(
        &env,
        phone.canister_id,
        second,
        UNPROBED_PWA_ENDPOINT,
        p256dh(10),
        &OTHER_AUTH_SECRET,
    )
    .expect("subscribe rejected")
    .expect("subscribe refused");
    register_key(&env, &phone, second, &BrowserKey::new(42));
    assert_eq!(
        claim_with(&env, &phone, safari(), &BrowserKey::new(42)),
        Ok(())
    );
    assert_eq!(probe(&env, &phone, installed_apps(&phone)), Ok(()));
    let mut probed = answer_outcalls(&env, 201);
    probed.sort();
    assert_eq!(
        probed,
        vec![PWA_ENDPOINT.to_string(), UNPROBED_PWA_ENDPOINT.to_string()]
    );

    // The first answers its probe, then an ordinary push reaches both and waits.
    remove_pwa_entry(&env, &phone, FIRST_ENTRY);
    deliver(&env, phone.canister_id, phone.account_principal, 1, 201);
    env.advance_time(PROBE_ANSWER_TIME);

    remove_unanswered(&env, &phone);

    // Only the second, whose probe nobody fetched, is gone.
    assert_eq!(
        deliver(&env, phone.canister_id, phone.account_principal, 2, 201),
        vec![PWA_ENDPOINT.to_string()]
    );
}

#[test]
fn should_leave_an_installed_app_sharing_the_callers_entry_off_the_path() {
    let env = env();
    // The installed app subscribes from the session of the very browser entry that asks.
    let (canister_id, anchor, signed_in) = install_subscribed(&env);
    let shared = Phone {
        canister_id,
        anchor,
        account_principal: signed_in.account_principal,
        pwa: session_of(&signed_in),
        safari_session: session_of(&signed_in),
    };

    assert_eq!(
        path_status(&env, &shared, installed_apps(&shared)).subscriptions,
        vec![]
    );
    assert_eq!(
        probe(&env, &shared, installed_apps(&shared)),
        Err(ProbeNotificationPathError::NotReachable)
    );
}

#[test]
fn should_remove_no_installed_app_on_an_entry_that_is_not_linked() {
    let env = env();
    let phone = phone(&env);
    deliver(&env, phone.canister_id, phone.account_principal, 1, 201);

    remove_unanswered(&env, &phone);

    let mut pushed = deliver(&env, phone.canister_id, phone.account_principal, 2, 201);
    pushed.sort();
    assert_eq!(
        pushed,
        vec![BROWSER_ENDPOINT.to_string(), PWA_ENDPOINT.to_string()]
    );
}

// ---- Who may call ----

#[test]
fn should_refuse_the_app_methods_without_a_live_session() -> Result<(), RejectResponse> {
    let env = env();
    let phone = phone(&env);

    for stranger in [
        principal_1(),
        safari(),
        principal_2(),
        Principal::anonymous(),
    ] {
        assert_eq!(
            app_get_same_device_link_status(&env, phone.canister_id, stranger)?,
            Err(AppGetSameDeviceLinkStatusError::NoSuchSession)
        );
        assert_eq!(
            app_prepare_same_device_link(
                &env,
                phone.canister_id,
                stranger,
                AppPrepareSameDeviceLinkRequest {
                    link_public_key: link_key().public_key(),
                },
            )?,
            Err(AppPrepareSameDeviceLinkError::NoSuchSession)
        );
        assert_eq!(
            app_probe_pwa_webpush_subscription(&env, phone.canister_id, stranger)?,
            Err(AppProbePwaWebPushSubscriptionError::NoSuchSession)
        );
    }
    Ok(())
}

#[test]
fn should_refuse_the_browser_methods_to_a_caller_that_is_no_browser_of_the_identity(
) -> Result<(), RejectResponse> {
    let env = env();
    let phone = linked_phone(&env);
    second_identity_at(&env, phone.canister_id, APP);
    let other_identitys_browser = BrowserKey::new(2).successor().principal();
    deliver(&env, phone.canister_id, phone.account_principal, 1, 201);

    for stranger in [
        principal_1(),
        other_identitys_browser,
        principal_2(),
        Principal::anonymous(),
    ] {
        assert_eq!(
            get_notification_path_status(
                &env,
                phone.canister_id,
                stranger,
                GetNotificationPathStatusRequest {
                    anchor_number: phone.anchor,
                    path: installed_apps(&phone),
                },
            )?,
            Err(GetNotificationPathStatusError::InvalidBrowserKey)
        );
        assert_eq!(
            probe_notification_path(
                &env,
                phone.canister_id,
                stranger,
                ProbeNotificationPathRequest {
                    anchor_number: phone.anchor,
                    path: installed_apps(&phone),
                },
            )?,
            Err(ProbeNotificationPathError::InvalidBrowserKey)
        );
        assert_eq!(
            remove_unanswered_pwa_webpush_subscriptions(
                &env,
                phone.canister_id,
                stranger,
                RemoveUnansweredPwaWebPushSubscriptionsRequest {
                    anchor_number: phone.anchor,
                    origin: APP.to_string(),
                    account_principal: phone.account_principal,
                },
            )?,
            Err(RemoveUnansweredPwaWebPushSubscriptionsError::InvalidBrowserKey)
        );
        assert_eq!(
            claim(&env, &phone, stranger, &link_key()),
            Err(ClaimSameDeviceLinkError::InvalidBrowserKey)
        );
    }
    // None of them probed or removed anything.
    assert_eq!(answer_outcalls(&env, 201), Vec::<String>::new());
    assert_eq!(
        count(&unanswered(&env, &phone, installed_apps(&phone))),
        vec![Some((1, false))]
    );
    Ok(())
}

#[test]
fn should_refuse_the_new_methods_while_notifications_are_disabled() -> Result<(), RejectResponse> {
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
        app_get_same_device_link_status(&env, canister_id, session)?,
        Err(AppGetSameDeviceLinkStatusError::InternalCanisterError(_))
    ));
    assert!(matches!(
        app_prepare_same_device_link(
            &env,
            canister_id,
            session,
            AppPrepareSameDeviceLinkRequest {
                link_public_key: link_key().public_key(),
            },
        )?,
        Err(AppPrepareSameDeviceLinkError::InternalCanisterError(_))
    ));
    assert!(matches!(
        app_probe_pwa_webpush_subscription(&env, canister_id, session)?,
        Err(AppProbePwaWebPushSubscriptionError::InternalCanisterError(
            _
        ))
    ));
    let path_request = || GetNotificationPathStatusRequest {
        anchor_number: anchor,
        path: NotificationPath::ThisBrowser,
    };
    assert!(matches!(
        get_notification_path_status(&env, canister_id, safari(), path_request())?,
        Err(GetNotificationPathStatusError::InternalCanisterError(_))
    ));
    assert!(matches!(
        probe_notification_path(
            &env,
            canister_id,
            safari(),
            ProbeNotificationPathRequest {
                anchor_number: anchor,
                path: NotificationPath::ThisBrowser,
            },
        )?,
        Err(ProbeNotificationPathError::InternalCanisterError(_))
    ));
    assert!(matches!(
        remove_unanswered_pwa_webpush_subscriptions(
            &env,
            canister_id,
            safari(),
            RemoveUnansweredPwaWebPushSubscriptionsRequest {
                anchor_number: anchor,
                origin: APP.to_string(),
                account_principal: principal_1(),
            },
        )?,
        Err(RemoveUnansweredPwaWebPushSubscriptionsError::InternalCanisterError(_))
    ));
    assert!(matches!(
        claim_same_device_link(
            &env,
            canister_id,
            safari(),
            ClaimSameDeviceLinkRequest {
                anchor_number: anchor,
                link_public_key: link_key().public_key(),
                signature: link_key().sign_same_device_link_claim(anchor, safari()),
            },
        )?,
        Err(ClaimSameDeviceLinkError::InternalCanisterError(_))
    ));
    Ok(())
}
