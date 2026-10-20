//! Tests for the notification API, exercised through Candid against a canister.

use candid::Principal;
use canister_tests::api::internet_identity::api_v2::prepare_account_session;
use canister_tests::api::internet_identity::notifications::{
    browser_get_next_notification, browser_remove_notification, consent_granted, grant_consent,
    link_notification_app, prepare_notification_app_link, revoke_consent,
};
use canister_tests::flows;
use canister_tests::framework::{
    arg_with_captcha_disabled, arg_with_notifications_enabled, device_data_2, env,
    install_ii_canister_with_arg, principal_1, principal_2, upgrade_ii_canister,
    upgrade_ii_canister_with_arg, BrowserKey, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserBrand, BrowserDescription, FormFactor, GetNextNotificationError,
    GetNextNotificationRequest, GetNextNotificationResponse, InternetIdentityInit,
    LinkNotificationAppRequest, NotificationGrantConsentError, NotificationRevokeConsentError,
    NotificationToShow, OperatingSystem, PrepareAccountSessionRequest,
    PrepareNotificationAppLinkRequest, RemoveNotificationError, RemoveNotificationRequest,
    RemoveNotificationResponse,
};
use pocket_ic::{PocketIc, RejectResponse};
use serde_bytes::ByteBuf;

const ORIGIN: &str = "https://some-dapp.com";
const GATEWAY: &str = "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app";
const UNREACHED: &str = "https://never-visited.example";
/// A canister that notifies, and one anchor registered to `principal_1` which has
/// signed in at `ORIGIN`.
fn install_with_anchor(env: &PocketIc) -> (CanisterId, AnchorNumber) {
    let canister_id =
        install_ii_canister_with_arg(env, II_WASM.clone(), arg_with_notifications_enabled());
    let anchor = flows::register_anchor(env, canister_id);
    sign_in_at(env, canister_id, anchor, ORIGIN, 1);
    (canister_id, anchor)
}

fn chrome_on_a_mac() -> BrowserDescription {
    BrowserDescription {
        brand: BrowserBrand::Chrome,
        os: OperatingSystem::Macos,
        form_factor: FormFactor::Desktop,
        model: None,
    }
}

/// Signs `anchor` in at `origin`, which puts the origin in the application registry.
/// `browser_seed` names the browser: a key rotates on every sign-in, so a repeat sign-in
/// needs a browser of its own.
fn sign_in_at(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor: AnchorNumber,
    origin: &str,
    browser_seed: u8,
) {
    sign_in_as(
        env,
        canister_id,
        principal_1(),
        anchor,
        origin,
        browser_seed,
    );
}

/// As [`sign_in_at`], for an identity `owner` rather than [`principal_1`] registered.
fn sign_in_as(
    env: &PocketIc,
    canister_id: CanisterId,
    owner: Principal,
    anchor: AnchorNumber,
    origin: &str,
    browser_seed: u8,
) {
    let browser = BrowserKey::new(browser_seed);
    let session_key = ByteBuf::from(vec![1; 32]);
    let next_browser_key = browser.successor().public_key();
    prepare_account_session(
        env,
        canister_id,
        owner,
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
        },
    )
    .expect("prepare_account_session rejected")
    .expect("prepare_account_session refused");
}

#[test]
fn should_refuse_every_entry_point_while_notifications_are_disabled() -> Result<(), RejectResponse>
{
    let env = env();
    // The default arg, which leaves notifications disabled.
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_captcha_disabled());
    let anchor = flows::register_anchor(&env, canister_id);

    assert!(matches!(
        grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?,
        Err(NotificationGrantConsentError::InternalCanisterError(_))
    ));
    assert!(matches!(
        revoke_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?,
        Err(NotificationRevokeConsentError::InternalCanisterError(_))
    ));
    // A disabled deployment answers like one nobody consented on.
    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    assert!(matches!(
        browser_get_next_notification(&env, canister_id, principal_1(), next_request(anchor))?,
        Err(GetNextNotificationError::InternalCanisterError(_))
    ));
    assert!(matches!(
        browser_remove_notification(&env, canister_id, principal_1(), remove_request(anchor))?,
        Err(RemoveNotificationError::InternalCanisterError(_))
    ));
    Ok(())
}

#[test]
fn should_refuse_a_caller_that_does_not_own_the_anchor() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    let granted = grant_consent(&env, canister_id, principal_2(), anchor, ORIGIN.into())?;
    assert!(matches!(
        granted,
        Err(NotificationGrantConsentError::Unauthorized(_))
    ));

    // And the refusal wrote nothing.
    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

#[test]
fn should_not_answer_a_query_for_an_anchor_the_caller_does_not_own() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("grant rejected");

    assert!(!consent_granted(
        &env,
        canister_id,
        principal_2(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

#[test]
fn should_record_and_revoke_consent() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);

    grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("grant rejected");
    assert!(consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);

    revoke_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("revoke rejected");
    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

#[test]
fn should_fold_the_gateway_twins_of_one_app_into_one_consent() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    // Signed in through the legacy spelling, which is the one the frontend remaps to.
    sign_in_at(
        &env,
        canister_id,
        anchor,
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app",
        2,
    );

    grant_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp0.io".into(),
    )?
    .expect("grant rejected");

    // Another gateway for the same canister is the same app.
    for twin in [
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app",
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp.net",
    ] {
        assert!(consent_granted(
            &env,
            canister_id,
            principal_1(),
            anchor,
            twin.into()
        )?);
    }
    // Granting through a twin must land on that same row rather than adding one.
    grant_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app".into(),
    )?
    .expect("grant rejected");

    // So one revoke, through a third spelling, clears them all.
    revoke_consent(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp.net".into(),
    )?
    .expect("revoke rejected");
    for spelling in [
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp0.io",
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app",
        "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp.net",
    ] {
        assert!(
            !consent_granted(&env, canister_id, principal_1(), anchor, spelling.into())?,
            "{spelling} still reports consent after the single revoke"
        );
    }
    Ok(())
}

#[test]
fn should_reject_an_origin_that_is_not_a_bare_authority() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    for origin in [
        "https://some-dapp.com/path",
        "https://some-dapp.com?query",
        "https://some-dapp.com#fragment",
        "https://",
        "",
        "some-dapp.com",
        "https://some-dapp.com:port",
    ] {
        let result = grant_consent(&env, canister_id, principal_1(), anchor, origin.into())?;
        assert!(
            matches!(
                result,
                Err(NotificationGrantConsentError::InternalCanisterError(_))
            ),
            "{origin} was not rejected: {result:?}"
        );
    }
    // None of the refusals wrote a row under the origin they canonicalize to.
    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

/// Consent is only the key an app's notifications are filed under, so any scheme may
/// hold it. Whether the app can send is decided when it does.
#[test]
fn should_grant_consent_at_a_plain_http_origin() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    let origin = "http://some-dapp.com";

    grant_consent(&env, canister_id, principal_1(), anchor, origin.into())?
        .expect("consent refused");
    assert!(consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        origin.into()
    )?);
    Ok(())
}

/// An app can ask before the identity has ever signed in at it, so the grant mints the
/// application it hangs off rather than refusing.
#[test]
fn should_grant_consent_for_an_app_the_identity_has_never_reached() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    grant_consent(&env, canister_id, principal_1(), anchor, UNREACHED.into())?
        .expect("granting at an unreached app");
    assert!(consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        UNREACHED.into()
    )?);
    Ok(())
}

/// The map lives in stable memory under an index chosen for this feature, and an
/// upgrade that lost it would be unrecoverable after release.
#[test]
fn should_keep_consent_across_an_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("grant rejected");

    // No argument, which is what an upgrade that changes nothing sends.
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    assert!(consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

/// The switch can be turned off again: consent held from before is then neither
/// changed nor reported.
#[test]
fn should_refuse_consent_once_notifications_are_disabled() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);
    grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
        .expect("consent refused");

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            notifications_enabled: Some(false),
            ..Default::default()
        }),
    )?;

    assert!(matches!(
        grant_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?,
        Err(NotificationGrantConsentError::InternalCanisterError(_))
    ));
    assert!(matches!(
        revoke_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?,
        Err(NotificationRevokeConsentError::InternalCanisterError(_))
    ));
    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        ORIGIN.into()
    )?);
    Ok(())
}

mod subscriptions {
    use super::*;
    use canister_tests::api::internet_identity::api_v2::prepare_account_session;
    use canister_tests::api::internet_identity::notifications::{
        remove_webpush_subscription, set_webpush_subscription,
    };
    use canister_tests::framework::BrowserKey;
    use internet_identity_interface::internet_identity::types::{
        BrowserBrand, BrowserDescription, BrowserId, FormFactor, OperatingSystem,
        PrepareAccountSessionRequest, RemoveWebPushSubscriptionError, SetWebPushSubscriptionError,
        SetWebPushSubscriptionRequest,
    };
    use pretty_assertions::assert_eq;
    use serde_bytes::ByteBuf;

    pub(super) const ENDPOINT: &str = "https://push.example.com/aBcDeF";
    pub(super) const ISSUED_AT_NS: u64 = 1_700_000_000_000_000_000;

    /// An uncompressed SEC1 point on P-256, which is what a browser mints.
    fn vapid_public_key() -> ByteBuf {
        use p256::elliptic_curve::sec1::ToEncodedPoint;
        let key = p256::SecretKey::from_bytes(&[7u8; 32].into()).expect("bad scalar");
        ByteBuf::from(key.public_key().to_encoded_point(false).as_bytes().to_vec())
    }

    fn jwt_pool() -> Vec<ByteBuf> {
        (0..30u8).map(|i| ByteBuf::from(vec![i; 64])).collect()
    }

    fn session_request(anchor: AnchorNumber, browser: &BrowserKey) -> PrepareAccountSessionRequest {
        let session_key = ByteBuf::from(vec![1; 32]);
        let next_browser_key = browser.successor().public_key();
        PrepareAccountSessionRequest {
            identity_number: anchor,
            origin: ORIGIN.to_string(),
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
        }
    }

    /// Signs a browser in, which puts it in the registry a subscription is keyed by.
    pub(super) fn sign_browser_in(
        env: &PocketIc,
        canister_id: CanisterId,
        anchor: AnchorNumber,
        browser: &BrowserKey,
    ) -> BrowserId {
        prepare_account_session(
            env,
            canister_id,
            principal_1(),
            session_request(anchor, browser),
        )
        .expect("prepare_account_session rejected")
        .expect("prepare_account_session returned Err")
        .browser_id
    }

    pub(super) fn install_with_browser(
        env: &PocketIc,
    ) -> (CanisterId, AnchorNumber, BrowserKey, BrowserId) {
        let canister_id =
            install_ii_canister_with_arg(env, II_WASM.clone(), arg_with_notifications_enabled());
        let anchor = flows::register_anchor(env, canister_id);
        let browser = BrowserKey::new(1);
        let browser_id = sign_browser_in(env, canister_id, anchor, &browser);
        (canister_id, anchor, browser, browser_id)
    }

    pub(super) fn request(anchor: AnchorNumber, endpoint: &str) -> SetWebPushSubscriptionRequest {
        SetWebPushSubscriptionRequest {
            anchor_number: anchor,
            endpoint: endpoint.to_string(),
            vapid_public_key: vapid_public_key(),
            jwt_signatures: jwt_pool(),
            jwt_issued_at_ns: ISSUED_AT_NS,
        }
    }

    /// The browser key a registered browser keeps between sign-ins, and so signs its
    /// subscription with.
    pub(super) fn key_holder(browser: &BrowserKey) -> BrowserKey {
        browser.successor()
    }

    #[test]
    fn should_refuse_to_subscribe_while_notifications_are_disabled() -> Result<(), RejectResponse> {
        let env = env();
        let canister_id =
            install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_captcha_disabled());
        let anchor = flows::register_anchor(&env, canister_id);
        let browser = BrowserKey::new(1);
        sign_browser_in(&env, canister_id, anchor, &browser);

        assert!(matches!(
            set_webpush_subscription(
                &env,
                canister_id,
                key_holder(&browser).principal(),
                request(anchor, ENDPOINT)
            )?,
            Err(SetWebPushSubscriptionError::InternalCanisterError(_))
        ));
        Ok(())
    }

    #[test]
    fn should_subscribe_and_unsubscribe_a_browser() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, browser_id) = install_with_browser(&env);

        set_webpush_subscription(
            &env,
            canister_id,
            key_holder(&browser).principal(),
            request(anchor, ENDPOINT),
        )?
        .expect("subscribe rejected");

        remove_webpush_subscription(&env, canister_id, principal_1(), anchor, browser_id)?
            .expect("unsubscribe rejected");
        // Idempotent on purpose, since silencing a browser is done from another one.
        remove_webpush_subscription(&env, canister_id, principal_1(), anchor, browser_id)?
            .expect("a second unsubscribe should be a no-op, not an error");
        Ok(())
    }

    /// One call both registers and tops up, so a browser replaying a pool it already
    /// uploaded must not be able to walk its own coverage backwards.
    #[test]
    fn should_replace_the_pool_only_with_a_newer_one() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_with_browser(&env);
        let caller = key_holder(&browser).principal();

        set_webpush_subscription(&env, canister_id, caller, request(anchor, ENDPOINT))?
            .expect("subscribe rejected");

        let mut topped_up = request(anchor, ENDPOINT);
        topped_up.jwt_issued_at_ns = ISSUED_AT_NS + 1;
        set_webpush_subscription(&env, canister_id, caller, topped_up)?
            .expect("a newer pool should replace the stored one");

        let replayed =
            set_webpush_subscription(&env, canister_id, caller, request(anchor, ENDPOINT))?;
        assert!(
            matches!(replayed, Err(SetWebPushSubscriptionError::StaleJwtPool)),
            "{replayed:?}"
        );
        Ok(())
    }

    /// The caller's key is what names the row, so a key the registry has never seen
    /// cannot write one.
    #[test]
    fn should_refuse_a_browser_this_identity_is_not_signed_in_from() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _browser, _) = install_with_browser(&env);

        let stranger = BrowserKey::new(9);
        let refused = set_webpush_subscription(
            &env,
            canister_id,
            stranger.principal(),
            request(anchor, ENDPOINT),
        )?;
        assert!(
            matches!(refused, Err(SetWebPushSubscriptionError::InvalidBrowserKey)),
            "{refused:?}"
        );
        Ok(())
    }

    /// The key a browser presented at sign-in is one it has already discarded, so
    /// accepting it would keep a copy taken off the wire usable.
    #[test]
    fn should_refuse_the_key_a_sign_in_retired() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_with_browser(&env);

        let refused = set_webpush_subscription(
            &env,
            canister_id,
            browser.principal(),
            request(anchor, ENDPOINT),
        )?;
        assert!(
            matches!(refused, Err(SetWebPushSubscriptionError::InvalidBrowserKey)),
            "{refused:?}"
        );
        Ok(())
    }

    /// An access method authorizes the identity, not a browser of it, so it cannot
    /// write a subscription no browser asked for.
    #[test]
    fn should_refuse_an_access_method_that_is_not_a_browser() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _browser, _) = install_with_browser(&env);

        let refused =
            set_webpush_subscription(&env, canister_id, principal_1(), request(anchor, ENDPOINT))?;
        assert!(
            matches!(refused, Err(SetWebPushSubscriptionError::InvalidBrowserKey)),
            "{refused:?}"
        );
        Ok(())
    }

    #[test]
    fn should_report_every_invalid_field_at_once() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_with_browser(&env);

        let mut sent = request(anchor, "");
        sent.vapid_public_key = ByteBuf::from(vec![4u8; 65]);
        sent.jwt_signatures = vec![];

        match set_webpush_subscription(&env, canister_id, key_holder(&browser).principal(), sent)? {
            Err(SetWebPushSubscriptionError::InternalCanisterError(problems)) => {
                // Failing on the first would send a browser round three times to
                // learn what a single answer can say.
                assert_eq!(problems.split("; ").count(), 3, "{problems:?}");
            }
            other => panic!("expected every field reported at once, got {other:?}"),
        }
        Ok(())
    }

    #[test]
    fn should_refuse_to_unsubscribe_for_an_anchor_the_caller_does_not_own(
    ) -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _browser, browser_id) = install_with_browser(&env);

        let refused =
            remove_webpush_subscription(&env, canister_id, principal_2(), anchor, browser_id)?;
        assert!(
            matches!(
                refused,
                Err(RemoveWebPushSubscriptionError::Unauthorized(_))
            ),
            "{refused:?}"
        );
        Ok(())
    }
}

/// What a browser registered and the pool it signed, read through
/// `get_webpush_subscription_status`.
mod subscription_status {
    use super::subscriptions::*;
    use super::*;
    use canister_tests::api::internet_identity::api_v2::revoke_browser_sessions;
    use canister_tests::api::internet_identity::notifications::{
        get_webpush_subscription_status, set_webpush_subscription,
    };
    use canister_tests::framework::BrowserKey;
    use internet_identity_interface::internet_identity::types::{
        RevokeBrowserSessionsRequest, SetWebPushSubscriptionError, WebPushSubscriptionStatus,
    };
    use pretty_assertions::assert_eq;

    const LATER_NS: u64 = ISSUED_AT_NS + 1_000_000;

    fn subscribe(
        env: &PocketIc,
        canister_id: CanisterId,
        anchor: AnchorNumber,
        browser: &BrowserKey,
    ) -> Result<(), RejectResponse> {
        set_webpush_subscription(
            env,
            canister_id,
            key_holder(browser).principal(),
            request(anchor, ENDPOINT),
        )?
        .expect("subscribe rejected");
        Ok(())
    }

    /// Signed as the browser, which is what the canister reads the registration off.
    fn status(
        env: &PocketIc,
        canister_id: CanisterId,
        anchor: AnchorNumber,
        browser: &BrowserKey,
    ) -> Result<Option<WebPushSubscriptionStatus>, RejectResponse> {
        get_webpush_subscription_status(env, canister_id, key_holder(browser).principal(), anchor)
    }

    #[test]
    fn should_report_nothing_for_a_browser_that_never_subscribed() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _browser_id) = install_with_browser(&env);

        assert_eq!(status(&env, canister_id, anchor, &browser)?, None);
        Ok(())
    }

    #[test]
    fn should_report_the_pool_a_browser_uploaded() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _browser_id) = install_with_browser(&env);
        subscribe(&env, canister_id, anchor, &browser)?;

        let reported =
            status(&env, canister_id, anchor, &browser)?.expect("no registration reported");
        // The endpoint, so a browser can tell its own registration from one another
        // identity's re-subscribe left behind. Then windows covered and issue time,
        // not a count of unused signatures, which would sit at 30 forever.
        assert_eq!(reported.endpoint, ENDPOINT);
        assert_eq!(reported.pool_len, 30);
        assert_eq!(reported.issued_at_ns, ISSUED_AT_NS);
        Ok(())
    }

    #[test]
    fn should_replace_the_pool_rather_than_grow_it() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _browser_id) = install_with_browser(&env);
        subscribe(&env, canister_id, anchor, &browser)?;

        let mut topped_up = request(anchor, ENDPOINT);
        topped_up.jwt_issued_at_ns = LATER_NS;
        set_webpush_subscription(
            &env,
            canister_id,
            key_holder(&browser).principal(),
            topped_up,
        )?
        .expect("top-up rejected");

        let reported = status(&env, canister_id, anchor, &browser)?.expect("no pool reported");
        assert_eq!(
            reported.pool_len, 30,
            "the pool grew instead of being replaced"
        );
        assert_eq!(reported.issued_at_ns, LATER_NS);
        Ok(())
    }

    /// The registration rides on the browser entry in the anchor, and an upgrade that
    /// lost it would be unrecoverable after release.
    #[test]
    fn should_keep_a_subscription_across_an_upgrade() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _browser_id) = install_with_browser(&env);
        subscribe(&env, canister_id, anchor, &browser)?;

        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        let reported = status(&env, canister_id, anchor, &browser)?
            .expect("the subscription did not survive the upgrade");
        assert_eq!(reported.issued_at_ns, ISSUED_AT_NS);
        Ok(())
    }

    /// A browser the identity signed out of must stop being notified for it, even
    /// though the entry itself stays so that signing back in is not a new browser.
    #[test]
    fn should_drop_the_subscription_when_the_browser_is_signed_out() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, browser_id) = install_with_browser(&env);
        subscribe(&env, canister_id, anchor, &browser)?;

        revoke_browser_sessions(
            &env,
            canister_id,
            principal_1(),
            RevokeBrowserSessionsRequest {
                identity_number: anchor,
                browser_id,
            },
        )?
        .expect("revoke_browser_sessions rejected");

        assert_eq!(
            status(&env, canister_id, anchor, &browser)?,
            None,
            "a signed-out browser kept its subscription"
        );
        Ok(())
    }

    /// The registry holds twenty browsers and evicts the oldest, and the subscription
    /// goes with it.
    #[test]
    fn should_drop_the_subscription_of_an_evicted_browser() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, first, _first_id) = install_with_browser(&env);
        subscribe(&env, canister_id, anchor, &first)?;
        assert!(status(&env, canister_id, anchor, &first)?.is_some());

        // Twenty more browsers, so the first is the one the cap retires.
        for seed in 2..=21u8 {
            sign_browser_in(&env, canister_id, anchor, &BrowserKey::new(seed));
        }
        // Signing back in is a fresh entry, so a subscription that outlived the one
        // that was evicted would be reported here.
        sign_browser_in(&env, canister_id, anchor, &first);

        assert_eq!(
            status(&env, canister_id, anchor, &first)?,
            None,
            "an evicted browser kept its subscription"
        );
        Ok(())
    }

    /// A browser reads its own registration, so an access method of the identity is
    /// as much a stranger to it as anyone else.
    #[test]
    fn should_not_report_a_pool_to_a_caller_that_is_not_the_browser() -> Result<(), RejectResponse>
    {
        let env = env();
        let (canister_id, anchor, browser, _browser_id) = install_with_browser(&env);
        subscribe(&env, canister_id, anchor, &browser)?;

        for caller in [principal_1(), principal_2(), BrowserKey::new(9).principal()] {
            assert_eq!(
                get_webpush_subscription_status(&env, canister_id, caller, anchor)?,
                None,
                "{caller} was answered"
            );
        }
        Ok(())
    }

    /// An access method authorizes the identity, not a browser of it, so a refused
    /// caller must leave the pool the browser signed exactly where it was.
    #[test]
    fn should_leave_the_stored_pool_alone_when_the_caller_is_not_the_browser(
    ) -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _browser_id) = install_with_browser(&env);
        subscribe(&env, canister_id, anchor, &browser)?;

        let mut topped_up = request(anchor, ENDPOINT);
        topped_up.jwt_issued_at_ns = LATER_NS;
        for caller in [principal_1(), principal_2(), BrowserKey::new(9).principal()] {
            let refused = set_webpush_subscription(&env, canister_id, caller, topped_up.clone())?;
            assert!(
                matches!(refused, Err(SetWebPushSubscriptionError::InvalidBrowserKey)),
                "{refused:?}"
            );
        }

        let reported = status(&env, canister_id, anchor, &browser)?.expect("no pool reported");
        assert_eq!(reported.issued_at_ns, ISSUED_AT_NS);
        Ok(())
    }
}

mod pull_delegation {
    use super::subscriptions::{install_with_browser, key_holder, ENDPOINT};
    use super::*;
    use canister_tests::api::internet_identity::api_v2::{
        prepare_account_delegation, AccountDelegationParams,
    };
    use canister_tests::api::internet_identity::notifications::{
        get_notification_delegation, prepare_notification_delegation, remove_webpush_subscription,
        revoke_consent, set_webpush_subscription,
    };
    use canister_tests::framework::{verify_delegation, verify_icrc3_attributes};
    use internet_identity_interface::internet_identity::types::{
        BrowserId, GetNotificationDelegationRequest, NotificationDelegationError,
        PrepareNotificationDelegationRequest, PrepareNotificationDelegationResponse,
    };

    const SESSION_KEY: &[u8] = b"notification session public key";
    const EIGHT_HOURS_NS: u64 = 8 * 60 * 60 * 1_000_000_000;

    fn prepare_request(anchor: AnchorNumber) -> PrepareNotificationDelegationRequest {
        PrepareNotificationDelegationRequest {
            anchor_number: anchor,
            origin: ORIGIN.into(),
            account_number: None,
            session_key: ByteBuf::from(SESSION_KEY),
        }
    }

    fn get_request(anchor: AnchorNumber, expiration: u64) -> GetNotificationDelegationRequest {
        GetNotificationDelegationRequest {
            anchor_number: anchor,
            origin: ORIGIN.into(),
            account_number: None,
            session_key: ByteBuf::from(SESSION_KEY),
            expiration,
        }
    }

    /// A delegation only exists for a browser that can be notified, so every
    /// test here registers the browser for Web Push and consents to the app.
    fn install_notifiable(
        env: &PocketIc,
    ) -> (
        CanisterId,
        AnchorNumber,
        canister_tests::framework::BrowserKey,
        BrowserId,
    ) {
        let (canister_id, anchor, browser, browser_id) = install_with_browser(env);
        set_webpush_subscription(
            env,
            canister_id,
            key_holder(&browser).principal(),
            super::subscriptions::request(anchor, ENDPOINT),
        )
        .expect("set_webpush_subscription rejected")
        .expect("set_webpush_subscription returned Err");
        grant_consent(env, canister_id, principal_1(), anchor, ORIGIN.into())
            .expect("grant_consent rejected")
            .expect("grant_consent returned Err");
        (canister_id, anchor, browser, browser_id)
    }

    fn prepare(
        env: &PocketIc,
        canister_id: CanisterId,
        caller: Principal,
        anchor: AnchorNumber,
    ) -> PrepareNotificationDelegationResponse {
        prepare_notification_delegation(env, canister_id, caller, prepare_request(anchor))
            .expect("prepare_notification_delegation rejected")
            .expect("prepare_notification_delegation returned Err")
    }

    #[test]
    fn should_mint_a_delegation_and_its_sender_info() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();

        let prepared = prepare(&env, canister_id, caller, anchor);
        let delegation = get_notification_delegation(
            &env,
            canister_id,
            caller,
            get_request(anchor, prepared.expiration),
        )?
        .expect("get_notification_delegation returned Err");

        assert_eq!(
            delegation.signed_delegation.delegation.pubkey,
            ByteBuf::from(SESSION_KEY)
        );
        assert_eq!(
            delegation.signed_delegation.delegation.expiration,
            prepared.expiration
        );

        // Both are canister signatures the IC verifies against the user key
        // before the app runs, so verify them the same way here: a wrong
        // message, domain or seed fails in the test rather than in production.
        let root_key = env.root_key().expect("no root key");
        verify_delegation(
            &env,
            prepared.user_key.clone(),
            &delegation.signed_delegation,
            &root_key,
        );
        verify_icrc3_attributes(
            &env,
            prepared.user_key,
            &prepared.sender_info,
            &delegation.sender_info_signature,
            &root_key,
        );
        Ok(())
    }

    /// The delegation is minted under a seed of its own, so the app is called
    /// by a principal that holds none of the account's authority — and the
    /// sender_info is what tells it which account it is serving.
    #[test]
    fn should_not_be_the_account_principal() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();

        let prepared = prepare(&env, canister_id, caller, anchor);
        let pull_principal = Principal::self_authenticating(&prepared.user_key);
        let account_delegation = prepare_account_delegation(
            &AccountDelegationParams {
                env: &env,
                canister_id,
                sender: principal_1(),
                identity_number: anchor,
                origin: ORIGIN.into(),
                account_number: None,
                session_key: ByteBuf::from(SESSION_KEY),
            },
            None,
        )?
        .expect("prepare_account_delegation returned Err");
        let account_principal = Principal::self_authenticating(&account_delegation.user_key);

        assert_ne!(pull_principal, account_principal);
        let account_bytes = account_principal.as_slice();
        assert!(
            prepared
                .sender_info
                .windows(account_bytes.len())
                .any(|window| window == account_bytes),
            "sender_info names the account principal"
        );
        Ok(())
    }

    /// Fixed lifetime: the pull happens long after the push, and preparing one
    /// costs an update plus a query.
    #[test]
    fn should_expire_after_eight_hours() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();

        let before = env.get_time().as_nanos_since_unix_epoch();
        let prepared = prepare(&env, canister_id, caller, anchor);
        let after = env.get_time().as_nanos_since_unix_epoch();

        // The canister stamps it from its own clock, which advances while the
        // call runs, so the window is the tick rather than a single instant.
        assert!(prepared.expiration >= before + EIGHT_HOURS_NS);
        assert!(prepared.expiration <= after + EIGHT_HOURS_NS);
        Ok(())
    }

    /// Two apps get different principals, so they cannot link the same user.
    #[test]
    fn should_differ_per_origin() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();
        sign_in_at(&env, canister_id, anchor, GATEWAY, 9);
        grant_consent(&env, canister_id, principal_1(), anchor, GATEWAY.into())?
            .expect("grant_consent returned Err");

        let at_origin = prepare(&env, canister_id, caller, anchor);
        let at_gateway = prepare_notification_delegation(
            &env,
            canister_id,
            caller,
            PrepareNotificationDelegationRequest {
                origin: GATEWAY.into(),
                ..prepare_request(anchor)
            },
        )?
        .expect("prepare_notification_delegation returned Err");

        assert_ne!(at_origin.user_key, at_gateway.user_key);
        assert_ne!(at_origin.sender_info, at_gateway.sender_info);
        Ok(())
    }

    #[test]
    fn should_refuse_a_caller_that_is_no_browser_of_the_identity() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _, _) = install_notifiable(&env);

        assert!(matches!(
            prepare_notification_delegation(
                &env,
                canister_id,
                principal_2(),
                prepare_request(anchor)
            )?,
            Err(NotificationDelegationError::NoNotificationAccess)
        ));
        assert!(matches!(
            get_notification_delegation(&env, canister_id, principal_2(), get_request(anchor, 0))?,
            Err(NotificationDelegationError::NoNotificationAccess)
        ));
        Ok(())
    }

    /// The same app reached through a gateway twin is the same account, so the
    /// origin folds to the spelling sign-in and consent key on.
    #[test]
    fn should_fold_a_gateway_twin_to_the_same_delegation() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();
        sign_in_at(&env, canister_id, anchor, GATEWAY, 9);
        grant_consent(&env, canister_id, principal_1(), anchor, GATEWAY.into())?
            .expect("grant_consent returned Err");

        let legacy = prepare_notification_delegation(
            &env,
            canister_id,
            caller,
            PrepareNotificationDelegationRequest {
                origin: GATEWAY.into(),
                ..prepare_request(anchor)
            },
        )?
        .expect("prepare_notification_delegation returned Err");
        let modern = prepare_notification_delegation(
            &env,
            canister_id,
            caller,
            PrepareNotificationDelegationRequest {
                origin: "https://abcde-aaaaa-aaaaa-aaaaa-cai.icp0.io".into(),
                ..prepare_request(anchor)
            },
        )?
        .expect("prepare_notification_delegation returned Err");

        assert_eq!(legacy.user_key, modern.user_key);
        assert_eq!(legacy.sender_info, modern.sender_info);
        Ok(())
    }

    /// Turning notifications off mints nothing more for a browser that could before.
    #[test]
    fn should_refuse_once_notifications_are_disabled() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();
        upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                notifications_enabled: Some(false),
                ..Default::default()
            }),
        )?;

        assert!(matches!(
            prepare_notification_delegation(&env, canister_id, caller, prepare_request(anchor))?,
            Err(NotificationDelegationError::InternalCanisterError(_))
        ));
        assert!(matches!(
            get_notification_delegation(&env, canister_id, caller, get_request(anchor, 0))?,
            Err(NotificationDelegationError::InternalCanisterError(_))
        ));
        Ok(())
    }

    /// Signing the browser out clears its Web Push registration, which is how
    /// an identity cuts a lost device off from minting more of these.
    #[test]
    fn should_refuse_a_browser_that_was_signed_out() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, browser_id) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();

        // Prove it works before the sign-out, so the refusal is the sign-out.
        prepare(&env, canister_id, caller, anchor);
        remove_webpush_subscription(&env, canister_id, principal_1(), anchor, browser_id)?
            .expect("remove_webpush_subscription returned Err");

        assert!(matches!(
            prepare_notification_delegation(&env, canister_id, caller, prepare_request(anchor))?,
            Err(NotificationDelegationError::NoNotificationAccess)
        ));
        Ok(())
    }

    /// Withdrawing the app's consent closes it too, which is the other lever
    /// the identity has from another device.
    #[test]
    fn should_refuse_an_app_whose_consent_was_withdrawn() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();

        let prepared = prepare(&env, canister_id, caller, anchor);
        revoke_consent(&env, canister_id, principal_1(), anchor, ORIGIN.into())?
            .expect("revoke_consent returned Err");

        assert!(matches!(
            prepare_notification_delegation(&env, canister_id, caller, prepare_request(anchor))?,
            Err(NotificationDelegationError::NoNotificationAccess)
        ));
        // And the one already prepared can no longer be fetched.
        assert!(matches!(
            get_notification_delegation(
                &env,
                canister_id,
                caller,
                get_request(anchor, prepared.expiration)
            )?,
            Err(NotificationDelegationError::NoNotificationAccess)
        ));
        Ok(())
    }

    /// Nothing is stored, so a get for a delegation that was never prepared —
    /// or for a different expiration than the one prepared — finds no
    /// signature rather than minting one.
    #[test]
    fn should_refuse_a_delegation_that_was_never_prepared() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();

        assert!(matches!(
            get_notification_delegation(&env, canister_id, caller, get_request(anchor, 1))?,
            Err(NotificationDelegationError::NoSuchDelegation)
        ));

        let prepared = prepare(&env, canister_id, caller, anchor);
        assert!(matches!(
            get_notification_delegation(
                &env,
                canister_id,
                caller,
                get_request(anchor, prepared.expiration + 1)
            )?,
            Err(NotificationDelegationError::NoSuchDelegation)
        ));
        Ok(())
    }
}

mod browser_queue {
    use super::subscriptions::{install_with_browser, key_holder};
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn should_have_no_next_for_a_browser_nothing_was_sent_to() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_with_browser(&env);

        assert_eq!(
            browser_get_next_notification(
                &env,
                canister_id,
                key_holder(&browser).principal(),
                next_request(anchor)
            )?,
            Ok(GetNextNotificationResponse { notification: None })
        );
        Ok(())
    }

    #[test]
    fn should_remove_nothing_for_a_notification_that_is_not_queued() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_with_browser(&env);

        assert_eq!(
            browser_remove_notification(
                &env,
                canister_id,
                key_holder(&browser).principal(),
                remove_request(anchor)
            )?,
            Ok(RemoveNotificationResponse {})
        );
        Ok(())
    }

    #[test]
    fn should_refuse_a_caller_that_is_no_browser_of_the_identity() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _, _) = install_with_browser(&env);

        assert!(matches!(
            browser_get_next_notification(&env, canister_id, principal_2(), next_request(anchor))?,
            Err(GetNextNotificationError::InternalCanisterError(_))
        ));
        assert!(matches!(
            browser_remove_notification(&env, canister_id, principal_2(), remove_request(anchor))?,
            Err(RemoveNotificationError::InternalCanisterError(_))
        ));
        Ok(())
    }
}

/// A canister that notifies, and two identities signed in from a browser each.
/// The browser each identity signs in from in these fixtures. Named rather than spelled
/// at every call site, because `link_app` takes the seed and the token has to be minted
/// by the same browser the canister will verify it against.
const FIRST_BROWSER_SEED: u8 = 1;
const SECOND_BROWSER_SEED: u8 = 2;

fn install_with_two_anchors(env: &PocketIc) -> (CanisterId, AnchorNumber, AnchorNumber) {
    let canister_id =
        install_ii_canister_with_arg(env, II_WASM.clone(), arg_with_notifications_enabled());
    let first = flows::register_anchor(env, canister_id);
    sign_in_at(env, canister_id, first, ORIGIN, 1);
    let second = flows::register_anchor_with_device(env, canister_id, &device_data_2());
    sign_in_as(env, canister_id, principal_2(), second, ORIGIN, 2);
    (canister_id, first, second)
}

/// Installs the Home Screen app `app_seed` names on `anchor`: the browser `browser_seed`
/// names registers the one-shot key, and the app links as it. Answers the principal the
/// app signs as.
fn link_app(
    env: &PocketIc,
    canister_id: CanisterId,
    anchor: AnchorNumber,
    browser_seed: u8,
    app_seed: u8,
) -> Principal {
    let app = BrowserKey::new(app_seed);
    let link = BrowserKey::new(app_seed.wrapping_add(1));
    prepare_notification_app_link(
        env,
        canister_id,
        // The successor the sign-in announced, the only slot a browser is recognised by.
        BrowserKey::new(browser_seed).successor().principal(),
        PrepareNotificationAppLinkRequest {
            anchor_number: anchor,
            link_principal: link.principal(),
            replacing_existing: false,
        },
    )
    .expect("prepare_notification_app_link rejected")
    .expect("prepare_notification_app_link returned Err");
    link_notification_app(
        env,
        canister_id,
        link.principal(),
        LinkNotificationAppRequest {
            anchor_number: anchor,
            app_key: app.public_key(),
        },
    )
    .expect("link_notification_app rejected")
    .expect("link_notification_app returned Err");
    app.principal()
}

/// Starting an install while this browser already points at an app.
mod installing_over_an_app {
    use super::subscriptions::{request, ENDPOINT};
    use super::*;
    use canister_tests::api::internet_identity::notifications::{
        set_notification_app_enabled, set_webpush_subscription,
    };
    use internet_identity_interface::internet_identity::types::{
        PrepareNotificationAppLinkError, SetNotificationAppEnabledError,
        SetNotificationAppEnabledRequest,
    };
    use pretty_assertions::assert_eq;

    /// Starts an install on the browser `browser_seed` names, naming the one-shot key
    /// `link_seed` does.
    fn prepare(
        env: &PocketIc,
        canister_id: CanisterId,
        anchor: AnchorNumber,
        browser_seed: u8,
        link_seed: u8,
    ) -> Result<Result<(), PrepareNotificationAppLinkError>, RejectResponse> {
        prepare_with(env, canister_id, anchor, browser_seed, link_seed, false)
    }

    fn prepare_with(
        env: &PocketIc,
        canister_id: CanisterId,
        anchor: AnchorNumber,
        browser_seed: u8,
        link_seed: u8,
        replacing_existing: bool,
    ) -> Result<Result<(), PrepareNotificationAppLinkError>, RejectResponse> {
        prepare_notification_app_link(
            env,
            canister_id,
            // The successor the sign-in announced, the only slot a browser is recognised by.
            BrowserKey::new(browser_seed).successor().principal(),
            PrepareNotificationAppLinkRequest {
                anchor_number: anchor,
                link_principal: BrowserKey::new(link_seed).principal(),
                replacing_existing,
            },
        )
    }

    /// The install steps are what a user walks after saying the app is gone, and the
    /// slot an install takes is the one the working app is reached through.
    #[test]
    fn should_refuse_an_install_over_an_app_that_is_delivering() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_anchor(&env);
        let app = link_app(&env, canister_id, anchor, FIRST_BROWSER_SEED, 100);
        set_webpush_subscription(&env, canister_id, app, request(anchor, ENDPOINT))?
            .expect("set_webpush_subscription returned Err");

        assert_eq!(
            prepare(&env, canister_id, anchor, FIRST_BROWSER_SEED, 200)?,
            Err(PrepareNotificationAppLinkError::AppAlreadyDelivering)
        );
        Ok(())
    }

    /// Only the user can see their own Home Screen, and the record cannot: a push service
    /// that goes on answering for a deleted app leaves it saying it delivers forever.
    #[test]
    fn should_install_over_a_delivering_app_the_user_says_is_gone() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_anchor(&env);
        let app = link_app(&env, canister_id, anchor, FIRST_BROWSER_SEED, 100);
        set_webpush_subscription(&env, canister_id, app, request(anchor, ENDPOINT))?
            .expect("set_webpush_subscription returned Err");

        assert_eq!(
            prepare_with(&env, canister_id, anchor, FIRST_BROWSER_SEED, 200, true)?,
            Ok(())
        );
        Ok(())
    }

    /// An install nobody has opened holds the slot but is no app: answering Ok would
    /// tell the caller the switch moved when the flag it writes does not exist yet.
    #[test]
    fn should_refuse_to_switch_an_install_nobody_has_opened() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_anchor(&env);
        prepare(&env, canister_id, anchor, FIRST_BROWSER_SEED, 200)?
            .expect("prepare_notification_app_link returned Err");

        assert_eq!(
            set_notification_app_enabled(
                &env,
                canister_id,
                BrowserKey::new(FIRST_BROWSER_SEED).successor().principal(),
                SetNotificationAppEnabledRequest {
                    anchor_number: anchor,
                    enabled: false,
                },
            )?,
            Err(SetNotificationAppEnabledError::NoNotificationApp)
        );
        Ok(())
    }

    #[test]
    fn should_install_over_an_app_that_never_registered() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_anchor(&env);
        link_app(&env, canister_id, anchor, FIRST_BROWSER_SEED, 100);

        assert_eq!(
            prepare(&env, canister_id, anchor, FIRST_BROWSER_SEED, 200)?,
            Ok(())
        );
        Ok(())
    }

    #[test]
    fn should_install_over_an_app_delivery_is_switched_off_for() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_anchor(&env);
        let app = link_app(&env, canister_id, anchor, FIRST_BROWSER_SEED, 100);
        set_webpush_subscription(&env, canister_id, app, request(anchor, ENDPOINT))?
            .expect("set_webpush_subscription returned Err");
        set_notification_app_enabled(
            &env,
            canister_id,
            BrowserKey::new(FIRST_BROWSER_SEED).successor().principal(),
            SetNotificationAppEnabledRequest {
                anchor_number: anchor,
                enabled: false,
            },
        )?
        .expect("set_notification_app_enabled returned Err");

        assert_eq!(
            prepare(&env, canister_id, anchor, FIRST_BROWSER_SEED, 200)?,
            Ok(())
        );
        Ok(())
    }
}

mod app_anchors {
    use super::*;
    use canister_tests::api::internet_identity::notifications::notification_app_anchors;
    use pretty_assertions::assert_eq;

    #[test]
    fn should_answer_an_app_only_the_identities_that_point_at_it() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, first, second) = install_with_two_anchors(&env);
        let shared = link_app(&env, canister_id, first, FIRST_BROWSER_SEED, 100);
        assert_eq!(
            link_app(&env, canister_id, second, SECOND_BROWSER_SEED, 100),
            shared
        );
        sign_in_as(&env, canister_id, principal_2(), second, ORIGIN, 3);
        let private = link_app(&env, canister_id, second, 3, 101);

        assert_eq!(
            notification_app_anchors(&env, canister_id, shared)?,
            vec![first, second]
        );
        assert_eq!(
            notification_app_anchors(&env, canister_id, private)?,
            vec![second]
        );
        Ok(())
    }

    #[test]
    fn should_answer_nothing_to_a_caller_that_is_no_app() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _) = install_with_two_anchors(&env);
        link_app(&env, canister_id, anchor, FIRST_BROWSER_SEED, 100);

        assert_eq!(
            notification_app_anchors(&env, canister_id, Principal::anonymous())?,
            Vec::<AnchorNumber>::new()
        );
        assert_eq!(
            notification_app_anchors(
                &env,
                canister_id,
                BrowserKey::new(FIRST_BROWSER_SEED).successor().principal()
            )?,
            Vec::<AnchorNumber>::new()
        );
        Ok(())
    }

    #[test]
    fn should_answer_nothing_while_notifications_are_disabled() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _) = install_with_two_anchors(&env);
        let app = link_app(&env, canister_id, anchor, FIRST_BROWSER_SEED, 100);

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
            notification_app_anchors(&env, canister_id, app)?,
            Vec::<AnchorNumber>::new()
        );

        upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            arg_with_notifications_enabled(),
        )?;
        assert_eq!(
            notification_app_anchors(&env, canister_id, app)?,
            vec![anchor]
        );
        Ok(())
    }

    #[test]
    fn should_answer_nothing_once_the_last_reference_is_dropped() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _) = install_with_two_anchors(&env);
        let first = link_app(&env, canister_id, anchor, FIRST_BROWSER_SEED, 100);
        assert_eq!(
            notification_app_anchors(&env, canister_id, first)?,
            vec![anchor]
        );

        let second = link_app(&env, canister_id, anchor, FIRST_BROWSER_SEED, 101);

        assert_eq!(
            notification_app_anchors(&env, canister_id, first)?,
            Vec::<AnchorNumber>::new()
        );
        assert_eq!(
            notification_app_anchors(&env, canister_id, second)?,
            vec![anchor]
        );
        Ok(())
    }
}

/// A second identity on the device pointing at the app the first one installed.
/// One Home Screen app, several identities pointing at it. What is shared is the record,
/// its key and its one Web Push registration; what is not is whether each identity wants
/// it to deliver, which rides on that identity's own browser entry.
mod shared_app {
    use super::subscriptions::{request, ENDPOINT};
    use super::*;
    use canister_tests::api::internet_identity::api_v2::identity_info;
    use canister_tests::api::internet_identity::notifications::{
        claim_notification_app, get_notification_app_status, get_webpush_subscription_status,
        set_notification_app_enabled, set_webpush_subscription,
    };
    use internet_identity_interface::internet_identity::types::{
        BrowserInfo, ClaimNotificationAppRequest, GetNotificationAppStatusRequest,
        NotificationAppInfo, NotificationAppStatus, SetNotificationAppEnabledError,
        SetNotificationAppEnabledRequest, SetWebPushSubscriptionError,
    };
    use pretty_assertions::assert_eq;

    /// The key a browser is left holding after the sign-in `browser_seed` names, which is
    /// the one its own calls arrive signed with.
    fn browser(browser_seed: u8) -> Principal {
        BrowserKey::new(browser_seed).successor().principal()
    }

    /// Two identities on one device pointing at one Home Screen app: the first installed
    /// it, the second claimed it in a single call. Answers the app's principal.
    fn install_with_shared_app(
        env: &PocketIc,
    ) -> (CanisterId, AnchorNumber, AnchorNumber, Principal) {
        let (canister_id, first, second) = install_with_two_anchors(env);
        let app = link_app(env, canister_id, first, FIRST_BROWSER_SEED, 100);

        let expires_at_ns = env.get_time().as_nanos_since_unix_epoch() + 3 * 60 * 1_000_000_000;
        claim_notification_app(
            env,
            canister_id,
            browser(SECOND_BROWSER_SEED),
            ClaimNotificationAppRequest {
                anchor_number: second,
                offering_anchor_number: first,
                expires_at_ns,
                // Signed by the identity that has the app, which is the whole authority.
                signature: BrowserKey::new(FIRST_BROWSER_SEED)
                    .successor()
                    .sign_claim_token(first, second, expires_at_ns),
            },
        )
        .expect("claim_notification_app rejected")
        .expect("claim_notification_app returned Err");

        // Taking the app over is not asking for delivery: the identity that claimed it
        // answers the opt-in screen, which is what these tests mean by sharing an app.
        set_notification_app_enabled(
            env,
            canister_id,
            browser(SECOND_BROWSER_SEED),
            SetNotificationAppEnabledRequest {
                anchor_number: second,
                enabled: true,
            },
        )
        .expect("set_notification_app_enabled rejected")
        .expect("set_notification_app_enabled returned Err");

        (canister_id, first, second, app)
    }

    /// What the identity itself is told about its browsers, which is the second of the
    /// two reads of an installed app.
    fn browsers_of(
        env: &PocketIc,
        canister_id: CanisterId,
        sender: Principal,
        anchor: AnchorNumber,
    ) -> Result<Vec<BrowserInfo>, RejectResponse> {
        Ok(identity_info(env, canister_id, sender, anchor)?
            .expect("identity_info returned Err")
            .browsers
            .expect("a signed-in identity lists its browser"))
    }

    fn status(
        env: &PocketIc,
        canister_id: CanisterId,
        sender: Principal,
        anchor: AnchorNumber,
    ) -> Result<NotificationAppStatus, RejectResponse> {
        get_notification_app_status(
            env,
            canister_id,
            sender,
            GetNotificationAppStatusRequest {
                anchor_number: anchor,
            },
        )
    }

    /// The app this browser points at, for a read that is only meaningful once it does.
    fn linked(
        env: &PocketIc,
        canister_id: CanisterId,
        sender: Principal,
        anchor: AnchorNumber,
    ) -> Result<NotificationAppInfo, RejectResponse> {
        match status(env, canister_id, sender, anchor)? {
            NotificationAppStatus::Linked(info) => Ok(info),
            other => panic!("expected a browser with an app, got {other:?}"),
        }
    }

    /// The app has one service worker registration whatever number of identities point
    /// at it, so what it registers naming one of them is what every one of them reads.
    #[test]
    fn should_register_one_subscription_for_every_identity_pointing_at_the_app(
    ) -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, first, second, app) = install_with_shared_app(&env);

        // Installed, pointed at, and no permission given yet, which is the whole of what
        // an unsubscribed record means.
        for (sender, anchor) in [
            (browser(FIRST_BROWSER_SEED), first),
            (browser(SECOND_BROWSER_SEED), second),
        ] {
            let info = linked(&env, canister_id, sender, anchor)?;
            assert!(!info.subscribed);
            assert!(info.enabled);
        }

        set_webpush_subscription(&env, canister_id, app, request(first, ENDPOINT))?
            .expect("set_webpush_subscription returned Err");

        for anchor in [first, second] {
            let registered = get_webpush_subscription_status(&env, canister_id, app, anchor)?
                .expect("the app reads its own registration");
            assert_eq!(registered.endpoint, ENDPOINT);
        }
        for (sender, anchor) in [
            (browser(FIRST_BROWSER_SEED), first),
            (browser(SECOND_BROWSER_SEED), second),
        ] {
            let info = linked(&env, canister_id, sender, anchor)?;
            assert!(info.subscribed);
            assert!(info.enabled);
        }
        Ok(())
    }

    /// An app may act for the identities that point at it and for no others, so naming
    /// one that does not is refused rather than answered.
    #[test]
    fn should_refuse_an_app_an_identity_it_does_not_deliver_for() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, first, second) = install_with_two_anchors(&env);
        let app = link_app(&env, canister_id, first, FIRST_BROWSER_SEED, 100);

        assert_eq!(
            set_webpush_subscription(&env, canister_id, app, request(second, ENDPOINT))?,
            Err(SetWebPushSubscriptionError::InvalidBrowserKey)
        );
        assert_eq!(
            get_webpush_subscription_status(&env, canister_id, app, second)?,
            None
        );
        Ok(())
    }

    /// Turning the app off is one identity's choice about delivery and never a
    /// subscription removal: the subscription is the app's and shared, so removing it
    /// would silence everyone else too.
    #[test]
    fn should_turn_one_identity_off_and_leave_the_others_delivering() -> Result<(), RejectResponse>
    {
        let env = env();
        let (canister_id, first, second, app) = install_with_shared_app(&env);
        set_webpush_subscription(&env, canister_id, app, request(first, ENDPOINT))?
            .expect("set_webpush_subscription returned Err");

        set_notification_app_enabled(
            &env,
            canister_id,
            browser(SECOND_BROWSER_SEED),
            SetNotificationAppEnabledRequest {
                anchor_number: second,
                enabled: false,
            },
        )?
        .expect("set_notification_app_enabled returned Err");
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        let turned_off = linked(&env, canister_id, browser(SECOND_BROWSER_SEED), second)?;
        assert!(!turned_off.enabled);
        assert!(turned_off.subscribed, "the app's registration is untouched");
        let still_on = linked(&env, canister_id, browser(FIRST_BROWSER_SEED), first)?;
        assert!(still_on.enabled);
        assert!(still_on.subscribed);
        assert_eq!(
            get_webpush_subscription_status(&env, canister_id, app, second)?
                .expect("the app reads its own registration")
                .endpoint,
            ENDPOINT
        );

        // The identity's own view of its browsers says the same thing.
        assert_eq!(
            browsers_of(&env, canister_id, principal_2(), second)?[0].notification_app,
            Some(turned_off)
        );
        Ok(())
    }

    /// Both entry points are the browser's own, so an app and a stranger are told the
    /// same thing: the read answers nothing, and the write says the key it was signed
    /// with is no browser of this identity rather than claiming the browser points at no
    /// app.
    #[test]
    fn should_refuse_a_caller_that_is_no_browser_of_this_identity() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, first, _, app) = install_with_shared_app(&env);

        for sender in [app, principal_2()] {
            assert_eq!(
                status(&env, canister_id, sender, first)?,
                NotificationAppStatus::NoApp
            );
            assert_eq!(
                set_notification_app_enabled(
                    &env,
                    canister_id,
                    sender,
                    SetNotificationAppEnabledRequest {
                        anchor_number: first,
                        enabled: false,
                    },
                )?,
                Err(SetNotificationAppEnabledError::InvalidBrowserKey)
            );
        }
        Ok(())
    }

    /// A deployment that does not notify says nothing about an app and changes nothing
    /// about one, although the entries are there.
    #[test]
    fn should_answer_nothing_about_an_app_while_notifications_are_disabled(
    ) -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, first, _, _) = install_with_shared_app(&env);

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
            status(&env, canister_id, browser(FIRST_BROWSER_SEED), first)?,
            NotificationAppStatus::NoApp
        );
        assert_eq!(
            browsers_of(&env, canister_id, principal_1(), first)?[0].notification_app,
            None,
            "the identity's own view of its browsers is gated the same way"
        );
        assert!(matches!(
            set_notification_app_enabled(
                &env,
                canister_id,
                browser(FIRST_BROWSER_SEED),
                SetNotificationAppEnabledRequest {
                    anchor_number: first,
                    enabled: false,
                },
            )?,
            Err(SetNotificationAppEnabledError::InternalCanisterError(_))
        ));

        upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            arg_with_notifications_enabled(),
        )?;
        assert!(matches!(
            status(&env, canister_id, browser(FIRST_BROWSER_SEED), first)?,
            NotificationAppStatus::Linked(_)
        ));
        assert!(browsers_of(&env, canister_id, principal_1(), first)?[0]
            .notification_app
            .is_some());
        Ok(())
    }

    #[test]
    fn should_leave_a_claimed_app_switched_off_until_the_claimer_asks() -> Result<(), RejectResponse>
    {
        let env = env();
        let (canister_id, first, second) = install_with_two_anchors(&env);
        link_app(&env, canister_id, first, FIRST_BROWSER_SEED, 100);
        let expires_at_ns = env.get_time().as_nanos_since_unix_epoch() + 3 * 60 * 1_000_000_000;

        claim_notification_app(
            &env,
            canister_id,
            browser(SECOND_BROWSER_SEED),
            ClaimNotificationAppRequest {
                anchor_number: second,
                offering_anchor_number: first,
                expires_at_ns,
                signature: BrowserKey::new(FIRST_BROWSER_SEED)
                    .successor()
                    .sign_claim_token(first, second, expires_at_ns),
            },
        )?
        .expect("claim_notification_app returned Err");

        assert!(
            !linked(&env, canister_id, browser(SECOND_BROWSER_SEED), second)?.enabled,
            "the identity that took the app over was switched on without being asked"
        );
        // The identity that installed it is untouched: it asked, and its answer stands.
        assert!(
            linked(&env, canister_id, browser(FIRST_BROWSER_SEED), first)?.enabled,
            "the claim switched the installing identity off"
        );

        set_notification_app_enabled(
            &env,
            canister_id,
            browser(SECOND_BROWSER_SEED),
            SetNotificationAppEnabledRequest {
                anchor_number: second,
                enabled: true,
            },
        )?
        .expect("set_notification_app_enabled returned Err");

        assert!(
            linked(&env, canister_id, browser(SECOND_BROWSER_SEED), second)?.enabled,
            "answering the opt-in screen did not switch delivery on"
        );
        Ok(())
    }
}

mod claims {
    use super::*;
    use canister_tests::api::internet_identity::notifications::{
        claim_notification_app, notification_app_anchors,
    };
    use internet_identity_interface::internet_identity::types::{
        ClaimNotificationAppError, ClaimNotificationAppRequest,
    };
    use pretty_assertions::assert_eq;

    /// Well inside the half hour a token may claim, so the window holds for a test's rounds.
    const TOKEN_VALID_FOR_NS: u64 = 3 * 60 * 1_000_000_000;

    /// An expiry a token minted now may claim.
    fn expires_at(env: &PocketIc) -> u64 {
        env.get_time().as_nanos_since_unix_epoch() + TOKEN_VALID_FOR_NS
    }

    /// Points the browser `claiming_browser_seed` names at the app the offering one has.
    fn claim(
        env: &PocketIc,
        canister_id: CanisterId,
        claiming: (AnchorNumber, u8),
        offering: (AnchorNumber, u8),
        expires_at_ns: u64,
    ) -> Result<Result<(), ClaimNotificationAppError>, RejectResponse> {
        let (claiming_anchor, claiming_browser_seed) = claiming;
        let (offering_anchor, offering_browser_seed) = offering;
        claim_notification_app(
            env,
            canister_id,
            BrowserKey::new(claiming_browser_seed)
                .successor()
                .principal(),
            ClaimNotificationAppRequest {
                anchor_number: claiming_anchor,
                offering_anchor_number: offering_anchor,
                expires_at_ns,
                // The successor the sign-in announced, the only slot a claim is honoured from.
                signature: BrowserKey::new(offering_browser_seed)
                    .successor()
                    .sign_claim_token(offering_anchor, claiming_anchor, expires_at_ns),
            },
        )
    }

    #[test]
    fn should_deliver_for_a_second_identity_after_one_call() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, first, second) = install_with_two_anchors(&env);
        let app = link_app(&env, canister_id, first, FIRST_BROWSER_SEED, 100);

        claim(&env, canister_id, (second, 2), (first, 1), expires_at(&env))?
            .expect("claim_notification_app returned Err");

        assert_eq!(
            notification_app_anchors(&env, canister_id, app)?,
            vec![first, second]
        );
        Ok(())
    }

    /// The claiming identity's own consent is what the caller check is: a browser of
    /// another identity cannot route this one's notifications through an app.
    #[test]
    fn should_refuse_a_claim_for_an_anchor_the_caller_is_no_browser_of(
    ) -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, first, second) = install_with_two_anchors(&env);
        let app = link_app(&env, canister_id, first, 1, 100);

        // Everything the offering identity signed, presented by its own browser.
        assert_eq!(
            claim(&env, canister_id, (second, 1), (first, 1), expires_at(&env))?,
            Err(ClaimNotificationAppError::InvalidBrowserKey)
        );
        assert_eq!(
            notification_app_anchors(&env, canister_id, app)?,
            vec![first]
        );
        Ok(())
    }

    /// A signature from a browser with no app hands over nothing, and says so rather
    /// than writing a reference to a record that does not exist.
    #[test]
    fn should_refuse_a_claim_whose_offering_browser_points_at_nothing() -> Result<(), RejectResponse>
    {
        let env = env();
        let (canister_id, first, second) = install_with_two_anchors(&env);

        assert_eq!(
            claim(&env, canister_id, (second, 2), (first, 1), expires_at(&env))?,
            Err(ClaimNotificationAppError::NoNotificationApp)
        );
        Ok(())
    }

    #[test]
    fn should_refuse_an_expired_claim_token() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, first, second) = install_with_two_anchors(&env);
        let app = link_app(&env, canister_id, first, 1, 100);

        let now = env.get_time().as_nanos_since_unix_epoch();
        assert_eq!(
            claim(&env, canister_id, (second, 2), (first, 1), now)?,
            Err(ClaimNotificationAppError::ClaimTokenExpired)
        );
        assert_eq!(
            notification_app_anchors(&env, canister_id, app)?,
            vec![first]
        );
        Ok(())
    }

    #[test]
    fn should_accept_the_same_claim_token_twice() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, first, second) = install_with_two_anchors(&env);
        let app = link_app(&env, canister_id, first, 1, 100);
        let expires_at_ns = expires_at(&env);

        for _ in 0..2 {
            claim(&env, canister_id, (second, 2), (first, 1), expires_at_ns)?
                .expect("claim_notification_app returned Err");
            assert_eq!(
                notification_app_anchors(&env, canister_id, app)?,
                vec![first, second]
            );
        }
        Ok(())
    }
}

fn next_request(anchor_number: AnchorNumber) -> GetNextNotificationRequest {
    GetNextNotificationRequest { anchor_number }
}

fn remove_request(anchor_number: AnchorNumber) -> RemoveNotificationRequest {
    RemoveNotificationRequest {
        anchor_number,
        notification: NotificationToShow {
            origin: ORIGIN.into(),
            account_number: None,
            canister_id: principal_2(),
            id: 1,
        },
    }
}

/// The two halves of an install: the browser registers the one-shot key it is about to
/// hand out, and the app spends it. Nothing is stored between them but the principal of
/// that key and when it stops being spendable.
mod app_link {
    use super::*;
    use candid::Principal;
    use canister_tests::api::internet_identity::notifications::{
        link_notification_app, prepare_notification_app_link,
    };
    use internet_identity_interface::internet_identity::types::PublicKey;
    use internet_identity_interface::internet_identity::types::{
        LinkNotificationAppError, LinkNotificationAppRequest, PrepareNotificationAppLinkError,
        PrepareNotificationAppLinkRequest,
    };
    use pretty_assertions::assert_eq;

    const BROWSER_SEED: u8 = 1;

    /// An identity signed in once, so its browser has a key the canister recognises.
    fn install_with_browser(env: &PocketIc) -> (CanisterId, AnchorNumber) {
        let canister_id =
            install_ii_canister_with_arg(env, II_WASM.clone(), arg_with_notifications_enabled());
        let anchor = flows::register_anchor(env, canister_id);
        sign_in_at(env, canister_id, anchor, ORIGIN, BROWSER_SEED);
        (canister_id, anchor)
    }

    /// The successor the sign-in announced, which is the only slot a browser is recognised by.
    fn browser(browser_seed: u8) -> Principal {
        BrowserKey::new(browser_seed).successor().principal()
    }

    fn prepare(
        env: &PocketIc,
        canister_id: CanisterId,
        sender: Principal,
        anchor: AnchorNumber,
        link_principal: Principal,
    ) -> Result<Result<(), PrepareNotificationAppLinkError>, RejectResponse> {
        prepare_notification_app_link(
            env,
            canister_id,
            sender,
            PrepareNotificationAppLinkRequest {
                anchor_number: anchor,
                link_principal,
                replacing_existing: false,
            },
        )
    }

    fn link(
        env: &PocketIc,
        canister_id: CanisterId,
        sender: Principal,
        anchor: AnchorNumber,
        app_key: PublicKey,
    ) -> Result<Result<(), LinkNotificationAppError>, RejectResponse> {
        link_notification_app(
            env,
            canister_id,
            sender,
            LinkNotificationAppRequest {
                anchor_number: anchor,
                app_key,
            },
        )
    }

    #[test]
    fn should_link_an_app_the_browser_handed_a_key_to() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_browser(&env);
        let key = BrowserKey::new(100);
        let app = BrowserKey::new(101);

        assert_eq!(
            prepare(
                &env,
                canister_id,
                browser(BROWSER_SEED),
                anchor,
                key.principal()
            )?,
            Ok(())
        );
        assert_eq!(
            link(&env, canister_id, key.principal(), anchor, app.public_key())?,
            Ok(())
        );
        Ok(())
    }

    /// The key is the whole of the authority, so a caller that is not the one the browser
    /// registered is refused however well formed its app key is.
    #[test]
    fn should_refuse_a_caller_the_browser_did_not_hand_the_key_to() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_browser(&env);
        let key = BrowserKey::new(100);
        let app = BrowserKey::new(101);

        prepare(
            &env,
            canister_id,
            browser(BROWSER_SEED),
            anchor,
            key.principal(),
        )?
        .expect("prepare_notification_app_link returned Err");

        assert_eq!(
            link(
                &env,
                canister_id,
                BrowserKey::new(102).principal(),
                anchor,
                app.public_key()
            )?,
            Err(LinkNotificationAppError::NoPendingLink)
        );
        Ok(())
    }

    /// One shot: the slot is cleared by the app that spends it, so the same URL opened
    /// again buys nothing. This is what a boundary node replaying the call would get.
    #[test]
    fn should_refuse_a_key_that_has_already_been_spent() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_browser(&env);
        let key = BrowserKey::new(100);

        prepare(
            &env,
            canister_id,
            browser(BROWSER_SEED),
            anchor,
            key.principal(),
        )?
        .expect("prepare_notification_app_link returned Err");
        link(
            &env,
            canister_id,
            key.principal(),
            anchor,
            BrowserKey::new(101).public_key(),
        )?
        .expect("link_notification_app returned Err");

        assert_eq!(
            link(
                &env,
                canister_id,
                key.principal(),
                anchor,
                BrowserKey::new(103).public_key()
            )?,
            Err(LinkNotificationAppError::NoPendingLink)
        );
        Ok(())
    }

    /// A principal no key can sign as is an install nothing could complete, and the
    /// anonymous one is an install anybody could: neither may be written to the slot.
    #[test]
    fn should_refuse_a_link_principal_no_key_can_sign_as() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_browser(&env);

        for named in [Principal::anonymous(), canister_id] {
            assert_eq!(
                prepare(&env, canister_id, browser(BROWSER_SEED), anchor, named)?,
                Err(PrepareNotificationAppLinkError::LinkPrincipalIsNotAKey)
            );
        }
        // And nothing was written: the app the anonymous caller would have been is refused.
        assert_eq!(
            link(
                &env,
                canister_id,
                Principal::anonymous(),
                anchor,
                BrowserKey::new(101).public_key()
            )?,
            Err(LinkNotificationAppError::NoPendingLink)
        );
        Ok(())
    }

    /// The record is keyed by these bytes, so anything that is not a key leaves this
    /// browser pointing at an app nobody can sign as, with its one-shot key spent.
    #[test]
    fn should_refuse_an_app_key_that_is_not_a_key() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_browser(&env);
        let key = BrowserKey::new(100);

        for offered in [
            PublicKey::from(vec![]),
            PublicKey::from(vec![4; 65]),
            PublicKey::from(vec![0; 2_000]),
        ] {
            prepare(
                &env,
                canister_id,
                browser(BROWSER_SEED),
                anchor,
                key.principal(),
            )?
            .expect("prepare_notification_app_link returned Err");
            assert_eq!(
                link(&env, canister_id, key.principal(), anchor, offered)?,
                Err(LinkNotificationAppError::InvalidAppKey)
            );
        }

        // Refused before the slot is read, so the install the browser started is intact.
        assert_eq!(
            link(
                &env,
                canister_id,
                key.principal(),
                anchor,
                BrowserKey::new(101).public_key()
            )?,
            Ok(())
        );
        Ok(())
    }

    /// An app key is minted in a partition no browser can reach, so a key one of this
    /// identity's browsers already signs with is not an app: it is refused, and the
    /// install stays spendable.
    #[test]
    fn should_refuse_an_app_key_a_browser_already_signs_with() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_browser(&env);
        let key = BrowserKey::new(100);

        for held in [
            BrowserKey::new(BROWSER_SEED).public_key(),
            BrowserKey::new(BROWSER_SEED).successor().public_key(),
        ] {
            prepare(
                &env,
                canister_id,
                browser(BROWSER_SEED),
                anchor,
                key.principal(),
            )?
            .expect("prepare_notification_app_link returned Err");
            assert_eq!(
                link(&env, canister_id, key.principal(), anchor, held)?,
                Err(LinkNotificationAppError::KeyAlreadyInUse)
            );
        }

        // Refused before anything is written, so the install the browser started is intact.
        assert_eq!(
            link(
                &env,
                canister_id,
                key.principal(),
                anchor,
                BrowserKey::new(101).public_key()
            )?,
            Ok(())
        );
        Ok(())
    }

    #[test]
    fn should_refuse_a_caller_that_is_no_browser_of_the_identity() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_browser(&env);

        assert_eq!(
            prepare(
                &env,
                canister_id,
                principal_2(),
                anchor,
                BrowserKey::new(100).principal()
            )?,
            Err(PrepareNotificationAppLinkError::InvalidBrowserKey)
        );
        Ok(())
    }

    #[test]
    fn should_refuse_once_notifications_are_disabled() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor) = install_with_browser(&env);
        upgrade_ii_canister_with_arg(
            &env,
            canister_id,
            II_WASM.clone(),
            Some(InternetIdentityInit {
                notifications_enabled: Some(false),
                ..Default::default()
            }),
        )?;

        assert!(matches!(
            prepare(
                &env,
                canister_id,
                browser(BROWSER_SEED),
                anchor,
                BrowserKey::new(100).principal()
            )?,
            Err(PrepareNotificationAppLinkError::InternalCanisterError(_))
        ));
        assert!(matches!(
            link(
                &env,
                canister_id,
                BrowserKey::new(100).principal(),
                anchor,
                BrowserKey::new(101).public_key()
            )?,
            Err(LinkNotificationAppError::InternalCanisterError(_))
        ));
        Ok(())
    }
}
