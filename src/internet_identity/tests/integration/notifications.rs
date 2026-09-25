//! Tests for the notification API, exercised through Candid against a canister.

use canister_tests::api::internet_identity::api_v2::prepare_account_session;
use canister_tests::api::internet_identity::notifications::{
    consent_granted, get_queued_notifications, grant_consent, remove_queued_notification,
    revoke_consent,
};
use canister_tests::flows;
use canister_tests::framework::{
    arg_with_captcha_disabled, arg_with_notifications_enabled_for, env,
    install_ii_canister_with_arg, principal_1, principal_2, upgrade_ii_canister, BrowserKey,
    II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, BrowserBrand, BrowserDescription, FormFactor, NotificationGrantConsentError,
    NotificationRevokeConsentError, NotificationToShow, OperatingSystem,
    PrepareAccountSessionRequest, QueuedNotificationError, RemoveQueuedNotificationRequest,
};
use pocket_ic::{PocketIc, RejectResponse};
use serde_bytes::ByteBuf;

const ORIGIN: &str = "https://some-dapp.com";
const GATEWAY: &str = "https://abcde-aaaaa-aaaaa-aaaaa-cai.ic0.app";
const UNREACHED: &str = "https://never-visited.example";
/// Every origin the tests below expect to be notifiable. One that is not enabled is
/// refused before anything else, which `should_refuse_an_origin_that_is_not_enabled`
/// covers on its own.
const ENABLED: &[&str] = &[ORIGIN, GATEWAY, UNREACHED];

/// A canister notifying for `ENABLED`, and one anchor registered to `principal_1`
/// which has signed in at `ORIGIN`.
fn install_with_anchor(env: &PocketIc) -> (CanisterId, AnchorNumber) {
    let canister_id = install_ii_canister_with_arg(
        env,
        II_WASM.clone(),
        arg_with_notifications_enabled_for(ENABLED),
    );
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
    let browser = BrowserKey::new(browser_seed);
    let session_key = ByteBuf::from(vec![1; 32]);
    let next_browser_key = browser.successor().public_key();
    prepare_account_session(
        env,
        canister_id,
        principal_1(),
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
fn should_refuse_every_entry_point_while_no_origin_is_enabled() -> Result<(), RejectResponse> {
    let env = env();
    // The default arg, which enables no origin.
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
        get_queued_notifications(&env, canister_id, principal_1(), anchor)?,
        Err(QueuedNotificationError::InternalCanisterError(_))
    ));
    assert!(matches!(
        remove_queued_notification(
            &env,
            canister_id,
            principal_1(),
            RemoveQueuedNotificationRequest {
                anchor_number: anchor,
                notification: shown_elsewhere(),
            }
        )?,
        Err(QueuedNotificationError::InternalCanisterError(_))
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
fn should_reject_an_origin_that_is_not_a_bare_https_authority() -> Result<(), RejectResponse> {
    let env = env();
    let (canister_id, anchor) = install_with_anchor(&env);

    for origin in [
        "http://some-dapp.com",
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

/// The allowlist is what rolls the feature out app by app, so an origin left off it is
/// refused even for an identity that has signed in there.
#[test]
fn should_refuse_an_origin_that_is_not_enabled() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_notifications_enabled_for(&[ORIGIN]),
    );
    let anchor = flows::register_anchor(&env, canister_id);
    sign_in_at(&env, canister_id, anchor, ORIGIN, 1);
    sign_in_at(&env, canister_id, anchor, "https://other-dapp.com", 2);

    assert!(matches!(
        grant_consent(
            &env,
            canister_id,
            principal_1(),
            anchor,
            "https://other-dapp.com".into()
        )?,
        Err(NotificationGrantConsentError::InternalCanisterError(_))
    ));
    assert!(!consent_granted(
        &env,
        canister_id,
        principal_1(),
        anchor,
        "https://other-dapp.com".into()
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
        let canister_id = install_ii_canister_with_arg(
            env,
            II_WASM.clone(),
            arg_with_notifications_enabled_for(ENABLED),
        );
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
    fn should_refuse_to_subscribe_while_no_origin_is_enabled() -> Result<(), RejectResponse> {
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
    use candid::Principal;
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

    /// Notifications roll out app by app, and these two entry points are no
    /// exception: an origin the operator left off the list mints nothing.
    #[test]
    fn should_refuse_an_origin_that_is_not_enabled() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_notifiable(&env);
        let caller = key_holder(&browser).principal();
        let disabled = "https://not-enabled.example";

        assert!(matches!(
            prepare_notification_delegation(
                &env,
                canister_id,
                caller,
                PrepareNotificationDelegationRequest {
                    origin: disabled.into(),
                    ..prepare_request(anchor)
                },
            )?,
            Err(NotificationDelegationError::InternalCanisterError(_))
        ));
        assert!(matches!(
            get_notification_delegation(
                &env,
                canister_id,
                caller,
                GetNotificationDelegationRequest {
                    origin: disabled.into(),
                    ..get_request(anchor, 0)
                },
            )?,
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
    fn should_have_nothing_to_show_for_a_browser_nothing_was_sent_to() -> Result<(), RejectResponse>
    {
        let env = env();
        let (canister_id, anchor, browser, _) = install_with_browser(&env);

        assert_eq!(
            get_queued_notifications(&env, canister_id, key_holder(&browser).principal(), anchor)?,
            Ok(vec![])
        );
        Ok(())
    }

    #[test]
    fn should_remove_nothing_for_a_notification_that_is_not_queued() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_with_browser(&env);

        assert_eq!(
            remove_queued_notification(
                &env,
                canister_id,
                key_holder(&browser).principal(),
                RemoveQueuedNotificationRequest {
                    anchor_number: anchor,
                    notification: shown_elsewhere(),
                }
            )?,
            Ok(())
        );
        Ok(())
    }

    #[test]
    fn should_refuse_a_caller_that_is_no_browser_of_the_identity() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _, _) = install_with_browser(&env);

        assert_eq!(
            get_queued_notifications(&env, canister_id, principal_2(), anchor)?,
            Err(QueuedNotificationError::InvalidBrowserKey)
        );
        assert_eq!(
            remove_queued_notification(
                &env,
                canister_id,
                principal_2(),
                RemoveQueuedNotificationRequest {
                    anchor_number: anchor,
                    notification: shown_elsewhere(),
                }
            )?,
            Err(QueuedNotificationError::InvalidBrowserKey)
        );
        Ok(())
    }
}

fn shown_elsewhere() -> NotificationToShow {
    NotificationToShow {
        origin: ORIGIN.into(),
        account_number: None,
        canister_id: principal_2(),
        id: 1,
    }
}
