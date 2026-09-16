//! Tests for the notification API, exercised through Candid against a canister.

use canister_tests::api::internet_identity::api_v2::prepare_account_session;
use canister_tests::api::internet_identity::notifications::{
    consent_granted, grant_consent, revoke_consent,
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
    NotificationRevokeConsentError, OperatingSystem, PrepareAccountSessionRequest,
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
        subscribe_device, unsubscribe_device,
    };
    use canister_tests::framework::BrowserKey;
    use internet_identity_interface::internet_identity::types::{
        BrowserBrand, BrowserDescription, BrowserId, FormFactor, OperatingSystem,
        PrepareAccountSessionRequest, SubscribeDeviceError, SubscribeDeviceRequest,
        UnsubscribeDeviceError,
    };
    use pretty_assertions::assert_eq;
    use serde_bytes::ByteBuf;

    const ENDPOINT: &str = "https://push.example.com/aBcDeF";
    const ISSUED_AT_NS: u64 = 1_700_000_000_000_000_000;

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
    fn sign_browser_in(
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

    fn install_with_browser(env: &PocketIc) -> (CanisterId, AnchorNumber, BrowserKey, BrowserId) {
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

    /// What a browser uploads. The successor key is the one it keeps between sign-ins.
    fn request_from(
        anchor: AnchorNumber,
        browser: &BrowserKey,
        endpoint: &str,
    ) -> SubscribeDeviceRequest {
        let key_holder = browser.successor();
        SubscribeDeviceRequest {
            anchor_number: anchor,
            endpoint: endpoint.to_string(),
            vapid_public_key: vapid_public_key(),
            jwt_signatures: jwt_pool(),
            jwt_issued_at_ns: ISSUED_AT_NS,
            browser_key: key_holder.public_key(),
            browser_key_signature: key_holder.sign_webpush_subscription(endpoint, ISSUED_AT_NS),
        }
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
            subscribe_device(
                &env,
                canister_id,
                principal_1(),
                request_from(anchor, &browser, ENDPOINT)
            )?,
            Err(SubscribeDeviceError::InternalCanisterError(_))
        ));
        Ok(())
    }

    #[test]
    fn should_subscribe_and_unsubscribe_a_browser() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, browser_id) = install_with_browser(&env);

        subscribe_device(
            &env,
            canister_id,
            principal_1(),
            request_from(anchor, &browser, ENDPOINT),
        )?
        .expect("subscribe rejected");

        unsubscribe_device(&env, canister_id, principal_1(), anchor, browser_id)?
            .expect("unsubscribe rejected");
        // Idempotent on purpose, since silencing a browser is done from another one.
        unsubscribe_device(&env, canister_id, principal_1(), anchor, browser_id)?
            .expect("a second unsubscribe should be a no-op, not an error");
        Ok(())
    }

    #[test]
    fn should_refuse_a_browser_this_identity_is_not_signed_in_from() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, _browser, _) = install_with_browser(&env);

        // A key the registry has never seen, held by whoever is calling.
        let stranger = BrowserKey::new(9);
        let refused = subscribe_device(
            &env,
            canister_id,
            principal_1(),
            request_from(anchor, &stranger, ENDPOINT),
        )?;
        assert!(
            matches!(refused, Err(SubscribeDeviceError::InvalidBrowserKey)),
            "{refused:?}"
        );
        Ok(())
    }

    /// Every device of an identity passes the same authorization, so naming a browser
    /// is not on its own evidence of being it.
    #[test]
    fn should_refuse_one_browser_registering_against_anothers_key() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, victim, _) = install_with_browser(&env);
        let attacker = BrowserKey::new(2);
        sign_browser_in(&env, canister_id, anchor, &attacker);

        let attacker_endpoint = "https://push.attacker.example/inbox";
        let forged = SubscribeDeviceRequest {
            anchor_number: anchor,
            endpoint: attacker_endpoint.to_string(),
            vapid_public_key: vapid_public_key(),
            jwt_signatures: jwt_pool(),
            jwt_issued_at_ns: ISSUED_AT_NS,
            // The victim's browser, and the best signature the attacker can make.
            browser_key: victim.successor().public_key(),
            browser_key_signature: attacker
                .successor()
                .sign_webpush_subscription(attacker_endpoint, ISSUED_AT_NS),
        };

        let refused = subscribe_device(&env, canister_id, principal_1(), forged)?;
        assert!(
            matches!(refused, Err(SubscribeDeviceError::InvalidBrowserKey)),
            "one browser registered against another's key: {refused:?}"
        );
        Ok(())
    }

    #[test]
    fn should_refuse_a_signature_lifted_onto_another_endpoint() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_with_browser(&env);

        let mut request = request_from(anchor, &browser, ENDPOINT);
        request.endpoint = "https://push.example.com/elsewhere".to_string();

        let refused = subscribe_device(&env, canister_id, principal_1(), request)?;
        assert!(
            matches!(refused, Err(SubscribeDeviceError::InvalidBrowserKey)),
            "{refused:?}"
        );
        Ok(())
    }

    #[test]
    fn should_report_every_invalid_field_at_once() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, _) = install_with_browser(&env);

        let endpoint = "";
        let key_holder = browser.successor();
        let request = SubscribeDeviceRequest {
            anchor_number: anchor,
            endpoint: endpoint.to_string(),
            vapid_public_key: ByteBuf::from(vec![4u8; 65]),
            jwt_signatures: vec![],
            jwt_issued_at_ns: ISSUED_AT_NS,
            browser_key: key_holder.public_key(),
            browser_key_signature: key_holder.sign_webpush_subscription(endpoint, ISSUED_AT_NS),
        };

        match subscribe_device(&env, canister_id, principal_1(), request)? {
            Err(SubscribeDeviceError::InternalCanisterError(problems)) => {
                // Failing on the first would send a browser round three times to
                // learn what a single answer can say.
                assert_eq!(problems.split("; ").count(), 3, "{problems:?}");
            }
            other => panic!("expected every field reported at once, got {other:?}"),
        }
        Ok(())
    }

    #[test]
    fn should_refuse_a_caller_that_does_not_own_the_anchor() -> Result<(), RejectResponse> {
        let env = env();
        let (canister_id, anchor, browser, browser_id) = install_with_browser(&env);

        let refused = subscribe_device(
            &env,
            canister_id,
            principal_2(),
            request_from(anchor, &browser, ENDPOINT),
        )?;
        assert!(
            matches!(refused, Err(SubscribeDeviceError::Unauthorized(_))),
            "{refused:?}"
        );
        let refused = unsubscribe_device(&env, canister_id, principal_2(), anchor, browser_id)?;
        assert!(
            matches!(refused, Err(UnsubscribeDeviceError::Unauthorized(_))),
            "{refused:?}"
        );
        Ok(())
    }
}
