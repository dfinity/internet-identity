use crate::openid;
use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, create_identity_with_openid_credential, test_authn_method,
};
use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    assert_labelled_metric, assert_metric, env, get_metrics, install_ii_canister, time,
    upgrade_ii_canister, II_WASM,
};
use internet_identity_interface::internet_identity::types::{
    AuthnMethod, AuthnMethodData, AuthnMethodProtection, AuthnMethodPurpose,
    AuthnMethodSecuritySettings, MetadataEntryV2, PublicKeyAuthn, WebAuthn,
};
use pocket_ic::RejectResponse;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::time::Duration;

const DAY_SECONDS: u64 = 24 * 60 * 60;
const MONTH_SECONDS: u64 = 30 * DAY_SECONDS;

const AUTHN_METHOD_TYPES: [&str; 2] = ["webauthn_auth", "recovery_phrase"];

/// Tests that daily active authn_methods are counted correctly.
#[test]
fn should_report_daily_active_authn_methods() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // ensure stats are initially absent
    assert!(
        !get_metrics(&env, canister_id).contains("internet_identity_daily_active_authn_methods")
    );

    for (authn_type, authn_method) in authn_methods_all_types() {
        // advance time to ensure a new collection period
        env.advance_time(Duration::from_secs(DAY_SECONDS));

        // Create two identities with the same authn_method to bump the counter to a value of 2
        // so that it's more than an off-by-one error away from 0.
        create_identity_with_authn_method(&env, canister_id, &authn_method);
        let identity_nr = create_identity_with_authn_method(&env, canister_id, &authn_method);

        // repeated activity with the same authn_method on the same identity within the 24h
        // collection period should not increase the counter
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?
            .expect("identity info failed");

        env.advance_time(Duration::from_secs(DAY_SECONDS));

        // some activity is required to update the stats
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?
            .expect("identity info failed");

        let metrics = get_metrics(&env, canister_id);
        assert_metric(
            &metrics,
            &format!("internet_identity_daily_active_authn_methods{{type=\"{authn_type}\"}}"),
            2f64,
        );

        // assert all other counters are 0
        assert_labelled_metric(
            &metrics,
            "internet_identity_daily_active_authn_methods",
            0f64,
            "type",
            &AUTHN_METHOD_TYPES
                .iter()
                .filter(|t| **t != authn_type)
                .copied()
                .collect::<Vec<&str>>(),
        );
    }

    Ok(())
}

/// Tests that monthly active authn_methods are counted correctly.
#[test]
fn should_report_monthly_active_authn_methods() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    // ensure stats are initially absent
    assert!(
        !get_metrics(&env, canister_id).contains("internet_identity_daily_active_authn_methods")
    );

    for (authn_type, authn_method) in authn_methods_all_types() {
        // advance time to ensure a new collection period
        env.advance_time(Duration::from_secs(MONTH_SECONDS));

        // Create two identities with the same authn_method to bump the counter to a value of 2
        // so that it's more than an off-by-one error away from 0.
        create_identity_with_authn_method(&env, canister_id, &authn_method);
        let identity_nr = create_identity_with_authn_method(&env, canister_id, &authn_method);

        // repeated activity with the same authn_method on the same identity within the 30-day
        // collection period should not increase the counter
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?
            .expect("identity info failed");

        env.advance_time(Duration::from_secs(MONTH_SECONDS));

        // some activity is required to update the stats
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?
            .expect("identity info failed");

        let metrics = get_metrics(&env, canister_id);
        assert_metric(
            &metrics,
            &format!("internet_identity_monthly_active_authn_methods{{type=\"{authn_type}\"}}"),
            2f64,
        );

        // assert all other counters are 0
        assert_labelled_metric(
            &metrics,
            "internet_identity_monthly_active_authn_methods",
            0f64,
            "type",
            &AUTHN_METHOD_TYPES
                .iter()
                .filter(|t| **t != authn_type)
                .copied()
                .collect::<Vec<&str>>(),
        );
    }

    Ok(())
}

/// Tests that active authn method stats are kept across upgrades from the same release.
#[test]
fn should_keep_stats_across_upgrades() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = AuthnMethodData {
        authn_method: AuthnMethod::WebAuthn(WebAuthn {
            pubkey: ByteBuf::from("example pubkey"),
            credential_id: ByteBuf::from("example credential id"),
            aaguid: None,
        }),
        metadata: HashMap::from([(
            "origin".to_string(),
            MetadataEntryV2::String("https://identity.ic0.app".to_string()),
        )]),
        security_settings: AuthnMethodSecuritySettings {
            protection: AuthnMethodProtection::Unprotected,
            purpose: AuthnMethodPurpose::Authentication,
        },
        last_authentication: None,
    };

    // ensure stats are initially absent
    assert!(
        !get_metrics(&env, canister_id).contains("internet_identity_daily_active_authn_methods")
    );

    let identity_nr = create_identity_with_authn_method(&env, canister_id, &authn_method);

    // upgrade current -> current
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    env.advance_time(Duration::from_secs(DAY_SECONDS));

    // some activity is required to update the stats
    api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?
        .expect("identity info failed");

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_daily_active_authn_methods{type=\"webauthn_auth\"}",
        1f64,
    );

    Ok(())
}

/// Tests that active OpenID authn_methods are counted correctly.
/// TODO: ID-155 Create OpenID mock data on-demand and use this instead in unit/integration tests
#[test]
fn should_report_active_openid_authn_methods() {
    // Create II instance that mocks Google certs
    let env = env();
    let canister_id = openid::setup_canister(&env);
    api::init_salt(&env, canister_id).unwrap();

    // Using Google as OpenID provider, but we could have used any other configured OpenID provider.
    let (jwt, salt, _claims, test_time, test_principal, _test_authn_method) =
        openid::openid_google_test_data();
    env.advance_time(Duration::from_millis(test_time) - Duration::from_nanos(time(&env)));

    // Ensure stats are initially absent
    assert!(
        !get_metrics(&env, canister_id).contains("internet_identity_daily_active_authn_methods")
    );

    // Create account with Google OpenID
    let identity_number =
        create_identity_with_openid_credential(&env, canister_id, &jwt, &salt, test_principal);

    // Get OpenID delegation principal
    let delegation_principal = Principal::self_authenticating(
        api::openid_prepare_delegation(
            &env,
            canister_id,
            test_principal,
            &jwt,
            &salt,
            &ByteBuf::from([0u8; 32]),
        )
        .unwrap()
        .unwrap()
        .user_key,
    );

    // Repeated activity within the same period should not increase the counter
    api_v2::identity_info(&env, canister_id, delegation_principal, identity_number)
        .unwrap()
        .unwrap();

    // Check daily stats, some activity is required first to update the stats
    env.advance_time(Duration::from_secs(DAY_SECONDS));
    create_identity_with_authn_method(&env, canister_id, &test_authn_method());
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_daily_active_authn_methods{type=\"openid\",issuer=\"https://accounts.google.com\"}",
        1f64,
    );

    // Check monthly stats, some activity is required first to update the stats
    env.advance_time(Duration::from_secs(MONTH_SECONDS - DAY_SECONDS));
    create_identity_with_authn_method(&env, canister_id, &test_authn_method());
    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_monthly_active_authn_methods{type=\"openid\",issuer=\"https://accounts.google.com\"}",
        1f64,
    );
}

fn authn_methods_all_types() -> Vec<(String, AuthnMethodData)> {
    vec![
        (
            "webauthn_auth".to_string(),
            AuthnMethodData {
                authn_method: AuthnMethod::WebAuthn(WebAuthn {
                    pubkey: ByteBuf::from("example pubkey"),
                    credential_id: ByteBuf::from("example credential id"),
                    aaguid: None,
                }),
                metadata: HashMap::from([(
                    "origin".to_string(),
                    MetadataEntryV2::String("https://identity.ic0.app".to_string()),
                )]),
                security_settings: AuthnMethodSecuritySettings {
                    protection: AuthnMethodProtection::Unprotected,
                    purpose: AuthnMethodPurpose::Authentication,
                },
                last_authentication: None,
            },
        ),
        (
            "recovery_phrase".to_string(),
            AuthnMethodData {
                authn_method: AuthnMethod::PubKey(PublicKeyAuthn {
                    pubkey: ByteBuf::from(vec![0; 32]),
                }),
                metadata: HashMap::from([(
                    "usage".to_string(),
                    MetadataEntryV2::String("recovery_phrase".to_string()),
                )]),
                security_settings: AuthnMethodSecuritySettings {
                    protection: AuthnMethodProtection::Unprotected,
                    purpose: AuthnMethodPurpose::Recovery,
                },
                last_authentication: None,
            },
        ),
    ]
}
