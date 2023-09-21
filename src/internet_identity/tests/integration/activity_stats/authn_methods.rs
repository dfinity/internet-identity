use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    assert_labelled_metric, assert_metric, env, get_metrics, install_ii_canister,
    upgrade_ii_canister, II_WASM,
};
use internet_identity_interface::internet_identity::types::{
    AuthnMethod, AuthnMethodData, MetadataEntry, Purpose, WebAuthn,
};
use pocket_ic::CallError;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::time::Duration;

const DAY_SECONDS: u64 = 24 * 60 * 60;
const MONTH_SECONDS: u64 = 30 * DAY_SECONDS;

const AUTHN_METHOD_TYPES: [&str; 5] = [
    "other",
    "webauthn_auth",
    "webauthn_recovery",
    "recovery_phrase",
    "browser_storage_key",
];

/// Tests that daily active authn_methods are counted correctly.
#[test]
fn should_report_daily_active_authn_methods() -> Result<(), CallError> {
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
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?;

        env.advance_time(Duration::from_secs(DAY_SECONDS));

        // some activity is required to update the stats
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?;

        let metrics = get_metrics(&env, canister_id);
        assert_metric(
            &metrics,
            &format!(
                "internet_identity_daily_active_authn_methods{{type=\"{}\"}}",
                authn_type
            ),
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
fn should_report_monthly_active_authn_methods() -> Result<(), CallError> {
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
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?;

        env.advance_time(Duration::from_secs(MONTH_SECONDS));

        // some activity is required to update the stats
        api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?;

        let metrics = get_metrics(&env, canister_id);
        assert_metric(
            &metrics,
            &format!(
                "internet_identity_monthly_active_authn_methods{{type=\"{}\"}}",
                authn_type
            ),
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

/// Tests that only authn methods bound to an II domain are counted.
#[test]
fn should_only_count_ii_domain_authn_methods() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let non_ii_authn_method = test_authn_method(); // not bound to II domain
    let ii_authn_method = AuthnMethodData {
        metadata: HashMap::from([(
            "origin".to_string(),
            MetadataEntry::String("https://identity.ic0.app".to_string()),
        )]),
        ..test_authn_method()
    };

    // some activity on an II domain is required to initialize the stats
    create_identity_with_authn_method(&env, canister_id, &ii_authn_method);
    // let a day pass to not have the initialization influence the result
    env.advance_time(Duration::from_secs(DAY_SECONDS));

    let identity_nr = create_identity_with_authn_method(&env, canister_id, &non_ii_authn_method);
    env.advance_time(Duration::from_secs(DAY_SECONDS));
    // some activity is required to update the stats
    create_identity_with_authn_method(&env, canister_id, &ii_authn_method);

    // assert all counters are 0
    assert_labelled_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_daily_active_authn_methods",
        0f64,
        "type",
        &AUTHN_METHOD_TYPES,
    );

    // let a month pass to not have the daily stats affect the monthly result
    env.advance_time(Duration::from_secs(MONTH_SECONDS));

    // non-ii activity
    api_v2::identity_info(
        &env,
        canister_id,
        non_ii_authn_method.principal(),
        identity_nr,
    )?;
    env.advance_time(Duration::from_secs(MONTH_SECONDS));
    // some activity on an II domain is required to update the stats
    create_identity_with_authn_method(&env, canister_id, &ii_authn_method);

    // assert all counters are 0
    assert_labelled_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_monthly_active_authn_methods",
        0f64,
        "type",
        &AUTHN_METHOD_TYPES,
    );

    Ok(())
}

/// Tests that active authn method stats are kept across upgrades from the same release.
#[test]
fn should_keep_stats_across_upgrades() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_method = AuthnMethodData {
        metadata: HashMap::from([(
            "origin".to_string(),
            MetadataEntry::String("https://identity.ic0.app".to_string()),
        )]),
        ..test_authn_method()
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
    api_v2::identity_info(&env, canister_id, authn_method.principal(), identity_nr)?;

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_daily_active_authn_methods{type=\"other\"}",
        1f64,
    );

    Ok(())
}

fn authn_methods_all_types() -> Vec<(String, AuthnMethodData)> {
    let webauthn_authn_method = AuthnMethod::WebAuthn(WebAuthn {
        pubkey: ByteBuf::from("example pubkey"),
        credential_id: ByteBuf::from("example credential id"),
    });
    let ii_domain_entry = (
        "origin".to_string(),
        MetadataEntry::String("https://identity.ic0.app".to_string()),
    );
    vec![
        (
            "other".to_string(),
            AuthnMethodData {
                metadata: HashMap::from([ii_domain_entry.clone()]),
                ..test_authn_method()
            },
        ),
        (
            "webauthn_auth".to_string(),
            AuthnMethodData {
                authn_method: webauthn_authn_method.clone(),
                metadata: HashMap::from([ii_domain_entry.clone()]),
                ..test_authn_method()
            },
        ),
        (
            "webauthn_recovery".to_string(),
            AuthnMethodData {
                authn_method: webauthn_authn_method,
                purpose: Purpose::Recovery,
                metadata: HashMap::from([ii_domain_entry.clone()]),
                ..test_authn_method()
            },
        ),
        (
            "recovery_phrase".to_string(),
            AuthnMethodData {
                metadata: HashMap::from([
                    (
                        "usage".to_string(),
                        MetadataEntry::String("recovery_phrase".to_string()),
                    ),
                    ii_domain_entry.clone(),
                ]),
                ..test_authn_method()
            },
        ),
        (
            "browser_storage_key".to_string(),
            AuthnMethodData {
                metadata: HashMap::from([
                    (
                        "usage".to_string(),
                        MetadataEntry::String("browser_storage_key".to_string()),
                    ),
                    ii_domain_entry.clone(),
                ]),
                ..test_authn_method()
            },
        ),
    ]
}
