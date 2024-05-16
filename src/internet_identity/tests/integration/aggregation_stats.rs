use crate::v2_api::authn_method_test_helpers::{
    create_identity_with_authn_method, test_authn_method,
};
use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    assert_metric, env, get_metrics, install_ii_canister, upgrade_ii_canister, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::{CallError, StateMachine};
use internet_identity_interface::internet_identity::types::{
    AuthnMethodData, IdentityNumber, MetadataEntryV2,
};
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::time::Duration;

const SESSION_LENGTH: u64 = 900; // 900 seconds
const PD_COUNT: &str = "prepare_delegation_count";
const PD_SESS_SEC: &str = "prepare_delegation_session_seconds";
const PRUNE_EVENT_COUNT: &str = "prune_event_count";

#[test]
fn should_report_expected_aggregations() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    for ii_origin in ["internetcomputer.org", "ic0.app", "other"] {
        let identity_nr = create_identity(&env, canister_id, ii_origin);

        delegation_for_origin(&env, canister_id, identity_nr, "https://some-dapp.com")?;
        delegation_for_origin(&env, canister_id, identity_nr, "https://some-dapp.com")?;

        let aggregations = api::stats(&env, canister_id)?.event_aggregations;
        assert_expected_aggregation(
            &aggregations,
            &aggregation_key(PD_COUNT, "24h", ii_origin),
            vec![("https://some-dapp.com".to_string(), 2u64)],
        );
        assert_expected_aggregation(
            &aggregations,
            &aggregation_key(PD_COUNT, "30d", ii_origin),
            vec![("https://some-dapp.com".to_string(), 2u64)],
        );
        assert_expected_aggregation(
            &aggregations,
            &aggregation_key(PD_SESS_SEC, "24h", ii_origin),
            vec![("https://some-dapp.com".to_string(), 2 * SESSION_LENGTH)],
        );
        assert_expected_aggregation(
            &aggregations,
            &aggregation_key(PD_SESS_SEC, "30d", ii_origin),
            vec![("https://some-dapp.com".to_string(), 2 * SESSION_LENGTH)],
        );
    }
    Ok(())
}

#[test]
fn should_not_report_empty_aggregations() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let ii_origin = "ic0.app";
    let identity_nr = create_identity(&env, canister_id, ii_origin);

    delegation_for_origin(&env, canister_id, identity_nr, "https://some-dapp.com")?;

    let aggregations = api::stats(&env, canister_id)?.event_aggregations;
    let mut expected_keys = vec![
        aggregation_key(PD_COUNT, "24h", ii_origin),
        aggregation_key(PD_COUNT, "30d", ii_origin),
        aggregation_key(PD_SESS_SEC, "24h", ii_origin),
        aggregation_key(PD_SESS_SEC, "30d", ii_origin),
    ];
    let mut keys = aggregations.into_keys().collect::<Vec<_>>();
    // sort for stable comparison
    expected_keys.sort();
    keys.sort();
    assert_eq!(keys, expected_keys);

    env.advance_time(Duration::from_secs(60 * 60 * 24 * 30)); // 30 days
    env.tick(); // execute timers

    let aggregations = api::stats(&env, canister_id)?.event_aggregations;

    // set of keys is now entirely different due to automatic pruning
    let mut expected_keys = vec![
        aggregation_key(PRUNE_EVENT_COUNT, "24h", "other"),
        aggregation_key(PRUNE_EVENT_COUNT, "30d", "other"),
    ];
    let mut keys = aggregations.into_keys().collect::<Vec<_>>();
    // sort for stable comparison
    expected_keys.sort();
    keys.sort();
    assert_eq!(keys, expected_keys);

    Ok(())
}

#[test]
fn should_report_at_most_100_entries() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let ii_origin = "ic0.app";
    let identity_nr = create_identity(&env, canister_id, ii_origin);

    for i in 0..102 {
        delegation_for_origin(
            &env,
            canister_id,
            identity_nr,
            &format!("https://some-dapp-{}", i),
        )?;
    }

    let aggregations = api::stats(&env, canister_id)?.event_aggregations;
    assert_eq!(
        aggregations
            .get(&aggregation_key(PD_COUNT, "24h", ii_origin))
            .unwrap()
            .len(),
        100
    );
    Ok(())
}

#[test]
fn should_keep_aggregations_across_upgrades() -> Result<(), CallError> {
    const II_ORIGIN: &str = "ic0.app";
    fn assert_expected_state(env: &StateMachine, canister_id: CanisterId) -> Result<(), CallError> {
        let aggregations = api::stats(env, canister_id)?.event_aggregations;
        assert_expected_aggregation(
            &aggregations,
            &aggregation_key(PD_COUNT, "24h", II_ORIGIN),
            vec![("https://some-dapp.com".to_string(), 2u64)],
        );
        assert_expected_aggregation(
            &aggregations,
            &aggregation_key(PD_COUNT, "30d", II_ORIGIN),
            vec![("https://some-dapp.com".to_string(), 2u64)],
        );
        assert_expected_aggregation(
            &aggregations,
            &aggregation_key(PD_SESS_SEC, "24h", II_ORIGIN),
            vec![("https://some-dapp.com".to_string(), 2 * SESSION_LENGTH)],
        );
        assert_expected_aggregation(
            &aggregations,
            &aggregation_key(PD_SESS_SEC, "30d", II_ORIGIN),
            vec![("https://some-dapp.com".to_string(), 2 * SESSION_LENGTH)],
        );
        assert_metric(
            &get_metrics(env, canister_id),
            "internet_identity_event_data_count",
            2f64,
        );
        assert_metric(
            &get_metrics(env, canister_id),
            "internet_identity_event_aggregations_count",
            4f64,
        );
        Ok(())
    }

    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_nr = create_identity(&env, canister_id, II_ORIGIN);

    delegation_for_origin(&env, canister_id, identity_nr, "https://some-dapp.com")?;
    delegation_for_origin(&env, canister_id, identity_nr, "https://some-dapp.com")?;

    assert_expected_state(&env, canister_id)?;

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    assert_expected_state(&env, canister_id)?;
    Ok(())
}

#[test]
fn should_prune_automatically_after_upgrade() -> Result<(), CallError> {
    const II_ORIGIN: &str = "ic0.app";
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_nr = create_identity(&env, canister_id, II_ORIGIN);

    delegation_for_origin(&env, canister_id, identity_nr, "https://some-dapp.com")?;

    let aggregations = api::stats(&env, canister_id)?.event_aggregations;
    let mut expected_keys = vec![
        aggregation_key(PD_COUNT, "24h", II_ORIGIN),
        aggregation_key(PD_COUNT, "30d", II_ORIGIN),
        aggregation_key(PD_SESS_SEC, "24h", II_ORIGIN),
        aggregation_key(PD_SESS_SEC, "30d", II_ORIGIN),
    ];
    let mut keys = aggregations.into_keys().collect::<Vec<_>>();
    // sort for stable comparison
    expected_keys.sort();
    keys.sort();
    assert_eq!(keys, expected_keys);

    // upgrade then advance time to trigger pruning
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    env.advance_time(Duration::from_secs(60 * 60 * 24 * 30)); // 30 days
    env.tick(); // execute timers

    let aggregations = api::stats(&env, canister_id)?.event_aggregations;

    // set of keys only reflects pruning now
    let mut expected_keys = vec![
        aggregation_key(PRUNE_EVENT_COUNT, "24h", "other"),
        aggregation_key(PRUNE_EVENT_COUNT, "30d", "other"),
    ];
    let mut keys = aggregations.into_keys().collect::<Vec<_>>();
    // sort for stable comparison
    expected_keys.sort();
    keys.sort();
    assert_eq!(keys, expected_keys);

    Ok(())
}

fn assert_expected_aggregation(
    aggregations: &HashMap<String, Vec<(String, u64)>>,
    key: &str,
    data: Vec<(String, u64)>,
) {
    assert_eq!(
        aggregations.get(key).unwrap(),
        &data,
        "Aggregation key \"{}\" does not match",
        key
    );
}

fn create_identity(env: &StateMachine, canister_id: CanisterId, ii_origin: &str) -> IdentityNumber {
    let authn_method = AuthnMethodData {
        metadata: HashMap::from([(
            "origin".to_string(),
            MetadataEntryV2::String(format!("https://identity.{}", ii_origin)),
        )]),
        ..test_authn_method()
    };
    create_identity_with_authn_method(env, canister_id, &authn_method)
}

fn aggregation_key(aggregation: &str, window: &str, ii_domain: &str) -> String {
    format!("{} {} {}", aggregation, window, ii_domain)
}

fn delegation_for_origin(
    env: &StateMachine,
    canister_id: CanisterId,
    identity_nr: IdentityNumber,
    frontend_hostname: &str,
) -> Result<(), CallError> {
    api::prepare_delegation(
        env,
        canister_id,
        test_authn_method().principal(),
        identity_nr,
        frontend_hostname,
        &ByteBuf::from("session public key"),
        Some(SESSION_LENGTH * 10u64.pow(9)), // seconds to nanoseconds
    )?;
    Ok(())
}
