//! Tests for tracking the latest delegation origins.

use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::{
    device_data_1, env, install_ii_canister, install_ii_canister_with_arg, principal_1,
    upgrade_ii_canister, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::{CallError, StateMachine};
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, DeviceData, InternetIdentityInit,
};
use serde_bytes::ByteBuf;
use std::time::Duration;

const DAY_SECONDS: u64 = 24 * 60 * 60;

/// Verifies that origins are recorded.
#[test]
fn should_record_used_delegation_origin() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    delegation_for_origin(&env, canister_id, user_number, "https://some-dapp.com")?;

    let latest_origins = api::stats(&env, canister_id)?.latest_delegation_origins;
    assert_eq!(latest_origins, vec!["https://some-dapp.com".to_string()]);
    Ok(())
}

/// Verifies that the configured limit for the number of latest used origins is respected.
#[test]
fn should_record_max_delegation_origins() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_with_latest_origin_limit(&env, 10);
    let user_number = flows::register_anchor(&env, canister_id);

    for i in 0..11 {
        delegation_for_origin(&env, canister_id, user_number, &format!("dapp-{}", i))?;
        env.advance_time(Duration::from_secs(1)); // let time pass so the ordering is deterministic
    }
    let latest_origins = api::stats(&env, canister_id)?.latest_delegation_origins;

    assert_eq!(latest_origins.len(), 10);
    assert!(!latest_origins.contains(&"dapp-0".to_string())); // the oldest has been pruned
    assert!(latest_origins.contains(&"dapp-10".to_string()));
    Ok(())
}

/// Verifies that the latest origins are dropped after 30 days.
#[test]
fn should_drop_orgins_after_30_days() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    delegation_for_origin(&env, canister_id, user_number, "https://dropped.com")?;
    delegation_for_origin(&env, canister_id, user_number, "https://not-dropped.com")?;
    env.advance_time(Duration::from_secs(15 * DAY_SECONDS));
    // repeated activity to refresh the timestamp (for only one origin)
    delegation_for_origin(&env, canister_id, user_number, "https://not-dropped.com")?;
    env.advance_time(Duration::from_secs(15 * DAY_SECONDS));
    // activity to refresh the list
    delegation_for_origin(&env, canister_id, user_number, "https://unrelated.com")?;

    let latest_origins = api::stats(&env, canister_id)?.latest_delegation_origins;

    assert!(latest_origins.contains(&"https://not-dropped.com".to_string()));
    assert!(!latest_origins.contains(&"https://dropped.com".to_string()));
    Ok(())
}

/// Verifies that the latest origins are kept across upgrades.
#[test]
fn should_keep_latest_origins_across_upgrades() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    delegation_for_origin(&env, canister_id, user_number, "https://some-dapp.com")?;
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let latest_origins = api::stats(&env, canister_id)?.latest_delegation_origins;
    assert_eq!(latest_origins, vec!["https://some-dapp.com".to_string()]);
    Ok(())
}

/// Verifies that origins are recorded only for devices on II domains.
#[test]
fn should_not_record_delegation_origin_for_devices_on_non_ii_domains() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let device = DeviceData {
        origin: Some("https://some-other-domain.com".to_string()),
        ..device_data_1()
    };
    let user_number = flows::register_anchor_with_device(&env, canister_id, &device);

    delegation_for_origin(&env, canister_id, user_number, "https://some-dapp.com")?;

    let latest_origins = api::stats(&env, canister_id)?.latest_delegation_origins;
    assert!(latest_origins.is_empty());
    Ok(())
}

fn install_ii_with_latest_origin_limit(env: &StateMachine, limit: u64) -> CanisterId {
    install_ii_canister_with_arg(
        env,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            max_num_latest_delegation_origins: Some(limit),
            ..Default::default()
        }),
    )
}

fn delegation_for_origin(
    env: &StateMachine,
    canister_id: CanisterId,
    user_number: AnchorNumber,
    frontend_hostname: &str,
) -> Result<(), CallError> {
    api::prepare_delegation(
        env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname,
        &ByteBuf::from("session public key"),
        None,
    )?;
    Ok(())
}
