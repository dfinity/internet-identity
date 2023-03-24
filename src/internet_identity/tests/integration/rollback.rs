//! Tests for making sure that the current version can be rolled back to from the last release.
//! This tests stable memory compatibility and pre / post install hooks.

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_test_state_machine_client::CallError;
use serde_bytes::ByteBuf;

/// Tests simple upgrade and downgrade.
#[test]
fn ii_canister_can_be_upgraded_and_rolled_back() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    api::health_check(&env, canister_id);
    upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());
    api::health_check(&env, canister_id);
}

/// Tests that the devices can still be read after upgrade and rollback.
#[test]
fn upgrade_and_rollback_keeps_anchor_intact() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let mut devices_before = api::get_anchor_info(&env, canister_id, principal_1(), user_number)
        .unwrap()
        .devices;
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    api::health_check(&env, canister_id);
    upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());
    api::health_check(&env, canister_id);
    let mut devices_after = api::get_anchor_info(&env, canister_id, principal_1(), user_number)
        .unwrap()
        .devices;

    devices_before.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
    devices_after.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));

    assert_eq!(devices_before, devices_after);
}

/// Verifies that an anchor that was created with the new version of II can still be used when
/// II is rolled back to the previous version.
#[test]
fn should_keep_new_anchor_across_rollback() -> Result<(), CallError> {
    let frontend_hostname = "frontend.com";
    let env = env();

    // start with the previous release to initialize v1 layout
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    api::init_salt(&env, canister_id)?;

    // use the new version to register an anchor
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let principal = api::get_principal(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.to_string(),
    )?;

    // roll back
    upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());

    // use anchor
    let devices =
        api::get_anchor_info(&env, canister_id, principal_1(), user_number)?.into_device_data();
    assert_eq!(devices, vec![device_data_1()]);

    let (user_key, _) = api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        frontend_hostname.to_string(),
        ByteBuf::from("session key"),
        None,
    )?;
    assert_eq!(Principal::self_authenticating(user_key), principal);

    // make sure devices can also be modified
    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        device_data_2(),
    )?;
    Ok(())
}
