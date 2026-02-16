//! Tests for making sure that the current version can be rolled back to from the last release.
//! This tests stable memory compatibility and pre / post install hooks.

use canister_tests::api::internet_identity as api;
use canister_tests::api::internet_identity::api_v2::create_account;
use canister_tests::api::internet_identity::api_v2::get_accounts;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::DeviceData;

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
        .devices
        .into_iter()
        .map(DeviceData::from)
        .collect::<Vec<_>>();

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    api::health_check(&env, canister_id);
    upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());
    api::health_check(&env, canister_id);
    let mut devices_after = api::get_anchor_info(&env, canister_id, principal_1(), user_number)
        .unwrap()
        .devices
        .into_iter()
        .map(DeviceData::from)
        .collect::<Vec<_>>();

    devices_before.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
    devices_after.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));

    assert_eq!(devices_before, devices_after);
}

/* TODO: Reenable after the next upgrade
/// Verifies that an anchor that was created with the new version of II can still be used when
/// II is rolled back to the previous version.
#[test]
fn should_keep_new_anchor_across_rollback() -> Result<(), RejectResponse> {
    let frontend_hostname = "frontend.com";
    let env = env();

    // start with the previous release
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
        frontend_hostname,
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
        frontend_hostname,
        &ByteBuf::from("session key"),
        None,
    )?;
    assert_eq!(Principal::self_authenticating(user_key), principal);

    // make sure devices can also be modified
    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;
    Ok(())
}*/

/// Tests that the accounts can still be read after upgrade and rollback.
#[test]
fn upgrade_and_rollback_keeps_accounts_intact() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let origin = "https://www.wassilykandinsky.net/".to_string();
    let name_a = "Bauhaus Archiv Museum".to_string();
    let name_b = "Zentrum Paul Klee".to_string();

    create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name_a.clone(),
    )
    .unwrap()
    .unwrap();

    create_account(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
        name_b.clone(),
    )
    .unwrap()
    .unwrap();

    let mut accounts_before = get_accounts(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
    )
    .unwrap()
    .unwrap();

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    api::health_check(&env, canister_id);

    let mut accounts_between = get_accounts(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
    )
    .unwrap()
    .unwrap();

    upgrade_ii_canister(&env, canister_id, II_WASM_PREVIOUS.clone());
    api::health_check(&env, canister_id);

    let mut accounts_after = get_accounts(
        &env,
        canister_id,
        principal_1(),
        identity_number,
        origin.clone(),
    )
    .unwrap()
    .unwrap();

    accounts_after.sort_by_key(|account_info| account_info.account_number);
    accounts_between.sort_by_key(|account_info| account_info.account_number);
    accounts_before.sort_by_key(|account_info| account_info.account_number);

    assert_eq!(accounts_before, accounts_between);
    assert_eq!(accounts_between, accounts_after);
}
