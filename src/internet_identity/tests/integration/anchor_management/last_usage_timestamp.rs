//! Tests related to the last_usage_timestamp.

use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_test_state_machine_client::CallError;
use internet_identity_interface::internet_identity::types::*;
use serde_bytes::ByteBuf;
use std::time::Duration;

/// Tests that get_anchor_info updates the last usage on the added device.
#[test]
fn should_set_last_usage_on_get_anchor_info() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    env.advance_time(Duration::from_secs(1));

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
    assert_device_last_used(&anchor_info, &device_data_1().pubkey, time(&env));
    Ok(())
}

/// Tests that add only updates last usage on the device used to authenticate the call (not the newly added one).
#[test]
fn should_set_last_usage_on_add() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &recovery_device_data_1(),
    )?;

    env.advance_time(Duration::from_secs(1));
    let expected_timestamp = time(&env);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;

    env.advance_time(Duration::from_secs(1));

    // use the recovery device to get the info, otherwise getting the info will update the timestamp we want to verify
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_recovery_1(), user_number)?;

    assert!(anchor_info
        .devices
        .contains(&DeviceWithUsage::from(device_data_2()))); // without last usage timestamp

    assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

    Ok(())
}

/// Tests that remove updates last usage on the device used to authenticate the call.
#[test]
fn should_set_last_usage_on_remove() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &recovery_device_data_1(),
    )?;
    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;

    env.advance_time(Duration::from_secs(1));
    let expected_timestamp = time(&env);

    api::remove(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2().pubkey,
    )?;

    env.advance_time(Duration::from_secs(1));

    // use the recovery device to get the info, otherwise getting the info will update the timestamp we want to verify
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_recovery_1(), user_number)?;

    assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

    Ok(())
}

/// Tests that update only updates last usage on the device used to authenticate the call (not the updated one).
#[test]
fn should_set_last_usage_on_update() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &recovery_device_data_1(),
    )?;
    let mut device_to_be_updated = device_data_2();
    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_to_be_updated,
    )?;

    env.advance_time(Duration::from_secs(1));
    let expected_timestamp_1 = time(&env);

    // use the device_to_be_updated to create a last usage timestamp
    api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;

    env.advance_time(Duration::from_secs(1));
    let expected_timestamp_2 = time(&env);

    device_to_be_updated.alias = "changed value".to_string();

    // this update is should keep the last usage timestamp of device_to_be_updated and update
    // the last usage of device_1
    api::update(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_to_be_updated.pubkey,
        &device_to_be_updated,
    )?;

    env.advance_time(Duration::from_secs(1));

    // use the recovery device to get the info, otherwise getting the info will update the timestamp we want to verify
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_recovery_1(), user_number)?;

    assert!(anchor_info
        .devices
        .clone()
        .into_iter()
        .map(DeviceData::from)
        .any(|d| d == device_to_be_updated));

    assert_device_last_used(&anchor_info, &device_data_2().pubkey, expected_timestamp_1);
    assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp_2);

    Ok(())
}

/// Tests that replace only updates last usage on the device used to authenticate the call (not the replacement device).
#[test]
fn should_set_last_usage_on_replace() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &recovery_device_data_1(),
    )?;
    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;

    env.advance_time(Duration::from_secs(1));
    let expected_timestamp = time(&env);

    api::replace(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2().pubkey,
        &max_size_device(),
    )?;

    env.advance_time(Duration::from_secs(1));

    // use the recovery device to get the info, otherwise getting the info will update the timestamp we want to verify
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_recovery_1(), user_number)?;

    assert!(anchor_info
        .devices
        .contains(&DeviceWithUsage::from(max_size_device()))); // without last usage timestamp

    assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

    Ok(())
}

/// Tests that prepare_delegation updates the last usage timestamp.
#[test]
fn should_set_last_usage_on_prepare_delegation() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    let pub_session_key = ByteBuf::from("session public key");

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;

    env.advance_time(Duration::from_secs(1));
    let expected_timestamp = time(&env);

    api::prepare_delegation(
        &env,
        canister_id,
        principal_1(),
        user_number,
        "https://some-dapp.com",
        &pub_session_key,
        None,
    )?;

    env.advance_time(Duration::from_secs(1));

    // use the device2 to get the info, otherwise getting the info will update the timestamp we want to verify
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
    assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

    Ok(())
}

/// Tests that calls related to tentative device registration update the last usage timestamp.
#[test]
fn should_update_last_usage_on_tentative_device_registration() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;

    env.advance_time(Duration::from_secs(1));
    let expected_timestamp = time(&env);

    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;

    env.advance_time(Duration::from_secs(1));

    // use the device2 to get the info, otherwise getting the info will update the timestamp we want to verify
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
    assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

    let verification_code = match api::add_tentative_device(
        &env,
        canister_id,
        principal_recovery_1(),
        user_number,
        &recovery_device_data_1(),
    )? {
        AddTentativeDeviceResponse::AddedTentatively {
            verification_code, ..
        } => verification_code,
        _ => panic!("unexpected response"),
    };

    env.advance_time(Duration::from_secs(1));
    let expected_timestamp = time(&env);

    api::verify_tentative_device(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &verification_code,
    )?;

    // use the device2 to get the info, otherwise getting the info will update the timestamp we want to verify
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
    assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

    Ok(())
}

/// Tests that exit_device_registration_mode updates the last usage timestamp.
#[test]
fn should_update_last_usage_on_exit_device_registration_mode() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;

    env.advance_time(Duration::from_secs(1));
    let expected_timestamp = time(&env);

    api::exit_device_registration_mode(&env, canister_id, principal_1(), user_number)?;

    // use the device2 to get the info, otherwise getting the info will update the timestamp we want to verify
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
    assert_device_last_used(&anchor_info, &device_data_1().pubkey, expected_timestamp);

    Ok(())
}
