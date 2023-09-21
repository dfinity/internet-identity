//! Tests for the temp key functionality (during registration).

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::{
    assert_metric, device_data_1, device_data_2, env, expect_user_error_with_message, get_metrics,
    install_ii_canister, test_principal, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::internet_identity::types::{
    AnchorNumber, Challenge, ChallengeAttempt, DeviceData, RegisterResponse,
};
use pocket_ic::{CallError, ErrorCode, PocketIc};
use regex::Regex;
use serde_bytes::ByteBuf;
use std::time::Duration;

/// Tests successful registration with a temporary key.
#[test]
fn should_register_anchor_with_temp_key() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let temp_key = test_principal(1);

    let anchor = register_with_temp_key(&env, canister_id, temp_key, &device_data_1());

    // make an authenticate call to verify that the temp key is working
    api::get_anchor_info(&env, canister_id, temp_key, anchor)?;
    Ok(())
}

/// Tests that the temporary key is removed on device deletion.
#[test]
fn should_remove_temp_key_on_device_deletion() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let temp_key = test_principal(1);
    let device = device_data_1();

    let anchor = register_with_temp_key(&env, canister_id, temp_key, &device);

    api::remove(&env, canister_id, temp_key, anchor, &device.pubkey)?;

    let result = api::get_anchor_info(&env, canister_id, temp_key, anchor);
    expect_user_error_with_message(
        result,
        ErrorCode::CanisterCalledTrap,
        Regex::new("[\\w-]+ could not be authenticated").unwrap(),
    );
    Ok(())
}

/// Tests that the temporary key is removed on device replacement.
#[test]
fn should_remove_temp_key_on_device_replacement() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let temp_key = test_principal(1);
    let device = device_data_1();

    let anchor = register_with_temp_key(&env, canister_id, temp_key, &device);

    api::replace(
        &env,
        canister_id,
        temp_key,
        anchor,
        &device.pubkey,
        &device_data_2(),
    )?;

    let result = api::get_anchor_info(&env, canister_id, temp_key, anchor);
    expect_user_error_with_message(
        result,
        ErrorCode::CanisterCalledTrap,
        Regex::new("[\\w-]+ could not be authenticated").unwrap(),
    );
    Ok(())
}

/// Tests that the temp key expires after 10 minutes.
#[test]
fn should_expire_temp_key() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let temp_key = test_principal(1);
    let device = device_data_1();

    let anchor = register_with_temp_key(&env, canister_id, temp_key, &device);

    env.advance_time(Duration::from_secs(600));

    let result = api::get_anchor_info(&env, canister_id, temp_key, anchor);
    expect_user_error_with_message(
        result,
        ErrorCode::CanisterCalledTrap,
        Regex::new("[\\w-]+ could not be authenticated").unwrap(),
    );
    Ok(())
}

/// Tests that the temp key must be different than the device key.
#[test]
fn should_not_allow_temp_key_to_equal_device_key() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let device = device_data_1();

    let challenge = api::create_challenge(&env, canister_id).unwrap();
    let response = api::register(
        &env,
        canister_id,
        device.principal(),
        &device,
        &challenge_solution(challenge),
        Some(device.principal()),
    );

    expect_user_error_with_message(
        response,
        ErrorCode::CanisterCalledTrap,
        Regex::new("temp_key and device key must not be equal").unwrap(),
    );
    Ok(())
}

/// Tests that the temp key is bound to a specific anchor even if the same device is used on multiple anchors.
#[test]
fn should_not_allow_temp_key_for_different_anchor() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let device = device_data_1();
    let temp_key = test_principal(1);

    let anchor_without_temp_key = flows::register_anchor_with_device(&env, canister_id, &device);
    register_with_temp_key(&env, canister_id, temp_key, &device);

    let result = api::get_anchor_info(&env, canister_id, temp_key, anchor_without_temp_key);
    expect_user_error_with_message(
        result,
        ErrorCode::CanisterCalledTrap,
        Regex::new("[\\w-]+ could not be authenticated").unwrap(),
    );
    Ok(())
}

/// Tests that the number of temp keys is exposed as a metric.
#[test]
fn should_provide_temp_keys_metric() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_temp_keys_count",
        0.0,
    );

    for i in 0..5 {
        register_with_temp_key(&env, canister_id, test_principal(i), &device(i));
    }

    assert_metric(
        &get_metrics(&env, canister_id),
        "internet_identity_temp_keys_count",
        5.0,
    );

    Ok(())
}

fn register_with_temp_key(
    env: &PocketIc,
    canister_id: CanisterId,
    temp_key: Principal,
    device: &DeviceData,
) -> AnchorNumber {
    let challenge = api::create_challenge(env, canister_id).unwrap();
    let response = api::register(
        env,
        canister_id,
        temp_key,
        device,
        &challenge_solution(challenge),
        Some(temp_key),
    )
    .unwrap();

    let RegisterResponse::Registered { user_number } = response else {
        panic!("expected RegisterResponse::Registered");
    };
    user_number
}

fn challenge_solution(challenge: Challenge) -> ChallengeAttempt {
    ChallengeAttempt {
        chars: "a".to_string(),
        key: challenge.challenge_key,
    }
}

fn device(n: u64) -> DeviceData {
    DeviceData {
        pubkey: ByteBuf::from([n as u8; 64]),
        alias: "Device ".to_string() + n.to_string().as_str(),
        ..DeviceData::auth_test_device()
    }
}
