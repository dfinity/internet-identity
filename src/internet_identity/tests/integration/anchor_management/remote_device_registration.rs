//! Tests concerning the device registration flow for remote devices (i.e. authenticators on another computer).
//! The flow has the following steps:
//! 1. on device 1: enter registration mode
//! 2. on device 2: register the new device tentatively -> this returns a verification code
//! 3. on device 1: enter the verification code
//!
//! Additionally, there are the following bounds on the registration flow:
//! 1. registration mode expires after 15 minutes
//! 2. there is a limit of 3 attempts for step 3 in the above process

use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::CallError;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use regex::Regex;
use std::ops::Add;
use std::time::{Duration, UNIX_EPOCH};

/// Test entering registration mode including returned expiration time.
#[test]
fn can_enter_device_registration_mode() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let result =
        api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;

    assert_eq!(
        result,
        env.time()
            .add(Duration::from_secs(900)) // 900 seconds -> 15 min
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64
    );
    Ok(())
}

/// Tests that only an authenticated user can enter device registration mode for themselves.
#[test]
fn can_not_enter_device_registration_mode_for_other_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let result = api::enter_device_registration_mode(&env, canister_id, principal_2(), user_number);

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z0-9-]+ could not be authenticated.").unwrap(),
    );
}

/// Tests that the device registration flow can be completed successfully.
#[test]
fn can_register_remote_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
    let add_response = api::add_tentative_device(&env, canister_id, user_number, &device_data_2())?;
    let verification_code = match add_response {
        AddTentativeDeviceResponse::AddedTentatively {
            verification_code, ..
        } => verification_code,
        err => panic!("failed to add tentative device: {err:?}"),
    };
    let verification_response = api::verify_tentative_device(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &verification_code,
    )?;

    assert!(matches!(
        verification_response,
        VerifyTentativeDeviceResponse::Verified
    ));
    Ok(())
}

/// Tests that the device registration flow can be completed successfully after submitting an invalid code.
#[test]
fn can_verify_remote_device_after_failed_attempt() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
    let add_response = api::add_tentative_device(&env, canister_id, user_number, &device_data_2())?;
    let verification_code = match add_response {
        AddTentativeDeviceResponse::AddedTentatively {
            verification_code, ..
        } => verification_code,
        err => panic!("failed to add tentative device: {err:?}"),
    };

    assert!(matches!(
        api::verify_tentative_device(
            &env,
            canister_id,
            principal_1(),
            user_number,
            "invalid code"
        )?,
        VerifyTentativeDeviceResponse::WrongCode { retries_left: 2 }
    ));

    let verification_response = api::verify_tentative_device(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &verification_code,
    )?;

    assert!(matches!(
        verification_response,
        VerifyTentativeDeviceResponse::Verified
    ));
    Ok(())
}

/// Tests that the anchor info call returns information about tentative devices.
/// This enables the front-end to continue an in progress flow (e.g. after a refresh of the page).
#[test]
fn anchor_info_should_return_tentative_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
    let new_device = device_data_2();
    api::add_tentative_device(&env, canister_id, user_number, &new_device)?;
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;

    assert!(matches!(
        anchor_info.device_registration,
        Some(DeviceRegistrationInfo {
            tentative_device: Some(tenative_device),
            ..
        }) if tenative_device == new_device
    ));
    Ok(())
}

/// Tests that devices cannot be registered tentatively if the registration mode is not enabled.
#[test]
fn reject_tentative_device_if_not_in_registration_mode() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
    api::exit_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
    let result = api::add_tentative_device(&env, canister_id, user_number, &device_data_2())?;

    assert!(matches!(
        result,
        AddTentativeDeviceResponse::DeviceRegistrationModeOff
    ));
    Ok(())
}

/// Tests device registration mode expiration.
#[test]
fn reject_tentative_device_if_registration_mode_is_expired() -> Result<(), CallError> {
    const REGISTRATION_MODE_EXPIRATION: Duration = Duration::from_secs(900);
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
    env.advance_time(REGISTRATION_MODE_EXPIRATION + Duration::from_secs(1));
    let result = api::add_tentative_device(&env, canister_id, user_number, &device_data_2())?;

    assert!(matches!(
        result,
        AddTentativeDeviceResponse::DeviceRegistrationModeOff
    ));
    Ok(())
}

/// Tests that an appropriate result is returned when a verification code is submitted without a
/// corresponding tentative device.
#[test]
fn reject_verification_without_tentative_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
    let verification_response =
        api::verify_tentative_device(&env, canister_id, principal_1(), user_number, "some code")?;

    assert!(matches!(
        verification_response,
        VerifyTentativeDeviceResponse::NoDeviceToVerify
    ));
    Ok(())
}

/// Tests that the flow is aborted after the expected number of failed verification attempts.
#[test]
fn reject_verification_with_wrong_code() -> Result<(), CallError> {
    const MAX_RETRIES: u8 = 3;
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::enter_device_registration_mode(&env, canister_id, principal_1(), user_number)?;
    api::add_tentative_device(&env, canister_id, user_number, &device_data_2())?;
    for expected_retries in (0..MAX_RETRIES).rev() {
        assert!(matches!(
            api::verify_tentative_device(
                &env,
                canister_id,
                principal_1(),
                user_number,
                "invalid code"
            )?,
            VerifyTentativeDeviceResponse::WrongCode {
                retries_left
            } if retries_left == expected_retries
        ));
    }

    assert!(matches!(
        api::verify_tentative_device(
            &env,
            canister_id,
            principal_1(),
            user_number,
            "invalid code"
        )?,
        VerifyTentativeDeviceResponse::DeviceRegistrationModeOff
    ));
    Ok(())
}
