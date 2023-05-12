//! Tests that `get_identity_info` returns the correct information.

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::flows;
use canister_tests::framework::{
    device_data_1, device_data_2, env, expect_user_error_with_message, install_ii_canister,
    principal_1, recovery_device_data_1, recovery_device_data_2, time, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use ic_test_state_machine_client::{CallError, StateMachine};
use internet_identity_interface::internet_identity::types::{
    AuthnMethodData, AuthnMethodRegistration, DeviceData, IdentityInfoResponse, IdentityNumber,
    KeyType, MetadataEntry,
};
use regex::Regex;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::time::Duration;

#[test]
fn should_get_identity_info() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let devices = sample_devices();
    let recovery_devices = sample_recovery_devices();

    let identity_number = create_identity_with_devices(
        &env,
        canister_id,
        &devices
            .iter()
            .chain(recovery_devices.iter())
            .cloned()
            .collect::<Vec<_>>(),
    );

    let Some(IdentityInfoResponse::Ok(identity_info)) =
        api_v2::get_identity_info(&env, canister_id, devices[0].principal(), identity_number)? else {
        panic!("Expected identity info to be returned");
    };

    assert_eq_ignoring_last_authentication(&identity_info.authn_methods, &devices);
    assert_eq_ignoring_last_authentication(
        &identity_info.recovery_authn_methods,
        &recovery_devices,
    );
    assert_eq!(identity_info.authn_method_registration, None);

    // check that the last authentication timestamp is set correctly
    assert_eq!(
        identity_info.authn_methods[0].last_authentication,
        Some(time(&env))
    );
    assert_eq!(identity_info.authn_methods[1].last_authentication, None);

    Ok(())
}

#[test]
fn should_require_authentication_for_identity_info() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let devices = sample_devices();
    let identity_number = create_identity_with_devices(&env, canister_id, &devices);

    let result =
        api_v2::get_identity_info(&env, canister_id, Principal::anonymous(), identity_number);

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
    Ok(())
}

#[test]
fn should_provide_authn_registration() -> Result<(), CallError> {
    const AUTHN_METHOD_REGISTRATION_TIMEOUT: u64 = Duration::from_secs(900).as_nanos() as u64; // 15 minutes
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let identity_number = flows::register_anchor(&env, canister_id);
    let device = DeviceData {
        metadata: Some(HashMap::from([(
            "recovery_metadata_1".to_string(),
            MetadataEntry::String("recovery data 1".to_string()),
        )])),
        ..recovery_device_data_1()
    };

    api::enter_device_registration_mode(&env, canister_id, principal_1(), identity_number)?;
    api::add_tentative_device(
        &env,
        canister_id,
        Principal::anonymous(),
        identity_number,
        &device,
    )?;

    let Some(IdentityInfoResponse::Ok(identity_info)) =
        api_v2::get_identity_info(&env, canister_id, principal_1(), identity_number)? else {
        panic!("Expected identity info to be returned");
    };

    assert_eq!(
        identity_info.authn_method_registration,
        Some(AuthnMethodRegistration {
            expiration: time(&env) + AUTHN_METHOD_REGISTRATION_TIMEOUT,
            authn_method: Some(AuthnMethodData::from(device))
        })
    );
    Ok(())
}

fn assert_eq_ignoring_last_authentication(
    authn_methods: &[AuthnMethodData],
    devices: &[DeviceData],
) {
    assert_eq!(
        authn_methods
            .iter()
            .cloned()
            .map(|mut pass| {
                pass.last_authentication = None;
                pass
            })
            .collect::<Vec<_>>(),
        devices
            .iter()
            .cloned()
            .map(AuthnMethodData::from)
            .collect::<Vec<_>>()
    );
}

fn create_identity_with_devices(
    env: &StateMachine,
    canister_id: CanisterId,
    devices: &[DeviceData],
) -> IdentityNumber {
    let mut iter = devices.iter();
    let device1 = iter.next().unwrap();
    let identity_number = flows::register_anchor_with_device(env, canister_id, device1);
    for (idx, device) in iter.enumerate() {
        api::add(
            env,
            canister_id,
            device1.principal(),
            identity_number,
            device,
        )
        .unwrap_or_else(|_| panic!("could not add device {}", idx));
    }
    identity_number
}

fn sample_devices() -> Vec<DeviceData> {
    let device1 = DeviceData {
        metadata: Some(HashMap::from([(
            "some_key".to_string(),
            MetadataEntry::String("some data".to_string()),
        )])),
        ..device_data_1()
    };

    let device2 = DeviceData {
        metadata: Some(HashMap::from([(
            "different_key".to_string(),
            MetadataEntry::String("other data".to_string()),
        )])),
        credential_id: None,
        ..device_data_2()
    };

    vec![device1, device2]
}

fn sample_recovery_devices() -> Vec<DeviceData> {
    let device1 = DeviceData {
        metadata: Some(HashMap::from([(
            "recovery_metadata_1".to_string(),
            MetadataEntry::String("recovery data 1".to_string()),
        )])),
        ..recovery_device_data_1()
    };

    let device2 = DeviceData {
        credential_id: Some(ByteBuf::from("credential id 1")),
        key_type: KeyType::CrossPlatform,
        ..recovery_device_data_2()
    };

    vec![device1, device2]
}
