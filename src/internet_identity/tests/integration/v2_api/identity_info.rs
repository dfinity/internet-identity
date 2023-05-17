//! Tests that `get_identity_info` returns the correct information.

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::flows;
use canister_tests::framework::{
    env, expect_user_error_with_message, install_ii_canister, time, II_WASM,
};
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use ic_test_state_machine_client::{CallError, StateMachine};
use internet_identity_interface::internet_identity::types::{
    AuthnMethodAddResponse, AuthnMethodData, AuthnMethodRegistration, DeviceData,
    IdentityInfoResponse, IdentityNumber, KeyType, MetadataEntry, Purpose,
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
    let identity_number = create_identity_with_devices(&env, canister_id, &devices);

    let Some(IdentityInfoResponse::Ok(identity_info)) =
        api_v2::identity_info(&env, canister_id, devices[0].principal(), identity_number)? else {
        panic!("Expected identity info to be returned");
    };

    assert_eq_ignoring_last_authentication(&identity_info.authn_methods, &devices);
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

    let result = api_v2::identity_info(&env, canister_id, Principal::anonymous(), identity_number);

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
    let device1 = sample_devices().remove(0);
    let identity_number = flows::register_anchor_with_device(&env, canister_id, &device1);
    let device2 = DeviceData {
        pubkey: ByteBuf::from([55; 32]),
        metadata: Some(HashMap::from([(
            "recovery_metadata_1".to_string(),
            MetadataEntry::String("recovery data 1".to_string()),
        )])),
        ..device1.clone()
    };

    api::enter_device_registration_mode(&env, canister_id, device1.principal(), identity_number)?;
    api::add_tentative_device(&env, canister_id, identity_number, &device2)?;

    let Some(IdentityInfoResponse::Ok(identity_info)) =
        api_v2::identity_info(&env, canister_id, device1.principal(), identity_number)? else {
        panic!("Expected identity info to be returned");
    };

    assert_eq!(
        identity_info.authn_method_registration,
        Some(AuthnMethodRegistration {
            expiration: time(&env) + AUTHN_METHOD_REGISTRATION_TIMEOUT,
            authn_method: Some(AuthnMethodData::from(device2))
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
        let response = api_v2::authn_method_add(
            env,
            canister_id,
            device1.principal(),
            identity_number,
            &AuthnMethodData::from(device.clone()),
        )
        .unwrap_or_else(|_| panic!("could not add device {}", idx))
        .unwrap();
        assert!(matches!(response, AuthnMethodAddResponse::Ok));
    }
    identity_number
}

fn sample_devices() -> Vec<DeviceData> {
    let device1 = DeviceData {
        metadata: Some(HashMap::from([(
            "some_key".to_string(),
            MetadataEntry::String("some data".to_string()),
        )])),
        origin: Some("https://some.origin".to_string()),
        ..DeviceData::auth_test_device()
    };

    let device2 = DeviceData {
        pubkey: ByteBuf::from([1; 32]),
        credential_id: Some(ByteBuf::from([12; 32])),
        metadata: Some(HashMap::from([(
            "different_key".to_string(),
            MetadataEntry::String("other data".to_string()),
        )])),
        ..DeviceData::auth_test_device()
    };
    let device3 = DeviceData {
        pubkey: ByteBuf::from([2; 32]),
        purpose: Purpose::Recovery,
        key_type: KeyType::SeedPhrase,
        metadata: Some(HashMap::from([(
            "recovery_metadata_1".to_string(),
            MetadataEntry::String("recovery data 1".to_string()),
        )])),
        ..DeviceData::auth_test_device()
    };
    let device4 = DeviceData {
        pubkey: ByteBuf::from([3; 32]),
        purpose: Purpose::Recovery,
        credential_id: Some(ByteBuf::from("credential id 1")),
        key_type: KeyType::CrossPlatform,
        ..DeviceData::auth_test_device()
    };

    vec![device1, device2, device3, device4]
}
