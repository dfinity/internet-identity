//! Tests that `get_identity_info` returns the correct information.

use crate::v2_api::authn_method_test_helpers::{
    assert_eq_ignoring_last_authentication, create_identity_with_authn_method,
    create_identity_with_authn_methods, sample_authn_methods,
};
use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::api::internet_identity::api_v2;
use canister_tests::framework::{
    env, expect_user_error_with_message, install_ii_canister, time, II_WASM,
};
use ic_test_state_machine_client::CallError;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use internet_identity_interface::internet_identity::types::{
    AuthnMethodData, AuthnMethodRegistration, DeviceData, DeviceWithUsage, MetadataEntry,
};
use regex::Regex;
use serde_bytes::ByteBuf;
use std::collections::HashMap;
use std::time::Duration;

#[test]
fn should_get_identity_info() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let authn_methods = sample_authn_methods();
    let identity_number = create_identity_with_authn_methods(&env, canister_id, &authn_methods);

    let identity_info = api_v2::identity_info(
        &env,
        canister_id,
        authn_methods[0].principal(),
        identity_number,
    )?
    .expect("identity info failed");

    assert_eq_ignoring_last_authentication(&identity_info.authn_methods, &authn_methods);
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
    let authn_methods = sample_authn_methods();
    let identity_number = create_identity_with_authn_methods(&env, canister_id, &authn_methods);

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
    let authn_method1 = sample_authn_methods().remove(0);
    let identity_number = create_identity_with_authn_method(&env, canister_id, &authn_method1);
    let device2 = DeviceData {
        pubkey: ByteBuf::from([55; 32]),
        metadata: Some(HashMap::from([(
            "recovery_metadata_1".to_string(),
            MetadataEntry::String("recovery data 1".to_string()),
        )])),
        ..DeviceData::from(DeviceWithUsage::try_from(authn_method1.clone()).unwrap())
    };

    api::enter_device_registration_mode(
        &env,
        canister_id,
        authn_method1.principal(),
        identity_number,
    )?;
    api::add_tentative_device(&env, canister_id, identity_number, &device2)?;

    let identity_info = api_v2::identity_info(
        &env,
        canister_id,
        authn_method1.principal(),
        identity_number,
    )?
    .expect("identity info failed");

    assert_eq!(
        identity_info.authn_method_registration,
        Some(AuthnMethodRegistration {
            expiration: time(&env) + AUTHN_METHOD_REGISTRATION_TIMEOUT,
            authn_method: Some(AuthnMethodData::from(device2))
        })
    );
    Ok(())
}
