//! Tests related to local device management (add, remove, update, replace, lookup, get_anchor_info and get_anchor_credentials).
//! Tests for the 'add remote device flow' are in the module [remote_device_registration_tests].

use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_test_state_machine_client::CallError;
use ic_test_state_machine_client::ErrorCode::CanisterCalledTrap;
use internet_identity_interface::internet_identity::types::*;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

/// Tests that lookup is consistent with get anchor info, but without alias.
#[test]
fn should_lookup() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    api::init_salt(&env, canister_id)?;
    let user_number = flows::register_anchor(&env, canister_id);
    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;
    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &recovery_device_data_1(),
    )?;

    let mut devices = api::lookup(&env, canister_id, user_number)?;
    devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));

    let mut anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
    // clear alias to make it consistent with lookup
    anchor_info
        .devices
        .iter_mut()
        .for_each(|device| device.alias = "".to_string());
    anchor_info.devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));

    assert_eq!(devices, anchor_info.into_device_data());
    Ok(())
}

/// Verifies that a new device can be added.
#[test]
fn should_add_additional_device() -> Result<(), CallError> {
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
    let mut devices =
        api::get_anchor_info(&env, canister_id, principal_1(), user_number)?.into_device_data();
    devices.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
    assert_eq!(devices, vec![device_data_2(), device_data_1()]);

    let mut credentials = api::get_anchor_credentials(&env, canister_id, user_number)?.credentials;
    // sort devices to not fail on different orderings
    credentials.sort_by(|a, b| a.pubkey.cmp(&b.pubkey));
    assert_eq!(
        credentials,
        vec![
            WebAuthnCredential::try_from(device_data_2()).unwrap(),
            WebAuthnCredential::try_from(device_data_1()).unwrap()
        ]
    );
    Ok(())
}

/// Verifies that the same device cannot be added twice.
#[test]
fn should_not_add_existing_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let result = api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_1(), // this device was already added during registration
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("Device with key \\w+ already exists on this anchor\\.").unwrap(),
    );
    Ok(())
}

/// Verifies that a second recovery phrase cannot be added.
#[test]
fn should_not_add_second_recovery_phrase() -> Result<(), CallError> {
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
    let result = api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &recovery_device_data_2(),
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("There is already a recovery phrase and only one is allowed\\.").unwrap(),
    );
    Ok(())
}

/// Verifies that the devices cannot be added for other users.
#[test]
fn should_not_add_device_for_different_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
    let user_number_2 =
        flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());

    let result = api::add(
        &env,
        canister_id,
        principal_1(),
        user_number_2,
        &device_data_1(),
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
}

/// Verifies that a new device can be added to anchors that were registered using the previous II release.
#[test]
fn should_add_additional_device_after_ii_upgrade() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
    assert_eq!(anchor_info.devices.len(), 2);
    assert!(anchor_info
        .into_device_data()
        .iter()
        .any(|device| device == &device_data_2()));
    Ok(())
}

/// Verifies that the total size of all devices stays under the variable length fields limit.
#[test]
fn should_respect_total_size_limit() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    for i in 0..3u8 {
        let mut device = large_size_device();
        device.pubkey = ByteBuf::from([i; 300]);
        api::add(&env, canister_id, principal_1(), user_number, &device)?;
    }

    let result = api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &large_size_device(),
    );

    expect_user_error_with_message(
            result,
            CanisterCalledTrap,
            Regex::new("Cumulative size of variable sized fields exceeds limit: length \\d+, limit \\d+\\. Either use shorter aliases or remove an existing device\\.").unwrap(),
        );
    Ok(())
}

/// Verifies that a device can be updated
#[test]
fn should_update_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let principal = principal_1();
    let mut device = device_data_1();

    let user_number = flows::register_anchor_with(&env, canister_id, principal, &device);

    let anchor_info = api::get_anchor_info(&env, canister_id, principal, user_number)?;
    assert_eq!(anchor_info.into_device_data(), vec![device.clone()]);

    device.alias.push_str("some suffix");

    api::update(
        &env,
        canister_id,
        principal,
        user_number,
        &device.pubkey,
        &device,
    )?;

    let anchor_info = api::get_anchor_info(&env, canister_id, principal, user_number)?;
    assert_eq!(anchor_info.into_device_data(), vec![device]);

    Ok(())
}

/// Verifies that a protected device can be updated
#[test]
fn should_update_protected_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let principal = principal_1();
    let mut device = DeviceData {
        protection: DeviceProtection::Protected,
        key_type: KeyType::SeedPhrase,
        credential_id: None,
        ..device_data_1()
    };

    let user_number = flows::register_anchor_with(&env, canister_id, principal, &device);

    let anchor_info = api::get_anchor_info(&env, canister_id, principal, user_number)?;
    assert_eq!(anchor_info.into_device_data(), vec![device.clone()]);

    device.alias.push_str("some suffix");

    api::update(
        &env,
        canister_id,
        principal,
        user_number,
        &device.pubkey,
        &device,
    )?;

    let anchor_info = api::get_anchor_info(&env, canister_id, principal, user_number)?;
    assert_eq!(anchor_info.into_device_data(), vec![device]);

    Ok(())
}

/// Verifies that the device key (i.e. the device ID) cannot be updated
#[test]
fn should_not_modify_pubkey() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let principal = principal_1();
    let mut device = device_data_1();

    let user_number = flows::register_anchor_with(&env, canister_id, principal, &device);

    let original_pubkey = device.pubkey;
    let pubkey_2 = ByteBuf::from(PUBKEY_2);
    assert_ne!(original_pubkey, pubkey_2);
    device.pubkey = pubkey_2;

    let result = api::update(
        &env,
        canister_id,
        principal,
        user_number,
        &original_pubkey,
        &device,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("Device key cannot be updated\\.").unwrap(),
    );
}

/// Verifies that users can only update their own devices.
#[test]
fn should_not_update_device_of_different_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
    let user_number_2 =
        flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());

    let result = api::update(
        &env,
        canister_id,
        principal_1(),
        user_number_2,
        &device_data_2().pubkey,
        &device_data_2(),
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
}

/// Verifies that unprotected devices can only be updated from themselves
#[test]
fn should_not_update_protected_with_different_device() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let device1 = DeviceData {
        protection: DeviceProtection::Protected,
        key_type: KeyType::SeedPhrase,
        credential_id: None,
        ..device_data_1()
    };

    let user_number = flows::register_anchor_with(&env, canister_id, principal_1(), &device1);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )
    .unwrap();

    let result = api::update(
        &env,
        canister_id,
        principal_2(),
        user_number,
        &device1.pubkey,
        &device1, // data here doesnt' actually matter
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("Device is locked. Must be authenticated with this device to mutate").unwrap(),
    );
}

/// Verifies that non-recovery devices cannot be updated to be protected.
#[test]
fn should_not_update_non_recovery_device_to_be_protected() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let mut device1 = device_data_1();
    device1.protection = DeviceProtection::Protected;
    let result = api::update(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device1.pubkey,
        &device1, // data here doesnt' actually matter
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("Only recovery phrases can be locked but key type is Unknown").unwrap(),
    );
}

/// Verifies that get_anchor_credentials returns the expected credentials.
#[test]
fn should_get_credentials() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let recovery_webauthn_device = DeviceData {
        pubkey: ByteBuf::from("recovery device"),
        alias: "Recovery Device".to_string(),
        credential_id: Some(ByteBuf::from("recovery credential id")),
        purpose: Purpose::Recovery,
        key_type: KeyType::CrossPlatform,
        protection: DeviceProtection::Unprotected,
        ..DeviceData::auth_test_device()
    };

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;
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
        &recovery_webauthn_device,
    )?;

    let response = api::get_anchor_credentials(&env, canister_id, user_number)?;

    assert_eq!(response.credentials.len(), 2);
    assert!(response.credentials.contains(&WebAuthnCredential {
        pubkey: device_data_1().pubkey,
        credential_id: device_data_1().credential_id.unwrap()
    }));
    assert!(response.credentials.contains(&WebAuthnCredential {
        pubkey: device_data_2().pubkey,
        credential_id: device_data_2().credential_id.unwrap()
    }));

    assert_eq!(
        response.recovery_credentials,
        vec![WebAuthnCredential {
            pubkey: recovery_webauthn_device.pubkey.clone(),
            credential_id: recovery_webauthn_device.credential_id.unwrap()
        }]
    );

    assert_eq!(
        response.recovery_phrases,
        vec![recovery_device_data_1().pubkey]
    );

    Ok(())
}

/// Verifies that get_anchor_credentials returns an empty list of credentials for invalid anchor numbers.
#[test]
fn should_return_empty_credentials_on_invalid_anchor_number() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    let response = api::get_anchor_credentials(&env, canister_id, 1234564)?;

    assert_eq!(response, AnchorCredentials::default());
    Ok(())
}

/// Verifies that get_anchor_credentials returns the expected credentials (i.e. no recovery credentials if there are none).
#[test]
fn should_not_get_recovery_credentials_if_there_are_none() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let response = api::get_anchor_credentials(&env, canister_id, user_number)?;

    assert_eq!(
        response.credentials,
        vec![WebAuthnCredential {
            pubkey: device_data_1().pubkey,
            credential_id: device_data_1().credential_id.unwrap()
        }]
    );
    assert_eq!(response.recovery_credentials, vec![]);
    assert_eq!(response.recovery_phrases, Vec::<PublicKey>::new());

    Ok(())
}

/// Verifies that a device can be removed.
#[test]
fn should_remove_device() -> Result<(), CallError> {
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
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
    assert_eq!(anchor_info.devices.len(), 2);
    assert!(anchor_info
        .into_device_data()
        .iter()
        .any(|device| device == &device_data_2()));

    api::remove(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2().pubkey,
    )?;

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
    assert_eq!(anchor_info.devices.len(), 1);
    assert!(!anchor_info
        .into_device_data()
        .iter()
        .any(|device| device == &device_data_2()));
    Ok(())
}

/// Verifies that a protected device can be removed.
#[test]
fn should_remove_protected_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let mut device2 = device_data_2();
    device2.protection = DeviceProtection::Protected;
    device2.key_type = KeyType::SeedPhrase;

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;
    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
    assert!(anchor_info
        .into_device_data()
        .iter()
        .any(|device| device == &device_data_2()));

    api::remove(
        &env,
        canister_id,
        principal_2(),
        user_number,
        &device_data_2().pubkey,
    )?;

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
    assert_eq!(anchor_info.devices.len(), 1);
    assert!(!anchor_info
        .into_device_data()
        .iter()
        .any(|device| device == &device_data_2()));
    Ok(())
}

/// Verifies that the even last device can be removed.
/// This behaviour should be changed because it makes anchors unusable, see L2-745.
#[test]
fn should_remove_last_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::remove(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_1().pubkey,
    )?;

    let anchor_credentials = api::get_anchor_credentials(&env, canister_id, user_number)?;
    assert!(anchor_credentials.credentials.is_empty());
    Ok(())
}

/// Verifies that users can only remove their own devices.
#[test]
fn should_not_remove_device_of_different_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
    let user_number_2 =
        flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());

    let result = api::remove(
        &env,
        canister_id,
        principal_1(),
        user_number_2,
        &device_data_2().pubkey,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
}

/// Verifies that unprotected devices can not be removed with another device
#[test]
fn should_not_remove_protected_with_different_device() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let device1 = DeviceData {
        protection: DeviceProtection::Protected,
        key_type: KeyType::SeedPhrase,
        credential_id: None,
        ..device_data_1()
    };

    let user_number = flows::register_anchor_with(&env, canister_id, principal_1(), &device1);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )
    .unwrap();

    let result = api::remove(
        &env,
        canister_id,
        principal_2(),
        user_number,
        &device1.pubkey,
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("Device is locked. Must be authenticated with this device to mutate").unwrap(),
    );
}

/// Verifies that a device can be removed if it has been added using the previous II release.
#[test]
fn should_remove_device_after_ii_upgrade() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2(),
    )?;
    let devices = api::compat::get_anchor_info(&env, canister_id, principal_1(), user_number)?
        .into_device_data();
    assert!(devices.iter().any(|device| device == &device_data_2()));

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    api::remove(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_2().pubkey,
    )?;

    let devices =
        api::get_anchor_info(&env, canister_id, principal_1(), user_number)?.into_device_data();
    assert_eq!(devices.len(), 1);
    assert!(!devices.iter().any(|device| device == &device_data_2()));
    Ok(())
}

/// Verifies that get_anchor_info requires authentication.
#[test]
fn should_not_allow_get_anchor_info_for_different_user() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let result = api::get_anchor_info(&env, canister_id, principal_2(), user_number);

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("[a-z\\d-]+ could not be authenticated.").unwrap(),
    );
}

/// Verifies that a device can be replaced with another device.
#[test]
fn should_replace_device() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;
    assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);

    api::replace(
        &env,
        canister_id,
        principal_1(),
        user_number,
        &device_data_1().pubkey,
        &device_data_2(),
    )?;

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_2(), user_number)?;
    assert_eq!(anchor_info.into_device_data(), vec![device_data_2()]);
    Ok(())
}

/// Verifies that metadata is stored.
#[test]
fn should_keep_metadata() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let device = DeviceData {
        metadata: Some(HashMap::from([(
            "key".to_string(),
            MetadataEntry::String("value".to_string()),
        )])),
        ..DeviceData::auth_test_device()
    };

    let user_number = flows::register_anchor_with_device(&env, canister_id, &device);

    let devices = api::get_anchor_info(&env, canister_id, device.principal(), user_number)?
        .into_device_data();
    assert_eq!(devices, vec![device]);
    Ok(())
}

/// Verifies that reserved metadata keys are rejected.
#[test]
fn should_not_allow_reserved_metadata_keys() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    let device = DeviceData {
        metadata: Some(HashMap::from([(
            "alias".to_string(),
            MetadataEntry::String("value".to_string()),
        )])),
        ..DeviceData::auth_test_device()
    };

    let result = api::add(&env, canister_id, principal_1(), user_number, &device);

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("Metadata key 'alias' is reserved and cannot be used\\.").unwrap(),
    );
    Ok(())
}
