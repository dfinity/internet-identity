//! Tests for making sure that the current version can be upgraded to from the last release.
//! This tests stable memory compatibility and pre / post install hooks.

use std::time::Duration;

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use pocket_ic::RejectResponse;
use regex::Regex;
use serde_bytes::ByteBuf;

use crate::v2_api::authn_method_test_helpers::create_identity_with_authn_method_and_name;

/// Basic upgrade test.
#[test]
fn ii_upgrade_works() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());
    env.upgrade_canister(
        canister_id,
        II_WASM.clone(),
        candid::encode_one(None::<InternetIdentityInit>).unwrap(),
        None,
    )?;
    api::health_check(&env, canister_id);
    Ok(())
}

/// Test to verify that anchors are kept across upgrades.
#[test]
fn ii_upgrade_retains_anchors() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)
        .expect("get_anchor_info failed");
    assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);
}

/// Test to verify that anchor numbers are unchanged by changing the user range.
#[test]
fn should_retain_anchor_on_user_range_change() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());
    let user_number = flows::register_anchor(&env, canister_id);

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((10_000, 11_000)),
    )?;

    let anchor_info = api::get_anchor_info(&env, canister_id, principal_1(), user_number)?;

    assert_eq!(anchor_info.into_device_data(), vec![device_data_1()]);
    Ok(())
}

/// Test to verify that anchors number range can be changed on upgrade.
#[test]
fn should_allow_change_of_user_range_on_upgrade() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((2000, 4000)),
    )?;

    let stats = api::stats(&env, canister_id)?;
    assert_eq!(stats.assigned_user_number_range, (2000, 4000));
    Ok(())
}

/// Test to verify that the user range cannot be changed to modify anchor numbers of existing anchors.
#[test]
fn should_not_affect_existing_anchors_on_user_range_change() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    flows::register_anchor(&env, canister_id);

    let result = upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((2000, 4000)),
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("specified range \\[\\d+, \\d+\\) does not start from the same number \\(\\d+\\) as the existing range thus would make existing anchors invalid")
            .unwrap(),
    );
}

/// Test to verify that the user range cannot be changed to allow fewer anchors than are already registered.
#[test]
fn should_not_allow_fewer_users_than_existing_on_user_range_change() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    flows::register_anchor(&env, canister_id);

    let result = upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((10_000, 10_000)),
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("specified range \\[\\d+, \\d+\\) does not accommodate all \\d+ anchors thus would make existing anchors invalid")
            .unwrap(),
    );
}

/// Test to verify that the user range cannot be changed to exceed the max capacity of the II canister.
#[test]
fn should_not_allow_user_range_exceeding_capacity() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM.clone());

    flows::register_anchor(&env, canister_id);

    let result = upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        arg_with_anchor_range((10_000, 10_000_000_000_000)),
    );

    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("specified range \\[\\d+, \\d+\\) is too large for this canister \\(max \\d+ entries\\)")
            .unwrap(),
    );
}

#[test]
fn test_sync_anchor_indices_migration() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

    const NUM_ANCHORS: usize = 20;

    for i in 0..NUM_ANCHORS {
        let name = Some(format!("Test User {}", i));
        let pubkey = format!("pub-key-{}", i);
        let origin = format!("https://www.app{}.org", i);
        let device_data = DeviceData {
            pubkey: ByteBuf::from(pubkey),
            alias: "My Device".to_string(),
            credential_id: None,
            origin: Some(origin.clone()),
            purpose: Purpose::Recovery,
            key_type: KeyType::SeedPhrase,
            ..DeviceData::auth_test_device()
        };
        let authn_method = AuthnMethodData::from(device_data);

        let _identity_number =
            create_identity_with_authn_method_and_name(&env, canister_id, &authn_method, name);
    }

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    for _ in 0..(NUM_ANCHORS / 2_000 + 1) {
        env.tick();
        env.tick();
        env.advance_time(Duration::from_secs(3));
        env.tick();
    }

    api::health_check(&env, canister_id);

    {
        let payload = candid::encode_one(()).unwrap();

        let data = env
            .update_call(
                canister_id,
                Principal::anonymous(),
                "list_recovery_phrase_migration_errors",
                payload,
            )
            .unwrap();

        let errors: Vec<String> = candid::decode_one(&data).unwrap();

        assert_eq!(errors, Vec::<String>::new());
    }

    {
        let payload = candid::encode_one(()).unwrap();

        let data = env
            .update_call(
                canister_id,
                Principal::anonymous(),
                "list_recovery_phrase_migration_current_batch_id",
                payload,
            )
            .unwrap();

        let list_recovery_phrase_migration_current_batch_id: u64 =
            candid::decode_one(&data).unwrap();

        // The special value u64::MAX indicates that the migration is complete.
        assert_eq!(list_recovery_phrase_migration_current_batch_id, u64::MAX);
    }

    {
        let payload = candid::encode_one(()).unwrap();

        let data = env
            .update_call(
                canister_id,
                Principal::anonymous(),
                "count_recovery_phrases",
                payload,
            )
            .unwrap();

        let count_recovery_phrases: u64 = candid::decode_one(&data).unwrap();

        assert_eq!(count_recovery_phrases, NUM_ANCHORS as u64);
    }

    // smoke test
    for (label, sender, expected_anchor_number) in [
        (
            "Anonymous user should not get any anchor number",
            Principal::anonymous(),
            None,
        ),
        (
            "User with pub-key-0 should get anchor number 10000",
            Principal::self_authenticating("pub-key-0"),
            Some(10000),
        ),
        (
            "User with pub-key-1 should get anchor number 10001",
            Principal::self_authenticating("pub-key-1"),
            Some(10001),
        ),
    ] {
        let payload = candid::encode_one(()).unwrap();

        let data = env
            .update_call(
                canister_id,
                sender,
                "lookup_caller_identity_by_recovery_phrase",
                payload,
            )
            .unwrap();

        let observed_anchor_number: Option<u64> = candid::decode_one(&data).unwrap();

        assert_eq!(
            observed_anchor_number, expected_anchor_number,
            "failed for {}",
            label
        );
    }
}
