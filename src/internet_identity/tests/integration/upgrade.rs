//! Tests for making sure that the current version can be upgraded to from the last release.
//! This tests stable memory compatibility and pre / post install hooks.

use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use pocket_ic::RejectResponse;
use regex::Regex;
use serde_bytes::ByteBuf;
use std::time::Duration;

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

/// Test that upgrading from the previous release to the current build correctly
/// repopulates the passkey pubkey index via the timer-based migration.
///
/// This is an end-to-end upgrade test for the key type change from `[u8; 32]`
/// (SHA-256 hash) to `Principal` in `lookup_anchor_with_passkey_pubkey_hash_memory`.
#[test]
fn should_repopulate_passkey_index_after_upgrade() {
    let env = env();

    // 1. Install the previous (production) version
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

    // 2. Register anchors with passkey devices (credential_id is set → passkey)
    let user_number_1 = flows::register_anchor(&env, canister_id);
    let user_number_2 = flows::register_anchor_with_device(&env, canister_id, &device_data_2());

    // Verify anchors were created with passkey devices (credential_id is set).
    // Note: the previous WASM doesn't expose `count_passkeys`, so we verify the
    // precondition through the device data — both devices have credential_id.
    let anchor_info_1 =
        api::get_anchor_info(&env, canister_id, principal_1(), user_number_1).unwrap();
    assert_eq!(anchor_info_1.into_device_data(), vec![device_data_1()]);
    assert!(
        device_data_1().credential_id.is_some(),
        "device_data_1 must be a passkey (has credential_id)"
    );
    let anchor_info_2 =
        api::get_anchor_info(&env, canister_id, principal_2(), user_number_2).unwrap();
    assert_eq!(anchor_info_2.into_device_data(), vec![device_data_2()]);
    assert!(
        device_data_2().credential_id.is_some(),
        "device_data_2 must be a passkey (has credential_id)"
    );

    // 3. Upgrade to the current version (triggers migration timer in post_upgrade)
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    // 4. Let the migration timer run to completion.
    //    The timer fires every 1 second processing 2000 anchors per batch.
    //    With only 2 anchors, a single tick should suffice, but we loop for safety.
    for _ in 0..10 {
        env.advance_time(Duration::from_secs(2));
        env.tick();
    }

    // 5. Verify the migration completed (batch_id == u64::MAX)
    let batch_id = api::list_anchor_migration_current_batch_id(&env, canister_id).unwrap();
    assert_eq!(
        batch_id,
        u64::MAX,
        "migration should have completed (batch_id should be u64::MAX)"
    );

    // 6. Verify the passkey index was repopulated with the correct count
    let passkey_count = api::count_passkeys(&env, canister_id).unwrap();
    assert_eq!(
        passkey_count, 2,
        "both passkeys should be indexed after upgrade migration"
    );

    // 7. Add a new passkey device to anchor 1 and verify the index grows to 3
    let new_device = DeviceData {
        pubkey: ByteBuf::from("a]new passkey pubkey"),
        alias: "New passkey".to_string(),
        credential_id: Some(ByteBuf::from("credential id 3")),
        origin: Some("https://identity.ic0.app".to_string()),
        ..DeviceData::auth_test_device()
    };
    api::add(
        &env,
        canister_id,
        principal_1(),
        user_number_1,
        &new_device,
    )
    .unwrap();

    let passkey_count_after_add = api::count_passkeys(&env, canister_id).unwrap();
    assert_eq!(
        passkey_count_after_add, 3,
        "passkey index should grow to 3 after adding a new device"
    );
}
