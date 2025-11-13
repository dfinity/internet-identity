//! Tests for making sure that the current version can be upgraded to from the last release.
//! This tests stable memory compatibility and pre / post install hooks.

use std::time::Duration;

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::api::internet_identity::api_v2::create_account;
use canister_tests::api::internet_identity::api_v2::get_accounts;
use canister_tests::flows;
use canister_tests::framework::*;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use pocket_ic::RejectResponse;
use regex::Regex;
use serde_bytes::ByteBuf;

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

/// Verifies that an anchor that was created with the new version of II can still be used when
/// II is rolled back to the previous version.
#[test]
fn should_keep_new_anchor_across_rollback() -> Result<(), RejectResponse> {
    let frontend_hostname = "frontend.com";
    let env = env();

    // start with the previous release to initialize v1 layout
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
}

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

// TODO: Remove this test after the migration takes place.
#[test]
fn upgrade_and_rollback_with_realistic_data_migration() {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

    for i in 0..20 {
        let pubkey = format!("pub-key-{}", i);
        let sender = Principal::self_authenticating(pubkey.clone());
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
        let identity_number = flows::register_anchor_with(&env, canister_id, sender, &device_data);

        let account_name = format!("Account-{}", i);

        create_account(
            &env,
            canister_id,
            sender,
            identity_number,
            origin,
            account_name,
        )
        .unwrap()
        .unwrap();
    }

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    for _ in 0..200 {
        env.tick();
        env.tick();
        env.advance_time(Duration::from_secs(3));
        env.tick();
    }

    api::health_check(&env, canister_id);

    {
        let payload = candid::encode_one(()).unwrap();

        let data = env
            .query_call(
                canister_id,
                Principal::anonymous(),
                "list_recovery_phrases",
                payload,
            )
            .unwrap();

        let recovery_phrases: Vec<(Principal, u64)> = candid::decode_one(&data).unwrap();

        println!("recovery_phrases: {:#?}", recovery_phrases);
    }

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

        println!("errors: {:#?}", errors);
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

        println!(
            "list_recovery_phrase_migration_current_batch_id: {}",
            list_recovery_phrase_migration_current_batch_id
        );
    }

    panic!("the end")
}
