/// Integration tests for the II <-> archive integration using the legacy push integration.
/// This file will be deleted, once support for push integration has been removed.
use canister_tests::api::archive as archive_api;
use canister_tests::api::internet_identity as ii_api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_test_state_machine_client::CallError;
use internet_identity_interface::archive::*;
use internet_identity_interface::*;
use serde_bytes::ByteBuf;
use std::time::SystemTime;

/// Test to verify that II can spawn an archive canister.
#[test]
fn should_deploy_archive() -> Result<(), CallError> {
    let env = env();
    let ii_canister = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_wasm_hash(ARCHIVE_WASM.clone(), None),
    );

    let result = ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
    assert!(matches!(result, DeployArchiveResult::Success(_)));
    Ok(())
}

/// Test to verify that II can spawn an archive canister when cycles are required.
#[test]
fn should_deploy_archive_with_cycles() -> Result<(), CallError> {
    let env = env();
    let ii_canister = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_config: Some(ArchiveConfig {
                module_hash: archive_wasm_hash(&ARCHIVE_WASM),
                entries_buffer_limit: 0,
                polling_interval_ns: 0,
                entries_fetch_limit: 0,
                archive_integration: None,
            }),
            canister_creation_cycles_cost: Some(100_000_000_000), // current cost in application subnets
        }),
    );
    env.add_cycles(ii_canister, 150_000_000_000);

    let result = ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
    assert!(matches!(result, DeployArchiveResult::Success(_)));
    assert_eq!(env.cycle_balance(ii_canister), 50_000_000_000);
    Ok(())
}

/// Test to verify that II will not deploy wasm modules that have the wrong hash.
#[test]
fn should_not_deploy_wrong_wasm() -> Result<(), CallError> {
    let env = env();
    let ii_canister = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_wasm_hash(ARCHIVE_WASM.clone(), None),
    );

    let result = ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(EMPTY_WASM.clone()))?;
    match result {
        DeployArchiveResult::Failed(msg) => {
            assert_eq!(msg, "invalid wasm module".to_string())
        }
        unexpected => panic!("unexpected result: {unexpected:?}"),
    }

    let stats = ii_api::stats(&env, ii_canister)?;
    assert!(stats.archive_info.archive_canister.is_none());
    Ok(())
}

/// Test to verify that II will not spawn the archive canister if no hash is configured.
#[test]
fn should_not_deploy_archive_when_disabled() -> Result<(), CallError> {
    let env = env();
    let ii_canister = install_ii_canister(&env, II_WASM.clone());

    let result = ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
    match result {
        DeployArchiveResult::Failed(msg) => {
            assert_eq!(msg, "archive deployment disabled".to_string())
        }
        unexpected => panic!("unexpected result: {unexpected:?}"),
    }

    let stats = ii_api::stats(&env, ii_canister)?;
    assert!(stats.archive_info.archive_canister.is_none());
    Ok(())
}

/// Test to verify that II will not lose information about the archive wasm hash on upgrade.
#[test]
fn should_keep_archive_module_hash_across_upgrades() -> Result<(), CallError> {
    let env = env();
    let ii_canister = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_wasm_hash(ARCHIVE_WASM.clone(), None),
    );
    upgrade_ii_canister(&env, ii_canister, II_WASM.clone());

    let result = ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
    assert!(matches!(result, DeployArchiveResult::Success(_)));
    Ok(())
}

/// Test to verify that the archive WASM hash can be changed on II upgrade and a the archive can be upgraded.
#[test]
fn should_upgrade_the_archive() -> Result<(), CallError> {
    let env = env();
    let ii_canister = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_wasm_hash(EMPTY_WASM.clone(), None),
    );

    let result = ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(EMPTY_WASM.clone()))?;
    assert!(matches!(result, DeployArchiveResult::Success(_)));

    upgrade_ii_canister_with_arg(
        &env,
        ii_canister,
        II_WASM.clone(),
        arg_with_wasm_hash(ARCHIVE_WASM.clone(), None),
    )
    .unwrap();

    let result = ii_api::deploy_archive(&env, ii_canister, ByteBuf::from(ARCHIVE_WASM.clone()))?;
    let DeployArchiveResult::Success(archive_canister) = result else {
        panic!("Unexpected result")
    };

    // interact with the archive to make sure it is no longer the empty wasm
    let entries = archive_api::get_entries(&env, archive_canister, None, None)?;
    assert_eq!(entries.entries.len(), 0);
    Ok(())
}

/// Test to verify that II will not lose information about already create archives on upgrade.
#[test]
fn should_keep_archive_across_upgrades() -> Result<(), CallError> {
    let env = env();
    let ii_canister = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_wasm_hash(ARCHIVE_WASM.clone(), None),
    );

    let archive_canister = deploy_archive_via_ii(&env, ii_canister);
    assert!(env.canister_exists(archive_canister));

    upgrade_ii_canister(&env, ii_canister, II_WASM.clone());

    let anchor = flows::register_anchor(&env, ii_canister);

    let entries = archive_api::get_anchor_entries(&env, archive_canister, anchor, None, None)?;
    assert_eq!(entries.entries.len(), 1);
    Ok(())
}

/// Test to verify that II records the anchor operations after spawning an archive.
#[test]
fn should_record_anchor_operations() -> Result<(), CallError> {
    let env = env();
    let ii_canister = install_ii_canister_with_arg(
        &env,
        II_WASM.clone(),
        arg_with_wasm_hash(ARCHIVE_WASM.clone(), None),
    );

    let archive_canister = deploy_archive_via_ii(&env, ii_canister);
    assert!(env.canister_exists(archive_canister));

    let anchor = flows::register_anchor(&env, ii_canister);

    let mut device = device_data_2();
    ii_api::add(&env, ii_canister, principal_1(), anchor, device.clone())?;

    device.purpose = Purpose::Recovery;
    let pubkey = device.pubkey.clone();
    ii_api::update(
        &env,
        ii_canister,
        principal_1(),
        anchor,
        pubkey.clone(),
        device,
    )?;

    ii_api::remove(&env, ii_canister, principal_1(), anchor, pubkey.clone())?;

    let entries = archive_api::get_entries(&env, archive_canister, None, None)?;

    assert_eq!(entries.entries.len(), 4);

    let timestamp = env
        .time()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_nanos() as u64;

    let register_entry = Entry {
        anchor,
        operation: Operation::RegisterAnchor {
            device: DeviceDataWithoutAlias {
                pubkey: device_data_1().pubkey,
                credential_id: None,
                purpose: Purpose::Authentication,
                key_type: KeyType::Unknown,
                protection: DeviceProtection::Unprotected,
            },
        },
        timestamp,
        caller: principal_1(),
        sequence_number: 0,
    };
    assert_eq!(
        entries.entries.get(0).unwrap().as_ref().unwrap(),
        &register_entry
    );

    let add_entry = Entry {
        anchor,
        operation: Operation::AddDevice {
            device: DeviceDataWithoutAlias::from(device_data_2()),
        },
        timestamp,
        caller: principal_1(),
        sequence_number: 1,
    };
    assert_eq!(
        entries.entries.get(1).unwrap().as_ref().unwrap(),
        &add_entry
    );

    let update_entry = Entry {
        anchor,
        operation: Operation::UpdateDevice {
            device: device_data_2().pubkey,
            new_values: DeviceDataUpdate {
                alias: None,
                credential_id: None,
                purpose: Some(Purpose::Recovery),
                key_type: None,
                protection: None,
            },
        },
        timestamp,
        caller: principal_1(),
        sequence_number: 2,
    };
    assert_eq!(
        entries.entries.get(2).unwrap().as_ref().unwrap(),
        &update_entry
    );

    let delete_entry = Entry {
        anchor,
        operation: Operation::RemoveDevice { device: pubkey },
        timestamp,
        caller: principal_1(),
        sequence_number: 3,
    };
    assert_eq!(
        entries.entries.get(3).unwrap().as_ref().unwrap(),
        &delete_entry
    );
    Ok(())
}
