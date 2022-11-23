use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_state_machine_tests::StateMachine;
use internet_identity_interface::*;
use serde_bytes::ByteBuf;

#[test]
fn should_migrate_anchors() -> Result<(), CallError> {
    let env = StateMachine::new();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

    for _ in 0..10 {
        flows::register_anchor(&env, canister_id);
    }

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_module_hash: None,
            canister_creation_cycles_cost: None,
            memory_migration_batch_size: Some(5),
        }),
    )
    .unwrap();

    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(
            10,
            MigrationState::Started {
                anchors_left: 10,
                batch_size: 5
            }
        )
    );

    // register anchor to trigger migration batch
    flows::register_anchor(&env, canister_id);

    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(
            11,
            MigrationState::Started {
                anchors_left: 5,
                batch_size: 5
            }
        )
    );

    // register anchor to trigger migration batch
    flows::register_anchor(&env, canister_id);

    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(12, MigrationState::Finished)
    );

    Ok(())
}

#[test]
fn should_keep_anchors_intact_when_migrating() -> Result<(), CallError> {
    let env = StateMachine::new();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

    let anchor0 = flows::register_anchor(&env, canister_id);
    let device_anchor0 = DeviceData {
        pubkey: ByteBuf::from("custom device key of anchor 0"),
        alias: "device of the very first anchor".to_string(),
        credential_id: None,
        purpose: Purpose::Authentication,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
    };
    api::add(
        &env,
        canister_id,
        principal_1(),
        anchor0,
        device_anchor0.clone(),
    )?;
    let anchor0_devices = vec![device_data_1(), device_anchor0.clone()];
    assert_devices_equal(&env, canister_id, anchor0, anchor0_devices.clone());

    // Generate dummy anchors to guarantee that the anchors we care about in this test span multiple
    // wasm pages
    for _ in 0..13 {
        flows::register_anchor(&env, canister_id);
    }
    let default_anchor = flows::register_anchor(&env, canister_id);

    let anchor1 = flows::register_anchor_with(&env, canister_id, principal_2(), &device_data_2());
    let device_anchor1 = DeviceData {
        pubkey: ByteBuf::from("custom device key"),
        alias: "originally old layout anchor device".to_string(),
        credential_id: None,
        purpose: Purpose::Authentication,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
    };
    api::add(
        &env,
        canister_id,
        principal_2(),
        anchor1,
        device_anchor1.clone(),
    )?;
    let anchor1_devices = vec![device_data_2(), device_anchor1];
    assert_devices_equal(&env, canister_id, anchor1, anchor1_devices.clone());

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_module_hash: None,
            canister_creation_cycles_cost: None,
            memory_migration_batch_size: Some(1),
        }),
    )
    .unwrap();

    // migration activated, but no anchor shifted yet
    assert_devices_equal(&env, canister_id, anchor1, anchor1_devices.clone());
    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(
            16,
            MigrationState::Started {
                anchors_left: 16,
                batch_size: 1
            }
        )
    );

    let anchor2 = flows::register_anchor(&env, canister_id);
    let device_anchor2 = DeviceData {
        pubkey: ByteBuf::from("custom device key 2"),
        alias: "originally new layout anchor device".to_string(),
        credential_id: None,
        purpose: Purpose::Recovery,
        key_type: KeyType::SeedPhrase,
        protection: DeviceProtection::Protected,
    };
    api::add(
        &env,
        canister_id,
        principal_1(),
        anchor2,
        device_anchor2.clone(),
    )?;

    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(
            17,
            MigrationState::Started {
                anchors_left: 14,
                batch_size: 1
            }
        )
    );

    // anchor1 has now been shifted by registering anchor2 and adding a device
    assert_devices_equal(&env, canister_id, anchor1, anchor1_devices.clone());

    // anchor0 has not yet been shifted
    assert_devices_equal(&env, canister_id, anchor0, anchor0_devices.clone());

    // anchor2 is in the new layout from the beginning
    let anchor2_devices = vec![device_data_1(), device_anchor2];
    assert_devices_equal(&env, canister_id, anchor2, anchor2_devices.clone());

    // upgrade again to increase migration speed
    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_module_hash: None,
            canister_creation_cycles_cost: None,
            memory_migration_batch_size: Some(100),
        }),
    )
    .unwrap();

    let device_anchor0_2 = DeviceData {
        pubkey: ByteBuf::from("custom device key of anchor 0, 2"),
        alias: "device added to the very first anchor during migration".to_string(),
        credential_id: None,
        purpose: Purpose::Authentication,
        key_type: KeyType::Unknown,
        protection: DeviceProtection::Unprotected,
    };
    api::add(
        &env,
        canister_id,
        principal_1(),
        anchor0,
        device_anchor0_2.clone(),
    )?;

    // adding the device has completed the migration
    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(17, MigrationState::Finished)
    );

    // check all anchors again
    assert_devices_equal(&env, canister_id, default_anchor, vec![device_data_1()]);
    assert_devices_equal(
        &env,
        canister_id,
        anchor0,
        vec![device_data_1(), device_anchor0, device_anchor0_2],
    );
    assert_devices_equal(&env, canister_id, anchor1, anchor1_devices);
    assert_devices_equal(&env, canister_id, anchor2, anchor2_devices);

    Ok(())
}

#[test]
fn should_upgrade_during_migration() -> Result<(), CallError> {
    let env = StateMachine::new();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

    for _ in 0..10 {
        flows::register_anchor(&env, canister_id);
    }

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_module_hash: None,
            canister_creation_cycles_cost: None,
            memory_migration_batch_size: Some(5),
        }),
    )
    .unwrap();

    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(
            10,
            MigrationState::Started {
                anchors_left: 10,
                batch_size: 5
            }
        )
    );

    // register anchor to trigger migration batch
    flows::register_anchor(&env, canister_id);

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_module_hash: None,
            canister_creation_cycles_cost: None,
            memory_migration_batch_size: Some(5),
        }),
    )
    .unwrap();

    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(
            11,
            MigrationState::Started {
                anchors_left: 5,
                batch_size: 5
            }
        )
    );
    Ok(())
}

#[test]
fn should_start_and_pause_migration() -> Result<(), CallError> {
    let env = StateMachine::new();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

    for _ in 0..10 {
        flows::register_anchor(&env, canister_id);
    }

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_module_hash: None,
            canister_creation_cycles_cost: None,
            memory_migration_batch_size: Some(5),
        }),
    )
    .unwrap();

    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(
            10,
            MigrationState::Started {
                anchors_left: 10,
                batch_size: 5
            }
        )
    );

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_module_hash: None,
            canister_creation_cycles_cost: None,
            memory_migration_batch_size: Some(0),
        }),
    )
    .unwrap();

    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(
            10,
            MigrationState::Started {
                anchors_left: 10,
                batch_size: 0
            }
        )
    );

    flows::register_anchor(&env, canister_id);

    // make sure it did not make further migrations
    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(
            11,
            MigrationState::Started {
                anchors_left: 10,
                batch_size: 0
            }
        )
    );

    Ok(())
}

#[test]
fn should_not_migrate_anchors_if_not_configured() -> Result<(), CallError> {
    let env = StateMachine::new();
    let canister_id = install_ii_canister(&env, II_WASM_PREVIOUS.clone());

    for _ in 0..10 {
        flows::register_anchor(&env, canister_id);
    }

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    flows::register_anchor(&env, canister_id);

    assert_eq!(
        api::stats(&env, canister_id)?,
        stats_with_migration_state(11, MigrationState::NotStarted)
    );
    Ok(())
}

fn stats_with_migration_state(
    num_users: u64,
    migration_state: MigrationState,
) -> InternetIdentityStats {
    let storage_layout_version = match migration_state {
        MigrationState::NotStarted => 1,
        MigrationState::Started { .. } => 2,
        MigrationState::Finished => 3,
    };

    InternetIdentityStats {
        assigned_user_number_range: (10000, 3784873),
        users_registered: num_users,
        archive_info: ArchiveInfo {
            archive_canister: None,
            expected_wasm_hash: None,
        },
        canister_creation_cycles_cost: 0,
        storage_layout_version,
        memory_migration_state: Some(migration_state),
    }
}
