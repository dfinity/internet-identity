use canister_tests::api::internet_identity as api;
use canister_tests::flows;
use canister_tests::framework::*;
use ic_state_machine_tests::StateMachine;
use internet_identity_interface::*;

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
            MigrationState::InProgress {
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
            MigrationState::InProgress {
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
            MigrationState::InProgress {
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
            MigrationState::InProgress {
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
            MigrationState::InProgress {
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
            MigrationState::Paused {
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
            MigrationState::Paused {
                anchors_left: 10,
                batch_size: 0
            }
        )
    );

    Ok(())
}

fn stats_with_migration_state(
    num_users: u64,
    migration_state: MigrationState,
) -> InternetIdentityStats {
    InternetIdentityStats {
        assigned_user_number_range: (10000, 3784873),
        users_registered: num_users,
        archive_info: ArchiveInfo {
            archive_canister: None,
            expected_wasm_hash: None,
        },
        canister_creation_cycles_cost: 0,
        anchor_migration_state: Some(migration_state),
    }
}
