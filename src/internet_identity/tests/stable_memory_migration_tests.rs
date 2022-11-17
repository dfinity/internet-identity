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

    let stats = api::stats(&env, canister_id)?;
    let archive_info = ArchiveInfo {
        archive_canister: None,
        expected_wasm_hash: None,
    };
    assert_eq!(
        stats,
        InternetIdentityStats {
            assigned_user_number_range: (10000, 3784873),
            users_registered: 10,
            archive_info: archive_info.clone(),
            canister_creation_cycles_cost: 0,
            anchor_migration_state: Some(MigrationState::InProgress {
                anchors_left: 10,
                batch_size: 5
            })
        }
    );

    // register anchor to trigger migration batch
    flows::register_anchor(&env, canister_id);

    let stats = api::stats(&env, canister_id)?;
    assert_eq!(
        stats,
        InternetIdentityStats {
            assigned_user_number_range: (10000, 3784873),
            users_registered: 11,
            archive_info: archive_info.clone(),
            canister_creation_cycles_cost: 0,
            anchor_migration_state: Some(MigrationState::InProgress {
                anchors_left: 5,
                batch_size: 5
            })
        }
    );

    // register anchor to trigger migration batch
    flows::register_anchor(&env, canister_id);

    let stats = api::stats(&env, canister_id)?;
    assert_eq!(
        stats,
        InternetIdentityStats {
            assigned_user_number_range: (10000, 3784873),
            users_registered: 12,
            archive_info,
            canister_creation_cycles_cost: 0,
            anchor_migration_state: Some(MigrationState::Finished)
        }
    );

    Ok(())
}
