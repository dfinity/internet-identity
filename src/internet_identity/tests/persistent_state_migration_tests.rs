use canister_tests::api::internet_identity as api;
use canister_tests::framework::{
    env, install_ii_canister, upgrade_ii_canister, upgrade_ii_canister_with_arg, II_WASM,
    II_WASM_V5_LAYOUT,
};
use ic_test_state_machine_client::CallError;
use internet_identity_interface::InternetIdentityInit;

#[test]
fn should_upgrade_persistent_state() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_V5_LAYOUT.clone());
    let stats = api::compat::stats(&env, canister_id)?;
    assert_eq!(stats.storage_layout_version, 5);

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_config: None,
            canister_creation_cycles_cost: None,
            upgrade_persistent_state: Some(true),
        }),
    )?;

    let new_stats = api::stats(&env, canister_id)?;
    assert_eq!(new_stats.storage_layout_version, 6);
    Ok(())
}

#[test]
fn should_not_migrate_if_flag_not_set() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_V5_LAYOUT.clone());
    let stats = api::compat::stats(&env, canister_id)?;
    assert_eq!(stats.storage_layout_version, 5);

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let new_stats = api::stats(&env, canister_id)?;
    assert_eq!(new_stats.storage_layout_version, 5);

    // rollback to v5 release
    upgrade_ii_canister(&env, canister_id, II_WASM_V5_LAYOUT.clone());
    let stats = api::compat::stats(&env, canister_id)?;
    assert_eq!(stats.storage_layout_version, 5);
    Ok(())
}

#[test]
fn should_not_migrate_if_flag_set_to_false() -> Result<(), CallError> {
    let env = env();
    let canister_id = install_ii_canister(&env, II_WASM_V5_LAYOUT.clone());
    let stats = api::compat::stats(&env, canister_id)?;
    assert_eq!(stats.storage_layout_version, 5);

    upgrade_ii_canister_with_arg(
        &env,
        canister_id,
        II_WASM.clone(),
        Some(InternetIdentityInit {
            assigned_user_number_range: None,
            archive_config: None,
            canister_creation_cycles_cost: None,
            upgrade_persistent_state: Some(false),
        }),
    )?;
    let new_stats = api::stats(&env, canister_id)?;
    assert_eq!(new_stats.storage_layout_version, 5);
    Ok(())
}
