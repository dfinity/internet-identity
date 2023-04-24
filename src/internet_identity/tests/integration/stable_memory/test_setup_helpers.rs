// Helper functions for generating persistent II states for testing purposes.

use super::*;

fn prepare_persistent_state_v7() {
    let env = env();
    let canister_id =
        install_ii_canister_with_arg(&env, II_WASM.clone(), arg_with_anchor_range((127, 129)));
    let anchor_number =
        flows::register_anchor_with(&env, canister_id, principal_1(), &device_data_1());
    for d in &known_devices() {
        api::add(&env, canister_id, principal_1(), anchor_number, d)
            .expect("Failure adding a device");
    }
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());
    save_compressed_stable_memory(
        &env,
        canister_id,
        "stable_memory/persistent_state_no_archive_v7.bin.gz",
        "persistent_state_no_archive_v7.bin",
    )
}
