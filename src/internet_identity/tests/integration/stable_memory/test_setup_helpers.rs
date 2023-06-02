// Helper functions for generating II stable memory backups for testing purposes.

use super::*;
use std::fs;

pub(crate) fn prepare_persistent_state_v7(anchor_count: usize) {
    let env = env();
    const FIRST_ANCHOR_NUMBER: AnchorNumber = 1000;
    const RANGE_SIZE: u64 = 5000;
    let arg = InternetIdentityInit {
        migrate_storage_to_memory_manager: Some(true),
        ..arg_with_anchor_range((FIRST_ANCHOR_NUMBER, FIRST_ANCHOR_NUMBER + RANGE_SIZE)).unwrap()
    };
    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(arg));
    let stats = api::stats(&env, canister_id).expect("Failed getting stats.");
    assert_eq!(7, stats.storage_layout_version);

    for i in 0..anchor_count {
        let anchor_number =
            flows::register_anchor_with(&env, canister_id, principal(i), &sample_unique_device(i));
        api::add(
            &env,
            canister_id,
            principal(i),
            anchor_number,
            &sample_unique_device(i + 10000),
        )
        .expect("Failure adding a device");
    }

    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    save_compressed_stable_memory(
        &env,
        canister_id,
        "stable_memory/persistent_state_full_bucket_v7.bin.gz",
        "persistent_state_full_bucket_v7.bin",
    )
}

pub(crate) fn sample_unique_device(id: usize) -> DeviceData {
    DeviceData {
        pubkey: ByteBuf::from(format!("public key #{}", id)),
        alias: format!("device #{}", id),
        ..known_devices()[id % 6].clone()
    }
}

pub(crate) fn principal(id: usize) -> Principal {
    Principal::self_authenticating(format!("public key #{}", id))
}

pub(crate) fn migrate_existing_backups(
    arg: &InternetIdentityInit,
    old_version: &str,
    new_version: &str,
) {
    let paths = fs::read_dir("./stable_memory/").unwrap();

    for path in paths {
        let Ok(path) = path else {
            continue;
        };
        let os_file_name = path.file_name();
        let file_name = os_file_name.to_string_lossy();
        if !path.path().is_file()
            || !file_name.ends_with(".bin.gz")
            || !file_name.contains(old_version)
            // skip the backup without persistent state because it traps (intentionally)
            || file_name.contains("no-persistent-state")
        {
            continue;
        }

        println!("Migrating backup: {}", file_name);

        let env = env();
        let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

        restore_compressed_stable_memory(
            &env,
            canister_id,
            &format!("stable_memory/{}", file_name),
        );

        upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), Some(arg.clone()))
            .unwrap();

        // upgrade again to regenerate persistent state
        upgrade_ii_canister(&env, canister_id, II_WASM.clone());

        let file_name = file_name.replace(old_version, new_version);
        save_compressed_stable_memory(
            &env,
            canister_id,
            &format!("stable_memory/{}", file_name),
            file_name.strip_suffix(".gz").unwrap(),
        )
    }
}

pub(crate) fn recreate_no_persistent_state_backup(arg: InternetIdentityInit, version: &str) {
    let env = env();
    let canister_id = install_ii_canister_with_arg(&env, II_WASM.clone(), Some(arg));

    save_compressed_stable_memory(
        &env,
        canister_id,
        &format!("stable_memory/no-persistent-state-{}.bin.gz", version),
        &format!("no-persistent-state-{}.bin", version),
    )
}
