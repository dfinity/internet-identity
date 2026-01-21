//! Tests related to stable memory.
//! These tests make sure that II can be recovered from a stable memory backup.

use candid::Principal;
use canister_tests::api::internet_identity as api;
use canister_tests::framework::*;
use pocket_ic::common::rest::BlobCompression::NoCompression;
use pocket_ic::ErrorCode::CanisterCalledTrap;
use pocket_ic::{ErrorCode, RejectResponse};
use regex::Regex;
use serde_bytes::ByteBuf;
use std::path::PathBuf;

/// Verifies that an anchor with two recovery phrases can still use both.
/// This anchor is recovered from stable memory because the current version of II does not allow to create such anchors.
#[test]
fn should_not_break_on_multiple_legacy_recovery_phrases() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());
    let frontend_hostname = "frontend_hostname";
    let session_key = ByteBuf::from("session_key");

    restore_compressed_stable_memory(
        &env,
        canister_id,
        "stable_memory/multiple-recovery-phrases-v9.bin.gz",
    );
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    api::prepare_delegation(
        &env,
        canister_id,
        principal_recovery_1(),
        10_000,
        frontend_hostname,
        &session_key,
        None,
    )?;
    api::prepare_delegation(
        &env,
        canister_id,
        principal_recovery_2(),
        10_000,
        frontend_hostname,
        &session_key,
        None,
    )?;
    Ok(())
}

/// Verifies that an existing account with two recovery phrases can only make changes after deleting one.
/// This anchor is recovered from stable memory because the current version of II does not allow to create such anchors.
#[test]
fn should_allow_modification_after_deleting_second_recovery_phrase() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

    restore_compressed_stable_memory(
        &env,
        canister_id,
        "stable_memory/multiple-recovery-phrases-v9.bin.gz",
    );
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let mut recovery_1 = recovery_device_data_1();
    recovery_1.alias = "new alias".to_string();
    let result = api::update(
        &env,
        canister_id,
        principal_1(),
        10_000,
        &recovery_1.pubkey,
        &recovery_1,
    );
    expect_user_error_with_message(
        result,
        CanisterCalledTrap,
        Regex::new("There is already a recovery phrase and only one is allowed\\.").unwrap(),
    );

    api::remove(
        &env,
        canister_id,
        principal_1(),
        10_000,
        &recovery_device_data_2().pubkey,
    )?;

    // successful after removing the other one
    api::update(
        &env,
        canister_id,
        principal_1(),
        10_000,
        &recovery_1.pubkey,
        &recovery_1,
    )?;
    Ok(())
}

/// Verifies that a stable memory backup with persistent state containing archive information is restored correctly.
#[test]
fn should_read_persistent_state_with_archive() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

    restore_compressed_stable_memory(
        &env,
        canister_id,
        "stable_memory/persistent_state_archive_v9.bin.gz",
    );
    upgrade_ii_canister(&env, canister_id, II_WASM.clone());

    let devices =
        api::get_anchor_info(&env, canister_id, principal_1(), 10_000)?.into_device_data();
    assert_eq!(devices.len(), 1);

    let stats = api::stats(&env, canister_id)?;
    assert_eq!(
        stats.archive_info.archive_canister.unwrap(),
        Principal::from_text("rrkah-fqaaa-aaaaa-aaaaq-cai").unwrap()
    );

    assert_eq!(
        stats
            .archive_info
            .archive_config
            .unwrap()
            .module_hash
            .to_vec(),
        hex::decode("12e2c2bd05dfcd86e3004ecd5f00533e6120e7bcf82bac0753af0a7fe14bfea1").unwrap()
    );
    // auto-migration to v9
    assert_eq!(stats.storage_layout_version, 9);
    Ok(())
}

/// Tests that II will refuse to install on a stable memory layout that is no longer supported.
#[test]
fn should_trap_on_old_stable_memory() -> Result<(), RejectResponse> {
    let env = env();
    let canister_id = install_ii_canister(&env, EMPTY_WASM.clone());

    let stable_memory_backup =
        std::fs::read(PathBuf::from("stable_memory/genesis-memory-layout.bin")).unwrap();
    env.set_stable_memory(canister_id, stable_memory_backup, NoCompression);
    let result = upgrade_ii_canister_with_arg(&env, canister_id, II_WASM.clone(), None);
    assert!(result.is_err());
    let err = result.err().unwrap();
    assert_eq!(err.error_code, ErrorCode::CanisterCalledTrap);
    assert!(err
        .reject_message
        .contains("stable memory layout version 1 is no longer supported"));
    Ok(())
}
