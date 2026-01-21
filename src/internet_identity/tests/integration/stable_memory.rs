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
