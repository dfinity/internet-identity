use crate::state;
use crate::state::{ArchiveData, ArchiveState, ArchiveStatusCache, DeviceDataInternal};
use candid::Principal;
use ic_cdk::api::call::{call_with_payment, CallResult};
use ic_cdk::api::management_canister::main::{
    canister_status, install_code, CanisterIdRecord, CanisterInstallMode,
    CanisterInstallMode::Install, CanisterStatusResponse, CreateCanisterArgument,
    InstallCodeArgument,
};
use ic_cdk::api::time;
use ic_cdk::{id, notify};
use internet_identity_interface::{
    ArchiveInit, DeployArchiveResult, DeviceDataUpdate, Entry, Operation, Private, UserNumber,
};
use serde_bytes::ByteBuf;
use sha2::Digest;
use sha2::Sha256;
use std::time::Duration;
use ArchiveState::{Created, CreationInProgress, NotCreated};
use CanisterInstallMode::Upgrade;
use DeployArchiveResult::{Failed, Success};

pub async fn deploy_archive(wasm: ByteBuf) -> DeployArchiveResult {
    let expected_hash = if let Some(hash) = state::expected_archive_hash() {
        hash
    } else {
        return Failed("archive deployment disabled".to_string());
    };

    let wasm = wasm.into_vec();

    let (archive_canister, install_mode, wasm_hash_verified) = match state::archive_state() {
        NotCreated => match create_archive(&wasm).await {
            Ok(archive) => (archive, Install, true),
            Err(result) => return result,
        },
        CreationInProgress(timestamp) => {
            if time() - timestamp > Duration::from_secs(24 * 60 * 60).as_nanos() as u64 {
                // The archive has been in creation for more than a day and the creation process
                // has likely failed thus another attempt should be made.
                match create_archive(&wasm).await {
                    Ok(archive) => (archive, Install, true),
                    Err(result) => return result,
                }
            } else {
                return DeployArchiveResult::CreationInProgress;
            }
        }
        Created(archive_data) => {
            let status = match archive_status(archive_data.archive_canister).await {
                Ok(status) => status,
                Err(message) => return Failed(message),
            };
            match status.module_hash {
                None => (archive_data.archive_canister, Install, false),
                Some(hash) if hash == expected_hash.to_vec() => {
                    // we already have an archive with the expected module and don't need to do anything
                    return Success(archive_data.archive_canister);
                }
                Some(_) => (archive_data.archive_canister, Upgrade, false),
            }
        }
    };

    // creating the archive verifies the wasm hash as well
    if !wasm_hash_verified {
        let hash_check = verify_wasm_hash(&wasm);
        if hash_check.is_err() {
            return Failed(hash_check.err().unwrap());
        }
    }

    match install_archive(archive_canister, wasm, install_mode).await {
        Ok(()) => Success(archive_canister),
        Err(message) => Failed(message),
    }
}

async fn create_archive(wasm: &Vec<u8>) -> Result<Principal, DeployArchiveResult> {
    let hash_check = verify_wasm_hash(wasm);
    if hash_check.is_err() {
        return Err(Failed(hash_check.err().unwrap()));
    }

    // lock the archive
    state::persistent_state_mut(|persistent_state| {
        persistent_state.archive_info.state = CreationInProgress(time());
    });

    let result = create_canister(CreateCanisterArgument { settings: None }).await;

    match result {
        Ok((CanisterIdRecord { canister_id },)) => {
            state::persistent_state_mut(|persistent_state| {
                // save archive info permanently
                persistent_state.archive_info.state = Created(ArchiveData {
                    sequence_number: 0,
                    archive_canister: canister_id,
                })
            });
            Ok(canister_id)
        }
        Err((reject_code, message)) => {
            // unlock archive creation again
            state::persistent_state_mut(|persistent_state| {
                persistent_state.archive_info.state = NotCreated
            });
            Err(Failed(format!(
                "failed to create archive! error code: {:?}, message: {}",
                reject_code, message
            )))
        }
    }
}

/// Register a new canister and get its canister id.
///
/// See [IC method `create_canister`](https://internetcomputer.org/docs/current/references/ic-interface-spec/#ic-create_canister).
///
/// Note: Copied from [ic-cdk::create_canister] but modified to allow a configurable amount of cycles
/// to be sent (canister creation is free on system subnets where II is running). We still make the
/// cycles cost configurable so that II can be run and deploy an archive on an application subnets
/// as well.
async fn create_canister(arg: CreateCanisterArgument) -> CallResult<(CanisterIdRecord,)> {
    let cycles_cost =
        state::persistent_state(|persistent_state| persistent_state.canister_creation_cycles_cost);
    call_with_payment(
        Principal::management_canister(),
        "create_canister",
        (arg,),
        cycles_cost,
    )
    .await
}

async fn install_archive(
    archive_canister: Principal,
    wasm_module: Vec<u8>,
    install_mode: CanisterInstallMode,
) -> Result<(), String> {
    let settings = ArchiveInit {
        ii_canister: id(),
        max_entries_per_call: 1000,
    };
    let encoded_arg = candid::encode_one(settings)
        .map_err(|err| format!("failed to encode archive install argument: {:?}", err))?;

    install_code(InstallCodeArgument {
        mode: install_mode,
        canister_id: archive_canister,
        wasm_module,
        arg: encoded_arg,
    })
    .await
    .map_err(|(code, message)| {
        format!(
            "failed to install archive canister! error code: {:?}, message: {}",
            code, message
        )
    })
}

async fn archive_status(archive_canister: Principal) -> Result<CanisterStatusResponse, String> {
    let status_opt = state::cached_archive_status();
    match status_opt {
        None => {
            let (archive_status,) = canister_status(CanisterIdRecord {
                canister_id: archive_canister,
            })
            .await
            .map_err(|err| format!("failed to retrieve archive status: {:?}", err))?;

            state::cache_archive_status(ArchiveStatusCache {
                timestamp: time(),
                status: archive_status.clone(),
            });
            Ok(archive_status)
        }
        Some(status) => Ok(status),
    }
}

fn verify_wasm_hash(wasm_module: &Vec<u8>) -> Result<[u8; 32], String> {
    let expected_hash = state::expected_archive_hash().expect("bug: no wasm hash to check against");

    let mut hasher = Sha256::new();
    hasher.update(&wasm_module);
    let wasm_hash: [u8; 32] = hasher.finalize().into();

    if wasm_hash != expected_hash {
        return Err("invalid wasm module".to_string());
    }
    Ok(expected_hash)
}

pub fn archive_operation(anchor: UserNumber, caller: Principal, operation: Operation) {
    let archive_data = state::archive_data(); // traps if archive is not available
    let timestamp = time();
    let entry = Entry {
        anchor,
        operation,
        timestamp,
        caller,
        sequence_number: archive_data.sequence_number,
    };
    let encoded_entry = candid::encode_one(entry).expect("failed to encode archive entry");

    // Notify can still trap if the message cannot be enqueued rolling back the anchor operation.
    // Therefore we only increment the sequence number after notifying successfully.
    let () = notify(
        archive_data.archive_canister,
        "write_entry",
        (anchor, timestamp, encoded_entry),
    )
    .expect("failed to send archive entry notification");
    state::increment_archive_seq_nr();
}

pub fn device_diff(old: &DeviceDataInternal, new: &DeviceDataInternal) -> DeviceDataUpdate {
    DeviceDataUpdate {
        alias: if old.alias == new.alias {
            None
        } else {
            Some(Private::Redacted)
        },
        credential_id: if old.credential_id == new.credential_id {
            None
        } else {
            new.credential_id.clone()
        },
        purpose: if old.purpose == new.purpose {
            None
        } else {
            new.purpose.clone()
        },
        key_type: if old.key_type == new.key_type {
            None
        } else {
            new.key_type.clone()
        },
        protection: if old.protection == new.protection {
            None
        } else {
            new.protection.clone()
        },
    }
}
