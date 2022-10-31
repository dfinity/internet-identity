use crate::state;
use crate::state::{ArchiveData, ArchiveState, DeviceDataInternal};
use candid::Principal;
use ic_cdk::api::call::{call_with_payment, CallResult};
use ic_cdk::api::management_canister::main::{
    canister_status, install_code, CanisterIdRecord, CanisterInstallMode,
    CanisterInstallMode::Install, CreateCanisterArgument, InstallCodeArgument,
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
    if state::expected_archive_hash().is_none() {
        return Failed("archive deployment disabled".to_string());
    }
    let wasm = wasm.into_vec();
    let hash_check = verify_wasm_hash(&wasm);
    if hash_check.is_err() {
        return Failed(hash_check.err().unwrap());
    }

    let archive_state = state::persistent_state_mut(|persistent_state| {
        let archive_state = persistent_state.archive_info.state.clone();
        match archive_state {
            NotCreated => {
                // lock archive creation because of async operation
                persistent_state.archive_info.state = CreationInProgress(time());
                archive_state
            }
            CreationInProgress(timestamp) => {
                let now = time();
                let lock_age = Duration::from_nanos(now - timestamp);
                if lock_age > Duration::from_secs(24 * 60 * 60) {
                    // The archive has been in creation for more than 1 day. The creation process
                    // has likely failed and another attempt should be made --> update the lock with
                    // current timestamp and proceed as if the state had been NotCreated.
                    persistent_state.archive_info.state = CreationInProgress(now);
                    return NotCreated;
                }
                archive_state
            }
            Created(_) => archive_state,
        }
    });

    match archive_state {
        NotCreated => {
            let creation_result = create_archive().await;
            match creation_result {
                Err(e) => {
                    state::persistent_state_mut(|persistent_state| {
                        // unlock archive creation again
                        persistent_state.archive_info.state = NotCreated
                    });
                    return Failed(format!("failed to create archive: {:?}", e));
                }
                Ok(data) => {
                    let archive_canister_id = data.archive_canister;
                    state::persistent_state_mut(|persistent_state| {
                        // save archive info permanently
                        persistent_state.archive_info.state = Created(data)
                    });
                    match install_archive(archive_canister_id, wasm).await {
                        Ok(()) => Success(archive_canister_id),
                        Err(err) => Failed(err),
                    }
                }
            }
        }
        Created(data) => match install_archive(data.archive_canister, wasm).await {
            Ok(()) => Success(data.archive_canister),
            Err(err) => Failed(err),
        },
        CreationInProgress(_) => DeployArchiveResult::CreationInProgress,
    }
}

async fn create_archive() -> CallResult<ArchiveData> {
    let (result,) = create_canister(CreateCanisterArgument { settings: None }).await?;
    Ok(ArchiveData {
        sequence_number: 0,
        archive_canister: result.canister_id,
    })
}

/// Register a new canister and get its canister id.
///
/// See [IC method `create_canister`](https://internetcomputer.org/docs/current/references/ic-interface-spec/#ic-create_canister).
///
/// Note: Copied from [ic-cdk::create_canister] but modified to allow a configurable amount of cycles
/// to be sent (canister creation is free on system subnets where II is running). We still make the
/// cycles cost configurable so that II can be run and deploy an archive on an application subnets
/// as well.
pub async fn create_canister(arg: CreateCanisterArgument) -> CallResult<(CanisterIdRecord,)> {
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

async fn install_archive(archive_canister: Principal, wasm_module: Vec<u8>) -> Result<(), String> {
    let (archive_status,) = canister_status(CanisterIdRecord {
        canister_id: archive_canister,
    })
    .await
    .map_err(|err| format!("failed to retrieve archive status: {:?}", err))?;

    let module_hash = archive_status.module_hash;
    let expected_hash = state::expected_archive_hash().expect("bug: now wasm hash available");
    let mode = match module_hash {
        None => Install,
        Some(hash) if hash == expected_hash => return Ok(()),
        Some(_) => Upgrade,
    };

    let settings = ArchiveInit {
        ii_canister: id(),
        max_entries_per_call: 1000,
    };
    let encoded_arg = candid::encode_one(settings)
        .map_err(|err| format!("failed to encode archive install argument: {:?}", err))?;

    install_code(InstallCodeArgument {
        mode,
        canister_id: archive_canister,
        wasm_module,
        arg: encoded_arg,
    })
    .await
    .map_err(|err| format!("failed to install archive canister: {:?}", err))
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
