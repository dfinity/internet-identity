use crate::state;
use crate::state::{ArchiveData, ArchiveState, DeviceDataInternal};
use candid::Principal;
use ic_cdk::api::call::CallResult;
use ic_cdk::api::management_canister::main::{
    canister_status, create_canister, install_code, CanisterIdRecord, CanisterInstallMode,
    CanisterInstallMode::Install, CreateCanisterArgument, InstallCodeArgument,
};
use ic_cdk::api::time;
use ic_cdk::{id, notify};
use internet_identity_interface::{
    ArchiveInit, DeployArchiveResult,
    DeployArchiveResult::{CreationFailed, UpgradeFailed},
    DeviceDataUpdate, Entry, Operation, Private, UserNumber,
};
use serde_bytes::ByteBuf;
use sha2::Digest;
use sha2::Sha256;
use CanisterInstallMode::Upgrade;

pub async fn deploy_archive(wasm: ByteBuf) -> DeployArchiveResult {
    let archive_state = state::persistent_state_mut(|persistent_state| {
        let archive_state = persistent_state.archive_info.state.clone();
        if archive_state == ArchiveState::NotCreated {
            // lock archive creation because of async operation
            persistent_state.archive_info.state = ArchiveState::CreationInProgress;
        }
        archive_state
    });

    match archive_state {
        ArchiveState::NotCreated => create_and_install_archive(wasm.into_vec()).await,
        ArchiveState::CreationInProgress => DeployArchiveResult::CreationInProgress,
        ArchiveState::Created(data) => {
            match install_archive(data.archive_canister, wasm.into_vec()).await {
                Ok(()) => DeployArchiveResult::Success(data.archive_canister),
                Err(err) => UpgradeFailed(err),
            }
        }
    }
}

async fn create_and_install_archive(wasm: Vec<u8>) -> DeployArchiveResult {
    match create_archive().await {
        Err(e) => {
            state::persistent_state_mut(|persistent_state| {
                // unlock archive creation again
                persistent_state.archive_info.state = ArchiveState::NotCreated
            });
            return CreationFailed(format!("failed to create archive: {:?}", e));
        }
        Ok(data) => {
            let archive_canister_id = data.archive_canister;
            state::persistent_state_mut(|persistent_state| {
                // safe archive info permanently
                persistent_state.archive_info.state = ArchiveState::Created(data)
            });
            match install_archive(archive_canister_id, wasm).await {
                Ok(()) => DeployArchiveResult::Success(archive_canister_id),
                Err(err) => UpgradeFailed(err),
            }
        }
    }
}

async fn create_archive() -> CallResult<ArchiveData> {
    let (result,) = create_canister(CreateCanisterArgument { settings: None }).await?;
    Ok(ArchiveData {
        sequence_number: 0,
        archive_canister: result.canister_id,
    })
}

async fn install_archive(archive_canister: Principal, wasm_module: Vec<u8>) -> Result<(), String> {
    let expected_hash = verify_wasm_hash(&wasm_module)?;

    let (archive_status,) = canister_status(CanisterIdRecord {
        canister_id: archive_canister,
    })
    .await
    .map_err(|err| format!("failed to retrieve archive status: {:?}", err))?;

    let module_hash = archive_status.module_hash;
    if module_hash
        .clone()
        .map(|hash| hash == expected_hash.to_vec())
        .unwrap_or(false)
    {
        // Don't do anything further if the archive canister has the given module already installed
        return Ok(());
    }

    let mode = match module_hash {
        None => Install,
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
