use crate::state;
use crate::storage::anchor::Device;
use candid::{CandidType, Deserialize, Principal};
use ic_cdk::api::call::{call_with_payment, CallResult};
use ic_cdk::api::management_canister::main::{
    canister_status, install_code, CanisterIdRecord, CanisterInstallMode,
    CanisterInstallMode::Install, CanisterStatusResponse, CreateCanisterArgument,
    InstallCodeArgument,
};
use ic_cdk::api::time;
use ic_cdk::{id, notify};
use internet_identity_interface::archive::*;
use internet_identity_interface::*;
use serde_bytes::ByteBuf;
use sha2::Digest;
use sha2::Sha256;
use std::rc::Rc;
use std::time::Duration;
use ArchiveState::{Configured, Created, CreationInProgress, NotConfigured};
use CanisterInstallMode::Upgrade;

/// State of the archive canister.
#[derive(Eq, PartialEq, Clone, CandidType, Debug, Deserialize)]
pub enum ArchiveState {
    /// Not configured and not created.
    NotConfigured,
    /// Configured but not created.
    Configured {
        /// Archive related configuration
        config: ArchiveConfig,
    },
    /// Creation in progress implies an existing configuration.
    CreationInProgress {
        /// timestamp when creation was initiated
        timestamp: Timestamp,
        /// Archive related configuration
        config: ArchiveConfig,
    },
    /// Created implies an existing configuration.
    Created {
        /// Archive related data.
        data: ArchiveData,
        /// Archive related configuration
        config: ArchiveConfig,
    },
}

impl Default for ArchiveState {
    fn default() -> Self {
        NotConfigured
    }
}

/// Management metadata about the archive.
#[derive(Eq, PartialEq, Clone, CandidType, Debug, Deserialize)]
pub struct ArchiveData {
    // Sequence number of anchor operations. Using this sequence number missing entries / reliability
    // can be assessed without having explicit error handling on the II side.
    pub sequence_number: u64,
    // Canister id of the archive canister
    pub archive_canister: Principal,
    // Entries to be fetched by the archive canister sorted in ascending order by sequence_number.
    // Once the limit has been reached, II will refuse further changes to anchors in stable memory
    // until the archive acknowledges entries and they can safely be deleted from this buffer.
    // The limit is configurable (entries_buffer_limit).
    // This is an Rc to avoid unnecessary copies of (potentially) a lot of data when cloning.
    // Currently unused: in preparation of switching the archive integration to pull.
    // Anything stored here will be discarded for storage_layout_version < 6.
    pub entries_buffer: Rc<Vec<BufferedEntry>>,
}

/// Cached archive status information
#[derive(Eq, PartialEq, Clone, CandidType, Debug, Deserialize)]
pub struct ArchiveStatusCache {
    // Timestamp when the status was last obtained
    pub timestamp: Timestamp,
    // Status of the archive canister
    pub status: CanisterStatusResponse,
}

struct VerifiedWasm(Vec<u8>);

pub async fn deploy_archive(wasm: ByteBuf) -> DeployArchiveResult {
    // Archive state without not_configured and creation_in_progress because we can exit early in
    // those cases.
    enum ReducedArchiveState {
        Configured,
        Created(ArchiveData),
    }

    unlock_archive_if_stuck();

    // exit early if archive is not configured or is being created by another call to deploy_archive
    // also unpacks the config from the other two states
    let (reduced_state, config) = match state::archive_state() {
        NotConfigured => {
            return DeployArchiveResult::Failed("archive deployment disabled".to_string())
        }
        CreationInProgress { .. } => return DeployArchiveResult::CreationInProgress,
        Configured { config } => (ReducedArchiveState::Configured, config),
        Created { config, data } => (ReducedArchiveState::Created(data), config),
    };

    // exit early if the expected wasm module is already installed
    if let ReducedArchiveState::Created(ref data) = reduced_state {
        let status = archive_status(data.archive_canister).await;
        match status.module_hash {
            Some(hash) if hash == config.module_hash.to_vec() => {
                // we already have an archive with the expected module and don't need to do anything
                return DeployArchiveResult::Success(data.archive_canister);
            }
            None | Some(_) => {}
        }
    }

    // all early checks passed, we need to make changes to the archive --> verify wasm
    let verified_wasm = match verify_wasm(wasm.into_vec(), &config.module_hash) {
        Ok(verified_wasm) => verified_wasm,
        Err(err) => return DeployArchiveResult::Failed(err),
    };

    // create if not exists and determine install mode
    let (archive_canister, install_mode) = match reduced_state {
        ReducedArchiveState::Configured => {
            let archive = match create_archive(config.clone()).await {
                Ok(archive) => archive,
                Err(err) => return DeployArchiveResult::Failed(err),
            };
            (archive, Install)
        }
        ReducedArchiveState::Created(data) => {
            let status = archive_status(data.archive_canister).await;
            match status.module_hash {
                None => (data.archive_canister, Install),
                Some(_) => (data.archive_canister, Upgrade),
            }
        }
    };

    match install_archive(archive_canister, verified_wasm, install_mode).await {
        Ok(()) => DeployArchiveResult::Success(archive_canister),
        Err(err) => DeployArchiveResult::Failed(err),
    }
}

fn unlock_archive_if_stuck() {
    match state::archive_state() {
        NotConfigured | Configured { .. } | Created { .. } => {}
        CreationInProgress { timestamp, config } => {
            // The archive has been in creation for more than a day and the creation process
            // has likely failed thus another attempt should be made.
            if time() - timestamp > Duration::from_secs(24 * 60 * 60).as_nanos() as u64 {
                state::persistent_state_mut(|persistent_state| {
                    persistent_state.archive_state = Configured { config }
                })
            }
        }
    }
}

async fn create_archive(config: ArchiveConfig) -> Result<Principal, String> {
    // lock the archive
    state::persistent_state_mut(|persistent_state| {
        persistent_state.archive_state = CreationInProgress {
            timestamp: time(),
            config: config.clone(),
        };
    });

    let result = create_canister(CreateCanisterArgument { settings: None }).await;

    match result {
        Ok((CanisterIdRecord { canister_id },)) => {
            state::persistent_state_mut(|persistent_state| {
                // save archive info permanently
                persistent_state.archive_state = Created {
                    data: ArchiveData {
                        sequence_number: 0,
                        archive_canister: canister_id,
                        entries_buffer: Rc::new(vec![]),
                    },
                    config,
                }
            });
            Ok(canister_id)
        }
        Err((reject_code, message)) => {
            // unlock archive creation again
            state::persistent_state_mut(|persistent_state| {
                persistent_state.archive_state = Configured { config }
            });
            Err(format!(
                "failed to create archive! error code: {:?}, message: {}",
                reject_code, message
            ))
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
    wasm: VerifiedWasm,
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
        wasm_module: wasm.0,
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

fn verify_wasm(wasm: Vec<u8>, expected_hash: &[u8; 32]) -> Result<VerifiedWasm, String> {
    let mut hasher = Sha256::new();
    hasher.update(&wasm);
    let actual_hash: [u8; 32] = hasher.finalize().into();

    if &actual_hash != expected_hash {
        return Err("invalid wasm module".to_string());
    }

    Ok(VerifiedWasm(wasm))
}

async fn archive_status(archive_canister: Principal) -> CanisterStatusResponse {
    let status_opt = state::cached_archive_status();
    match status_opt {
        None => {
            let (archive_status,) = canister_status(CanisterIdRecord {
                canister_id: archive_canister,
            })
            .await
            .expect("failed to retrieve archive status");

            state::cache_archive_status(ArchiveStatusCache {
                timestamp: time(),
                status: archive_status.clone(),
            });
            archive_status
        }
        Some(status) => status,
    }
}

pub fn archive_operation(anchor_number: AnchorNumber, caller: Principal, operation: Operation) {
    let Created {data, ..} = state::archive_state() else {
        // nothing to archive if the archive has not been deployed yet
        return
    };

    let timestamp = time();
    let entry = Entry {
        anchor: anchor_number,
        operation,
        timestamp,
        caller,
        sequence_number: data.sequence_number,
    };
    let encoded_entry = candid::encode_one(entry).expect("failed to encode archive entry");

    // Notify can still trap if the message cannot be enqueued rolling back the anchor operation.
    // Therefore we only increment the sequence number after notifying successfully.
    let () = notify(
        data.archive_canister,
        "write_entry",
        (anchor_number, timestamp, encoded_entry),
    )
    .expect("failed to send archive entry notification");
    state::archive_data_mut(|data| {
        data.sequence_number += 1;
    })
}

pub fn device_diff(old: &Device, new: &Device) -> DeviceDataUpdate {
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
            Some(new.purpose.clone())
        },
        key_type: if old.key_type == new.key_type {
            None
        } else {
            Some(new.key_type.clone())
        },
        protection: if old.protection == new.protection {
            None
        } else {
            Some(new.protection.clone())
        },
    }
}
