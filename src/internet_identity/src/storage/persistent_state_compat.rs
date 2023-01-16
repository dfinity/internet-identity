//! Legacy layout version < 6 persistent state

use crate::storage::persistent_state_compat::ArchiveState::{
    Created, CreationInProgress, NotCreated,
};
use crate::{archive, state};
use candid::{CandidType, Deserialize, Principal};
use internet_identity_interface::ArchiveIntegration::Push;
use internet_identity_interface::{ArchiveConfig, Timestamp};
use std::rc::Rc;
use std::time::Duration;

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct PersistentState {
    // Information related to the archive
    pub archive_info: ArchiveInfo,
    // Amount of cycles that need to be attached when II creates a canister
    pub canister_creation_cycles_cost: u64,
}

impl From<state::PersistentState> for PersistentState {
    fn from(value: state::PersistentState) -> Self {
        let expected_module_hash = match value.archive_state {
            archive::ArchiveState::NotConfigured => None,
            archive::ArchiveState::Configured { ref config, .. }
            | archive::ArchiveState::CreationInProgress { ref config, .. }
            | archive::ArchiveState::Created { ref config, .. } => Some(config.module_hash),
        };
        Self {
            archive_info: ArchiveInfo {
                state: ArchiveState::from(value.archive_state),
                expected_module_hash,
            },
            canister_creation_cycles_cost: value.canister_creation_cycles_cost,
        }
    }
}

impl From<PersistentState> for state::PersistentState {
    fn from(value: PersistentState) -> Self {
        let config = value
            .archive_info
            .expected_module_hash
            .map(|hash| ArchiveConfig {
                module_hash: hash,
                // sensible defaults
                entries_buffer_limit: 10_000,
                polling_interval_ns: Duration::from_secs(60).as_nanos() as u64,
                entries_fetch_limit: 1_000,
                // Push is the legacy mode (i.e. what the old persistent state supported).
                archive_integration: Some(Push),
            });

        let Some(config) = config else {
            return Self { archive_state: archive::ArchiveState::NotConfigured, canister_creation_cycles_cost: value.canister_creation_cycles_cost }
        };

        let state = match value.archive_info.state {
            NotCreated => archive::ArchiveState::Configured { config },
            CreationInProgress(timestamp) => {
                archive::ArchiveState::CreationInProgress { timestamp, config }
            }
            Created(data) => archive::ArchiveState::Created {
                data: archive::ArchiveData::from(data),
                config,
            },
        };
        Self {
            archive_state: state,
            canister_creation_cycles_cost: value.canister_creation_cycles_cost,
        }
    }
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct ArchiveInfo {
    pub expected_module_hash: Option<[u8; 32]>,
    pub state: ArchiveState,
}

#[derive(Eq, PartialEq, Clone, CandidType, Debug, Deserialize)]
pub enum ArchiveState {
    NotCreated,
    CreationInProgress(Timestamp), // timestamp when creation was initiated
    Created(ArchiveData),
}

impl From<archive::ArchiveState> for ArchiveState {
    fn from(value: archive::ArchiveState) -> Self {
        match value {
            archive::ArchiveState::NotConfigured => NotCreated,
            archive::ArchiveState::Configured { .. } => NotCreated,
            archive::ArchiveState::CreationInProgress { timestamp, .. } => {
                CreationInProgress(timestamp)
            }
            archive::ArchiveState::Created { data, .. } => Created(ArchiveData::from(data)),
        }
    }
}

#[derive(Eq, PartialEq, Clone, CandidType, Debug, Deserialize)]
pub struct ArchiveData {
    // Sequence number of anchor operations. Using this sequence number missing entries / reliability
    // can be assessed without having explicit error handling on the II side.
    pub sequence_number: u64,
    // Canister id of the archive canister
    pub archive_canister: Principal,
}

impl From<archive::ArchiveData> for ArchiveData {
    fn from(value: archive::ArchiveData) -> Self {
        ArchiveData {
            archive_canister: value.archive_canister,
            sequence_number: value.sequence_number,
        }
    }
}

impl From<ArchiveData> for archive::ArchiveData {
    fn from(value: ArchiveData) -> Self {
        archive::ArchiveData {
            archive_canister: value.archive_canister,
            sequence_number: value.sequence_number,
            entries_buffer: Rc::new(vec![]),
        }
    }
}
