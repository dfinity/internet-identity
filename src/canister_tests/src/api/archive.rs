use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::{
    call_candid, call_candid_as, query_candid, CallError, StateMachine,
};
use internet_identity_interface::archive::types::*;
use internet_identity_interface::internet_identity::types::*;

pub fn add_entry(
    env: &StateMachine,
    canister_id: CanisterId,
    sender: Principal,
    anchor: AnchorNumber,
    timestamp: Timestamp,
    entry: Vec<u8>,
) -> Result<(), CallError> {
    call_candid_as(
        env,
        canister_id,
        sender,
        "write_entry",
        (anchor, timestamp, entry),
    )
}

pub fn get_entries(
    env: &StateMachine,
    canister_id: CanisterId,
    idx: Option<u64>,
    limit: Option<u16>,
) -> Result<Entries, CallError> {
    query_candid(env, canister_id, "get_entries", (idx, limit)).map(|(x,)| x)
}

pub fn get_anchor_entries(
    env: &StateMachine,
    canister_id: CanisterId,
    anchor: AnchorNumber,
    cursor: Option<Cursor>,
    limit: Option<u16>,
) -> Result<AnchorEntries, CallError> {
    query_candid(
        env,
        canister_id,
        "get_anchor_entries",
        (anchor, cursor, limit),
    )
    .map(|(x,)| x)
}

pub fn status(env: &StateMachine, canister_id: CanisterId) -> Result<ArchiveStatus, CallError> {
    call_candid(env, canister_id, "status", ()).map(|(x,)| x)
}

/// A "compatibility" module for the previous version of the archive to handle API changes.
pub mod compat {
    use candid::{CandidType, Deserialize, Principal};
    use internet_identity_interface::archive::types::{
        DeviceDataUpdate, DeviceDataWithoutAlias, Entry, Operation,
    };
    use internet_identity_interface::internet_identity::types::{
        AnchorNumber, PublicKey, Timestamp,
    };

    #[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
    pub enum CompatOperation {
        #[serde(rename = "register_anchor")]
        RegisterAnchor { device: DeviceDataWithoutAlias },
        #[serde(rename = "add_device")]
        AddDevice { device: DeviceDataWithoutAlias },
        #[serde(rename = "update_device")]
        UpdateDevice {
            device: PublicKey,
            new_values: DeviceDataUpdate,
        },
        #[serde(rename = "replace_device")]
        ReplaceDevice {
            old_device: PublicKey,
            new_device: DeviceDataWithoutAlias,
        },
        #[serde(rename = "remove_device")]
        RemoveDevice { device: PublicKey },
    }

    impl From<Operation> for CompatOperation {
        fn from(op: Operation) -> Self {
            match op {
                Operation::RegisterAnchor { device } => CompatOperation::RegisterAnchor { device },
                Operation::AddDevice { device } => CompatOperation::AddDevice { device },
                Operation::UpdateDevice { device, new_values } => {
                    CompatOperation::UpdateDevice { device, new_values }
                }
                Operation::ReplaceDevice {
                    old_device,
                    new_device,
                } => CompatOperation::ReplaceDevice {
                    old_device,
                    new_device,
                },
                Operation::RemoveDevice { device } => CompatOperation::RemoveDevice { device },
                Operation::IdentityMetadataWrite { .. } => panic!("not available in compat typ"),
            }
        }
    }

    #[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
    pub struct CompatEntry {
        // store anchor in LogEntry, such that anchor operations can be attributed to an anchor without consulting the index.
        pub anchor: AnchorNumber,
        pub operation: CompatOperation,
        pub timestamp: Timestamp,
        pub caller: Principal,
        // global sequence number to detect lost messages (if any)
        pub sequence_number: u64,
    }

    impl From<Entry> for CompatEntry {
        fn from(entry: Entry) -> Self {
            CompatEntry {
                anchor: entry.anchor,
                caller: entry.caller,
                sequence_number: entry.sequence_number,
                timestamp: entry.timestamp,
                operation: CompatOperation::from(entry.operation),
            }
        }
    }
}
