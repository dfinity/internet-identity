use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use ic_test_state_machine_client::{
    call_candid, call_candid_as, query_candid, CallError, StateMachine,
};
use internet_identity_interface::archive::*;
use internet_identity_interface::*;

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

/// A "compatibility" module for the previous version of II to handle API changes.
pub mod compat {
    use super::*;
    use crate::framework::{
        principal_1, ANCHOR_NUMBER_1, ANCHOR_NUMBER_2, PUBKEY_1, TIMESTAMP_1, TIMESTAMP_2,
    };
    use candid::{CandidType, Deserialize};
    use serde_bytes::ByteBuf;

    #[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
    pub enum Operation {
        #[serde(rename = "register_anchor")]
        RegisterAnchor { device: DeviceDataWithoutAlias },
        #[serde(rename = "add_device")]
        AddDevice { device: DeviceDataWithoutAlias },
        #[serde(rename = "update_device")]
        UpdateDevice {
            device: PublicKey,
            new_values: DeviceDataUpdate,
        },
        #[serde(rename = "remove_device")]
        RemoveDevice { device: PublicKey },
    }

    #[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
    pub struct Entry {
        // store anchor in LogEntry, such that anchor operations can be attributed to an anchor without consulting the index.
        pub anchor: AnchorNumber,
        pub operation: Operation,
        pub timestamp: Timestamp,
        pub caller: Principal,
        // global sequence number to detect lost messages (if any)
        pub sequence_number: u64,
    }

    #[derive(Clone, Debug, CandidType, Deserialize)]
    pub struct Entries {
        // make this a vec of options to keep Entry extensible
        pub entries: Vec<Option<Entry>>,
    }

    pub fn log_entry_1() -> Entry {
        Entry {
            timestamp: TIMESTAMP_1,
            anchor: ANCHOR_NUMBER_1,
            caller: principal_1(),
            operation: Operation::RegisterAnchor {
                device: DeviceDataWithoutAlias {
                    pubkey: ByteBuf::from(PUBKEY_1),
                    credential_id: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::Unknown,
                    protection: DeviceProtection::Unprotected,
                },
            },
            sequence_number: 0,
        }
    }

    pub fn log_entry_2() -> Entry {
        Entry {
            timestamp: TIMESTAMP_2,
            anchor: ANCHOR_NUMBER_2,
            caller: principal_1(),
            operation: Operation::AddDevice {
                device: DeviceDataWithoutAlias {
                    pubkey: ByteBuf::from(PUBKEY_1),
                    credential_id: None,
                    purpose: Purpose::Authentication,
                    key_type: KeyType::Unknown,
                    protection: DeviceProtection::Unprotected,
                },
            },
            sequence_number: 1,
        }
    }

    pub fn get_entries(
        env: &StateMachine,
        canister_id: CanisterId,
        idx: Option<u64>,
        limit: Option<u16>,
    ) -> Result<Entries, CallError> {
        query_candid(env, canister_id, "get_entries", (idx, limit)).map(|(x,)| x)
    }
}
