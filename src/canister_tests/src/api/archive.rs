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

/// A "compatibility" module for the previous version of the archive to handle API changes.
pub mod compat {
    use super::*;
    use candid::{CandidType, Deserialize};

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

    impl From<archive::Entry> for Entry {
        fn from(value: archive::Entry) -> Self {
            Self {
                anchor: value.anchor,
                operation: Operation::from(value.operation),
                timestamp: value.timestamp,
                caller: value.caller,
                sequence_number: value.sequence_number,
            }
        }
    }

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
        #[serde(rename = "replace_device")]
        ReplaceDevice {
            old_device: PublicKey,
            new_device: DeviceDataWithoutAlias,
        },
        #[serde(rename = "remove_device")]
        RemoveDevice { device: PublicKey },
    }

    impl From<archive::Operation> for Operation {
        fn from(value: archive::Operation) -> Self {
            match value {
                archive::Operation::RegisterAnchor { device } => Operation::RegisterAnchor {
                    device: DeviceDataWithoutAlias::from(device),
                },
                archive::Operation::AddDevice { device } => Operation::AddDevice {
                    device: DeviceDataWithoutAlias::from(device),
                },
                archive::Operation::UpdateDevice { device, new_values } => {
                    Operation::UpdateDevice {
                        device,
                        new_values: DeviceDataUpdate::from(new_values),
                    }
                }
                archive::Operation::ReplaceDevice {
                    old_device,
                    new_device,
                } => Operation::ReplaceDevice {
                    old_device,
                    new_device: DeviceDataWithoutAlias::from(new_device),
                },
                archive::Operation::RemoveDevice { device } => Operation::RemoveDevice { device },
            }
        }
    }

    #[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
    pub struct DeviceDataWithoutAlias {
        pub pubkey: DeviceKey,
        pub credential_id: Option<CredentialId>,
        pub purpose: Purpose,
        pub key_type: KeyType,
        pub protection: DeviceProtection,
    }

    impl From<archive::DeviceDataWithoutAlias> for DeviceDataWithoutAlias {
        fn from(value: archive::DeviceDataWithoutAlias) -> Self {
            Self {
                pubkey: value.pubkey,
                credential_id: value.credential_id,
                purpose: value.purpose,
                key_type: value.key_type,
                protection: value.protection,
            }
        }
    }

    impl From<DeviceData> for DeviceDataWithoutAlias {
        fn from(device_data: DeviceData) -> Self {
            Self {
                pubkey: device_data.pubkey,
                credential_id: device_data.credential_id,
                purpose: device_data.purpose,
                key_type: device_data.key_type,
                protection: device_data.protection,
            }
        }
    }

    // If present, the attribute has been changed to the value given.
    // Does not include the pubkey because it cannot be changed.
    #[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
    pub struct DeviceDataUpdate {
        pub alias: Option<Private>,
        pub credential_id: Option<CredentialId>,
        pub purpose: Option<Purpose>,
        pub key_type: Option<KeyType>,
        pub protection: Option<DeviceProtection>,
    }

    impl From<archive::DeviceDataUpdate> for DeviceDataUpdate {
        fn from(value: archive::DeviceDataUpdate) -> Self {
            Self {
                alias: value.alias,
                credential_id: value.credential_id,
                purpose: value.purpose,
                key_type: value.key_type,
                protection: value.protection,
            }
        }
    }

    #[derive(Clone, Debug, CandidType, Deserialize)]
    pub struct Entries {
        // make this a vec of options to keep Entry extensible
        pub entries: Vec<Option<Entry>>,
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
