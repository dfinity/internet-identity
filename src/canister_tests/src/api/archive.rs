use candid::Principal;
use ic_cdk::api::management_canister::main::CanisterId;
use internet_identity_interface::archive::types::*;
use internet_identity_interface::internet_identity::types::*;
use pocket_ic::common::rest::RawEffectivePrincipal;
use pocket_ic::{call_candid, call_candid_as, query_candid, CallError, PocketIc};

pub fn add_entry(
    env: &PocketIc,
    canister_id: CanisterId,
    sender: Principal,
    anchor: AnchorNumber,
    timestamp: Timestamp,
    entry: Vec<u8>,
) -> Result<(), CallError> {
    call_candid_as(
        env,
        canister_id,
        RawEffectivePrincipal::None,
        sender,
        "write_entry",
        (anchor, timestamp, entry),
    )
}

pub fn get_entries(
    env: &PocketIc,
    canister_id: CanisterId,
    idx: Option<u64>,
    limit: Option<u16>,
) -> Result<Entries, CallError> {
    query_candid(env, canister_id, "get_entries", (idx, limit)).map(|(x,)| x)
}

pub fn get_anchor_entries(
    env: &PocketIc,
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

pub fn status(env: &PocketIc, canister_id: CanisterId) -> Result<ArchiveStatus, CallError> {
    call_candid(env, canister_id, RawEffectivePrincipal::None, "status", ()).map(|(x,)| x)
}

/// A "compatibility" module for the previous version of the archive to handle API changes.
pub mod compat {
    use candid::{CandidType, Deserialize, Principal};
    use internet_identity_interface::archive::types::{
        DeviceDataUpdate, DeviceDataWithoutAlias, Entry, Operation, Private,
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
        #[serde(rename = "add_openid_credential")]
        AddOpenIdCredential { iss: String },
        #[serde(rename = "remove_openid_credential")]
        RemoveOpenIdCredential { iss: String },
        #[serde(rename = "register_with_openid_credential")]
        RegisterWithOpenIdCredential { iss: String },
        #[serde(rename = "add_name")]
        AddName,
        #[serde(rename = "update_name")]
        UpdateName,
        #[serde(rename = "remove_name")]
        RemoveName,
        #[serde(rename = "create_account")]
        CreateAccount { name: Private },
        #[serde(rename = "update_account")]
        UpdateAccount { name: Option<Private> },
        #[serde(rename = "delete_account")]
        DeleteAccount,
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
                Operation::AddOpenIdCredential { iss } => {
                    CompatOperation::AddOpenIdCredential { iss }
                }
                Operation::RemoveOpenIdCredential { iss } => {
                    CompatOperation::RemoveOpenIdCredential { iss }
                }
                Operation::RegisterAnchorWithOpenIdCredential { iss } => {
                    CompatOperation::RegisterWithOpenIdCredential { iss }
                }
                Operation::AddName => CompatOperation::AddName,
                Operation::UpdateName => CompatOperation::UpdateName,
                Operation::RemoveName => CompatOperation::RemoveName,
                Operation::IdentityMetadataReplace { .. } => {
                    panic!("not available in compat type")
                }
                Operation::CreateAccount { name } => CompatOperation::CreateAccount { name },
                Operation::UpdateAccount { name } => CompatOperation::UpdateAccount { name },
                Operation::DeleteAccount => CompatOperation::DeleteAccount,
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
