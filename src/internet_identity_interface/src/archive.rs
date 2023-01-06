use crate::{
    AnchorNumber, CredentialId, DeviceData, DeviceKey, DeviceProtection, KeyType, PublicKey,
    Purpose, Timestamp,
};
use candid::{CandidType, Deserialize, Principal};
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

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub struct DeviceDataWithoutAlias {
    pub pubkey: DeviceKey,
    pub credential_id: Option<CredentialId>,
    pub purpose: Purpose,
    pub key_type: KeyType,
    pub protection: DeviceProtection,
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

// Placeholder for information that has been hidden for privacy reasons.
#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum Private {
    #[serde(rename = "redacted")]
    Redacted,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct Entries {
    // make this a vec of options to keep Entry extensible
    pub entries: Vec<Option<Entry>>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct AnchorEntries {
    // make this a vec of options to keep Entry extensible
    pub entries: Vec<Option<Entry>>,
    // cursor pointing to the next entry not included in this response, if any
    pub cursor: Option<Cursor>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum Cursor {
    // timestamp of the next entry not included in this response, if any
    #[serde(rename = "timestamp")]
    Timestamp { timestamp: Timestamp },
    // index of the next entry not included in this response, if any
    #[serde(rename = "next_token")]
    NextToken { next_token: ByteBuf },
}

/// Init arguments of the archive canister.
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct ArchiveInit {
    pub ii_canister: Principal,
    pub max_entries_per_call: u16,
}

/// Encoded entry as buffered on the II side (until acknowledged by the archive).
#[derive(Eq, PartialEq, Clone, CandidType, Debug, Deserialize)]
pub struct BufferedEntry {
    pub anchor_number: AnchorNumber,
    pub timestamp: u64,
    pub entry: ByteBuf,
    pub sequence_number: u64,
}
