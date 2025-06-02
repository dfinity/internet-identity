use crate::internet_identity::types::{
    AnchorNumber, CredentialId, DeviceKey, DeviceProtection, KeyType, PublicKey, Purpose, Timestamp,
};
use candid::{CandidType, Deserialize, Nat, Principal};
use ic_cdk::api::management_canister::main::{CanisterStatusType, QueryStats};
use serde::Serialize;
use serde_bytes::ByteBuf;

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum Operation {
    // V1 API
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

    // V2 API
    // See the II candid interface for more details.
    #[serde(rename = "identity_metadata_replace")]
    IdentityMetadataReplace { metadata_keys: Vec<String> },

    // OpenID credentials, only the issuer is within the operation due to privacy considerations
    #[serde(rename = "add_openid_credential")]
    AddOpenIdCredential { iss: String },
    #[serde(rename = "remove_openid_credential")]
    RemoveOpenIdCredential { iss: String },
    #[serde(rename = "register_anchor_with_openid_credential")]
    RegisterAnchorWithOpenIdCredential { iss: String },

    // Identity name, set for new users in new discoverable passkeys flow
    #[serde(rename = "add_name")]
    AddName,
    #[serde(rename = "update_name")]
    UpdateName,
    #[serde(rename = "remove_name")]
    RemoveName,

    // Accounts creating and updating
    #[serde(rename = "create_account")]
    CreateAccount { name: Private },
    #[serde(rename = "update_account")]
    UpdateAccount { name: Option<Private> },
    #[serde(rename = "delete_account")]
    DeleteAccount,
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
    pub origin: Option<String>,
    // Only the top level keys are archived for privacy reasons.
    pub metadata_keys: Option<Vec<String>>,
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
    pub origin: Option<Option<String>>,
    // If present, the metadata has been changed and now contains the given keys.
    // Only the top level keys are archived for privacy reasons.
    pub metadata_keys: Option<Vec<String>>,
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
#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub struct ArchiveInit {
    pub ii_canister: Principal,
    pub max_entries_per_call: u16,
    pub polling_interval_ns: u64,
    pub error_buffer_limit: u16,
}

/// Encoded entry as buffered on the II side (until acknowledged by the archive).
#[derive(Eq, PartialEq, Clone, CandidType, Debug, Deserialize)]
pub struct BufferedEntry {
    pub anchor_number: AnchorNumber,
    pub timestamp: u64,
    pub entry: ByteBuf,
    pub sequence_number: u64,
}

// Copies of `DefiniteCanisterSettings` resp. `CanisterStatusResponse` from ic-cdk v0.12, as v.0.13 has introduced
// new fields (cf. https://github.com/dfinity/cdk-rs/blob/main/src/ic-cdk/CHANGELOG.md#changed-3),
// and this leads to Candid incompatibility in `ArchiveStatus` returned by `status()-function.
#[derive(
    CandidType, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone, Default,
)]
pub struct DefiniteCanisterSettings {
    /// Controllers of the canister.
    pub controllers: Vec<Principal>,
    /// Compute allocation.
    pub compute_allocation: Nat,
    /// Memory allocation.
    pub memory_allocation: Nat,
    /// Freezing threshold.
    pub freezing_threshold: Nat,
}
#[derive(
    CandidType, Serialize, Deserialize, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone,
)]
pub struct CanisterStatus {
    /// See [CanisterStatusType].
    pub status: CanisterStatusType,
    /// See [DefiniteCanisterSettings].
    pub settings: DefiniteCanisterSettings,
    /// A SHA256 hash of the module installed on the canister. This is null if the canister is empty.
    pub module_hash: Option<Vec<u8>>,
    /// The memory size taken by the canister.
    pub memory_size: Nat,
    /// The cycle balance of the canister.
    pub cycles: Nat,
    /// Amount of cycles burned per day.
    pub idle_cycles_burned_per_day: Nat,
    /// Query statistics
    pub query_stats: QueryStats,
}

/// Information about the archive canister (i.e. useful for debugging).
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct ArchiveStatus {
    pub call_info: CallInfo,
    pub init: ArchiveInit,
    pub canister_status: CanisterStatus,
}

/// Information about the calls the archive is making to II.
#[derive(Clone, Debug, Default, CandidType, Deserialize)]
pub struct CallInfo {
    /// Timestamp of the last successful run of [fetch_entries], if any.
    pub last_successful_fetch: Option<FetchInfo>,
    /// A small buffer to keep the last call errors to help debugging in case of an incident.
    /// Can be retrieved using the info query.
    pub call_errors: Vec<CallErrorInfo>,
}

/// Information about the last successful fetch of II archive entries.
#[derive(Clone, Debug, Default, CandidType, Deserialize, Eq, PartialEq)]
pub struct FetchInfo {
    /// Timestamp when the last execution of the archive `fetch_entries` method finished.
    pub timestamp: Timestamp,
    /// The number of entries fetched (regardless of how many of those were actually archived).
    pub number_of_entries: u16,
}

/// Struct to keep debug info about a call failure.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct CallErrorInfo {
    /// Timestamp when the call was made (not when the error was received).
    pub time: u64,
    /// Target canister.
    pub canister: Principal,
    pub method: String,
    pub argument: ByteBuf,
    pub rejection_code: i32,
    pub message: String,
}

/// Sha256 Digest: 32 bytes
pub type Hash = [u8; 32];
