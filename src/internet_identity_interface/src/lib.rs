use candid::{CandidType, Deserialize, Func, Principal};
use serde_bytes::{ByteBuf, Bytes};
use std::borrow::Cow;

pub type UserNumber = u64;
pub type Anchor = UserNumber;
pub type CredentialId = ByteBuf;
pub type PublicKey = ByteBuf;
pub type DeviceKey = PublicKey;
pub type UserKey = PublicKey;
pub type SessionKey = PublicKey;
pub type FrontendHostname = String;
pub type Timestamp = u64; // in nanos since epoch
pub type Signature = ByteBuf;
pub type DeviceVerificationCode = String;
pub type FailedAttemptsCounter = u8;

pub struct Base64(pub String);

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub struct DeviceData {
    pub pubkey: DeviceKey,
    pub alias: String,
    pub credential_id: Option<CredentialId>,
    pub purpose: Purpose,
    pub key_type: KeyType,
    pub protection: DeviceProtection,
}

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum Purpose {
    #[serde(rename = "recovery")]
    Recovery,
    #[serde(rename = "authentication")]
    Authentication,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum RegisterResponse {
    #[serde(rename = "registered")]
    Registered { user_number: UserNumber },
    #[serde(rename = "canister_full")]
    CanisterFull,
    #[serde(rename = "bad_challenge")]
    BadChallenge,
}

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum KeyType {
    #[serde(rename = "unknown")]
    Unknown,
    #[serde(rename = "platform")]
    Platform,
    #[serde(rename = "cross_platform")]
    CrossPlatform,
    #[serde(rename = "seed_phrase")]
    SeedPhrase,
}

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum DeviceProtection {
    #[serde(rename = "protected")]
    Protected,
    #[serde(rename = "unprotected")]
    Unprotected,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct Challenge {
    pub png_base64: String,
    pub challenge_key: ChallengeKey,
}

pub type ChallengeKey = String;

// The user's attempt
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct ChallengeAttempt {
    pub chars: String,
    pub key: ChallengeKey,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct Delegation {
    pub pubkey: PublicKey,
    pub expiration: Timestamp,
    pub targets: Option<Vec<Principal>>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct SignedDelegation {
    pub delegation: Delegation,
    pub signature: Signature,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum GetDelegationResponse {
    #[serde(rename = "signed_delegation")]
    SignedDelegation(SignedDelegation),
    #[serde(rename = "no_such_delegation")]
    NoSuchDelegation,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum AddTentativeDeviceResponse {
    #[serde(rename = "added_tentatively")]
    AddedTentatively {
        verification_code: DeviceVerificationCode,
        device_registration_timeout: Timestamp,
    },
    #[serde(rename = "device_registration_mode_off")]
    DeviceRegistrationModeOff,
    #[serde(rename = "another_device_tentatively_added")]
    AnotherDeviceTentativelyAdded,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum VerifyTentativeDeviceResponse {
    #[serde(rename = "verified")]
    Verified,
    #[serde(rename = "wrong_code")]
    WrongCode { retries_left: u8 },
    #[serde(rename = "device_registration_mode_off")]
    DeviceRegistrationModeOff,
    #[serde(rename = "no_device_to_verify")]
    NoDeviceToVerify,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct DeviceRegistrationInfo {
    pub expiration: Timestamp,
    pub tentative_device: Option<DeviceData>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct IdentityAnchorInfo {
    pub devices: Vec<DeviceData>,
    pub device_registration: Option<DeviceRegistrationInfo>,
}

pub type HeaderField = (String, String);

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct Token {}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum StreamingStrategy {
    Callback { callback: Func, token: Token },
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct StreamingCallbackHttpResponse {
    pub body: ByteBuf,
    pub token: Option<Token>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpRequest {
    pub method: String,
    pub url: String,
    pub headers: Vec<(String, String)>,
    pub body: ByteBuf,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct HttpResponse {
    pub status_code: u16,
    pub headers: Vec<HeaderField>,
    pub body: Cow<'static, Bytes>,
    pub streaming_strategy: Option<StreamingStrategy>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct InternetIdentityInit {
    pub assigned_user_number_range: Option<(UserNumber, UserNumber)>,
    pub archive_module_hash: Option<[u8; 32]>,
    pub canister_creation_cycles_cost: Option<u64>,
    pub memory_migration_batch_size: Option<u32>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum MigrationState {
    #[serde(rename = "not_started")]
    NotStarted,
    #[serde(rename = "in_progress")]
    InProgress { anchors_left: u64, batch_size: u64 },
    #[serde(rename = "paused")]
    Paused,
    #[serde(rename = "finished")]
    Finished,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct InternetIdentityStats {
    pub assigned_user_number_range: (UserNumber, UserNumber),
    pub users_registered: u64,
    pub archive_info: ArchiveInfo,
    pub canister_creation_cycles_cost: u64,
    pub anchor_migration_state: Option<MigrationState>,
}

// Archive specific types

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
    pub anchor: Anchor,
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

/// Information about the archive.
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct ArchiveInfo {
    pub archive_canister: Option<Principal>,
    pub expected_wasm_hash: Option<[u8; 32]>,
}

/// Init arguments of the archive canister.
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct ArchiveInit {
    pub ii_canister: Principal,
    pub max_entries_per_call: u16,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum DeployArchiveResult {
    #[serde(rename = "success")]
    Success(Principal),
    #[serde(rename = "creation_in_progress")]
    CreationInProgress,
    #[serde(rename = "failed")]
    Failed(String),
}
