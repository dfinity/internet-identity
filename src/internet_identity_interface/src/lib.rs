use candid::{CandidType, Deserialize, Func, Principal};
use serde_bytes::{ByteBuf, Bytes};
use std::borrow::Cow;

pub type UserNumber = u64;
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
    pub assigned_user_number_range: (UserNumber, UserNumber),
}

// Archive specific types

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum OperationType {
    #[serde(rename = "register_anchor")]
    RegisterAnchor {
        initial_device: DeviceDataWithoutAlias,
    },
    #[serde(rename = "add_device")]
    AddDevice { new_device: DeviceDataWithoutAlias },
    #[serde(rename = "update_device")]
    UpdateDevice {
        updated_device: PublicKey,
        changed_data: DeviceDataUpdate,
    },
    #[serde(rename = "remove_device")]
    RemoveDevice { removed_device: PublicKey },
}

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub struct LogEntry {
    // store user_number in LogEntry, such that anchor operations can be attributed to a user without consulting the index.
    pub user_number: UserNumber,
    pub operation: OperationType,
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

// If present, the attribute has been changed to the value given.
// Does not include the pubkey because it cannot be changed.
#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub struct DeviceDataUpdate {
    pub alias: Option<Hidden>,
    pub credential_id: Option<CredentialId>,
    pub purpose: Option<Purpose>,
    pub key_type: Option<KeyType>,
    pub protection: Option<DeviceProtection>,
}

// Placeholder for information that has been hidden for privacy reasons.
#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum Hidden {
    HiddenForPrivacyReasons,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct Logs {
    // make this a vec of options to keep LogEntry extensible
    pub entries: Vec<Option<LogEntry>>,
    // index pointing to the next entry not included in this response, if any
    pub next_idx: Option<u64>,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct UserLogs {
    // make this a vec of options to keep LogEntry extensible
    pub entries: Vec<Option<LogEntry>>,
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

/// Init arguments of the II archive canister.
#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct ArchiveInit {
    pub ii_canister: Principal,
    pub max_entries_per_call: u16,
}
