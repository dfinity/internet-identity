use candid::{CandidType, Deserialize, Principal};
use serde_bytes::ByteBuf;

/// types related to the archive
pub mod archive;
/// types as specified by the HTTP gateway protocol.
/// See https://internetcomputer.org/docs/current/references/ic-interface-spec/#http-gateway
pub mod http_gateway;

pub type AnchorNumber = u64;
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
    pub origin: Option<String>,
}

impl From<DeviceWithUsage> for DeviceData {
    fn from(device: DeviceWithUsage) -> Self {
        Self {
            pubkey: device.pubkey,
            alias: device.alias,
            credential_id: device.credential_id,
            purpose: device.purpose,
            key_type: device.key_type,
            protection: device.protection,
            origin: device.origin,
        }
    }
}

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub struct DeviceWithUsage {
    pub pubkey: DeviceKey,
    pub alias: String,
    pub credential_id: Option<CredentialId>,
    pub purpose: Purpose,
    pub key_type: KeyType,
    pub protection: DeviceProtection,
    pub origin: Option<String>,
    pub last_usage: Option<Timestamp>,
}

impl From<DeviceData> for DeviceWithUsage {
    fn from(device: DeviceData) -> Self {
        Self {
            pubkey: device.pubkey,
            alias: device.alias,
            credential_id: device.credential_id,
            purpose: device.purpose,
            key_type: device.key_type,
            protection: device.protection,
            origin: device.origin,
            last_usage: None,
        }
    }
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
    Registered { user_number: AnchorNumber },
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
    pub devices: Vec<DeviceWithUsage>,
    pub device_registration: Option<DeviceRegistrationInfo>,
}

impl IdentityAnchorInfo {
    pub fn into_device_data(self) -> Vec<DeviceData> {
        self.devices.into_iter().map(DeviceData::from).collect()
    }
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct InternetIdentityInit {
    pub assigned_user_number_range: Option<(AnchorNumber, AnchorNumber)>,
    pub archive_config: Option<ArchiveConfig>,
    pub canister_creation_cycles_cost: Option<u64>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct InternetIdentityStats {
    pub assigned_user_number_range: (AnchorNumber, AnchorNumber),
    pub users_registered: u64,
    pub archive_info: ArchiveInfo,
    pub canister_creation_cycles_cost: u64,
    pub storage_layout_version: u8,
    pub active_anchor_stats: Option<ActiveAnchorStatistics>,
}

/// Information about the archive.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct ArchiveInfo {
    pub archive_canister: Option<Principal>,
    pub archive_config: Option<ArchiveConfig>,
}

/// Configuration parameters of the archive to be used on the next deployment.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct ArchiveConfig {
    // Wasm module hash that is allowed to be deployed to the archive canister.
    pub module_hash: [u8; 32],
    // Buffered archive entries limit. If reached, II will stop accepting new anchor operations
    // until the buffered operations are acknowledged by the archive.
    pub entries_buffer_limit: u64,
    // Polling interval at which the archive should fetch buffered archive entries from II (in nanoseconds).
    pub polling_interval_ns: u64,
    // Max number of archive entries to be fetched in a single call.
    pub entries_fetch_limit: u16,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct ActiveAnchorStatistics {
    // Stats for the last completed collection period for daily and monthly active anchors
    pub completed: CompletedActiveAnchorStats,
    // ongoing periods for daily and monthly active anchors
    pub ongoing: OngoingActiveAnchorStats,
}

impl ActiveAnchorStatistics {
    pub fn new(time: Timestamp) -> Self {
        Self {
            completed: CompletedActiveAnchorStats {
                daily_active_anchors: None,
                monthly_active_anchors: None,
            },
            ongoing: OngoingActiveAnchorStats {
                daily_active_anchors: ActiveAnchorCounter::new(time),
                monthly_active_anchors: vec![ActiveAnchorCounter::new(time)],
            },
        }
    }
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct CompletedActiveAnchorStats {
    pub daily_active_anchors: Option<ActiveAnchorCounter>,
    pub monthly_active_anchors: Option<ActiveAnchorCounter>,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct OngoingActiveAnchorStats {
    // Ongoing active anchor counter for
    pub daily_active_anchors: ActiveAnchorCounter,
    // Monthly active users are collected using 30 day sliding windows.
    // This vec contains up to 29 30-day active windows each offset by one day.
    // The vec is sorted with the first element being the next window to reach the end of the
    // 30 day collection period.
    pub monthly_active_anchors: Vec<ActiveAnchorCounter>,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct ActiveAnchorCounter {
    pub start_timestamp: Timestamp,
    pub counter: u64,
}

impl ActiveAnchorCounter {
    pub fn new(time: Timestamp) -> Self {
        Self {
            start_timestamp: time,
            counter: 0,
        }
    }
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
