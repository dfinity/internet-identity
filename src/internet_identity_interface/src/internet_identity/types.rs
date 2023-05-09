use candid::{CandidType, Deserialize, Principal};
use serde_bytes::ByteBuf;
use std::collections::HashMap;

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
    // Metadata map for additional device information.
    //
    // Note: some fields above will be moved to the metadata map in the future.
    // All field names of `DeviceData` (such as 'alias', 'origin, etc.) are
    // reserved and cannot be written.
    pub metadata: Option<HashMap<String, MetadataEntry>>,
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
    pub metadata: Option<HashMap<String, MetadataEntry>>,
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

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub enum MetadataEntry {
    #[serde(rename = "string")]
    String(String),
    #[serde(rename = "bytes")]
    Bytes(ByteBuf),
    #[serde(rename = "map")]
    Map(HashMap<String, MetadataEntry>),
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

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct WebAuthnCredential {
    pub pubkey: DeviceKey,
    pub credential_id: CredentialId,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Default)]
pub struct AnchorCredentials {
    pub credentials: Vec<WebAuthnCredential>,
    pub recovery_credentials: Vec<WebAuthnCredential>,
    pub recovery_phrases: Vec<PublicKey>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Default)]
pub struct InternetIdentityInit {
    pub assigned_user_number_range: Option<(AnchorNumber, AnchorNumber)>,
    pub archive_config: Option<ArchiveConfig>,
    pub canister_creation_cycles_cost: Option<u64>,
    pub register_rate_limit: Option<RateLimitConfig>,
    pub max_num_latest_delegation_origins: Option<u64>,
    pub migrate_storage_to_memory_manager: Option<bool>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct InternetIdentityStats {
    pub assigned_user_number_range: (AnchorNumber, AnchorNumber),
    pub users_registered: u64,
    pub archive_info: ArchiveInfo,
    pub canister_creation_cycles_cost: u64,
    pub storage_layout_version: u8,
    pub active_anchor_stats: Option<ActiveAnchorStatistics<ActiveAnchorCounter>>,
    pub domain_active_anchor_stats: Option<ActiveAnchorStatistics<DomainActiveAnchorCounter>>,
    pub max_num_latest_delegation_origins: u64,
    pub latest_delegation_origins: Vec<FrontendHostname>,
}

/// Information about the archive.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct ArchiveInfo {
    pub archive_canister: Option<Principal>,
    pub archive_config: Option<ArchiveConfig>,
}

/// Configuration for a rate limit.
/// Currently only used on the `register` call.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct RateLimitConfig {
    // time it takes for a rate limiting token to be replenished.
    pub time_per_token_ns: u64,
    // How many tokens are at most generated (to accommodate peaks).
    pub max_tokens: u64,
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
pub struct ActiveAnchorStatistics<T> {
    // Stats for the last completed collection period for daily and monthly active anchors
    pub completed: CompletedActiveAnchorStats<T>,
    // ongoing periods for daily and monthly active anchors
    pub ongoing: OngoingActiveAnchorStats<T>,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct CompletedActiveAnchorStats<T> {
    pub daily_active_anchors: Option<T>,
    pub monthly_active_anchors: Option<T>,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct OngoingActiveAnchorStats<T> {
    // Ongoing active anchor counter for the current 24 h time bucket.
    pub daily_active_anchors: T,
    // Monthly active users are collected using 30-day sliding windows.
    // This vec contains up to 30 30-day active windows each offset by one day.
    // The vec is sorted, new collection windows are added at the end.
    pub monthly_active_anchors: Vec<T>,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct ActiveAnchorCounter {
    pub start_timestamp: Timestamp,
    pub counter: u64,
}

#[derive(Clone, CandidType, Deserialize, Eq, PartialEq, Debug)]
pub struct DomainActiveAnchorCounter {
    pub start_timestamp: Timestamp,
    pub ic0_app_counter: u64,
    pub internetcomputer_org_counter: u64,
    pub both_ii_domains_counter: u64,
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
