use candid::{CandidType, Deserialize, Principal};
use serde::Serialize;
use serde_bytes::ByteBuf;
use std::collections::HashMap;

pub type AnchorNumber = u64;
pub type CredentialId = ByteBuf;
pub type Aaguid = [u8; 16];
pub type PublicKey = ByteBuf;
pub type DeviceKey = PublicKey;
pub type UserKey = PublicKey;
pub type SessionKey = PublicKey;
pub type CanisterSigPublicKeyDer = PublicKey;
pub type FrontendHostname = String;
pub type ApplicationNumber = u64;
pub type Timestamp = u64; // in nanos since epoch
pub type Signature = ByteBuf;
pub type DeviceConfirmationCode = String;
pub type FailedAttemptsCounter = u8;
pub type AccountNumber = u64;

mod api_v2;
pub mod attributes;
pub mod openid;
pub mod vc_mvp;

// re-export v2 types without the ::v2 prefix, so that this crate can be restructured once v1 is removed
// without breaking clients
pub use crate::internet_identity::types::openid::*;
pub use api_v2::*;

#[derive(Eq, PartialEq, Clone, Debug, CandidType, Deserialize)]
pub struct DeviceData {
    pub pubkey: DeviceKey,
    pub alias: String,
    pub credential_id: Option<CredentialId>,
    pub aaguid: Option<Aaguid>,
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
    pub aaguid: Option<[u8; 16]>,
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
    #[serde(rename = "browser_storage_key")]
    BrowserStorageKey,
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

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
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
        verification_code: DeviceConfirmationCode,
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

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DeviceRegistrationInfo {
    pub expiration: Timestamp,
    pub tentative_device: Option<DeviceData>,
    pub tentative_session: Option<Principal>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct IdentityAnchorInfo {
    pub devices: Vec<DeviceWithUsage>,
    pub device_registration: Option<DeviceRegistrationInfo>,
    pub openid_credentials: Option<Vec<OpenIdCredentialData>>,
    pub name: Option<String>,
    pub created_at: Option<Timestamp>,
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

/// Init arguments of II frontend canister which can be supplied on install and upgrade.
///
/// Each field is wrapped in `Option<>` to indicate whether the field should be updated or not.
///
/// Some fields, like `analytics_config`, have an additional nested `Option<>`, this indicates
/// enable/disable status (e.g. `Some(None)` disables a feature while `None` leaves it untouched).
#[derive(Clone, Debug, CandidType, Serialize, Deserialize, Eq, PartialEq)]
pub struct InternetIdentityFrontendArgs {
    pub backend_canister_id: Principal,
    /// For example, "https://backend.id.ai" (no trailing slash)
    pub backend_origin: String,

    pub related_origins: Option<Vec<String>>,
    pub openid_configs: Option<Vec<OpenIdConfig>>,
    pub fetch_root_key: Option<bool>,
    pub analytics_config: Option<Option<AnalyticsConfig>>,
    pub dummy_auth: Option<Option<DummyAuthConfig>>,
}

impl From<InternetIdentityFrontendArgs> for InternetIdentityInit {
    fn from(value: InternetIdentityFrontendArgs) -> Self {
        let InternetIdentityFrontendArgs {
            backend_canister_id,
            backend_origin,
            fetch_root_key,
            analytics_config,
            dummy_auth,
            related_origins,
            openid_configs,
        } = value;

        Self {
            backend_canister_id: Some(backend_canister_id),
            backend_origin: Some(backend_origin),

            fetch_root_key,
            analytics_config,
            dummy_auth,
            related_origins,

            // TODO: pull this config field from the backend and set it to None here.
            openid_configs,

            // Config fields not used by the frontend
            canister_creation_cycles_cost: None,
            assigned_user_number_range: None,
            archive_config: None,
            register_rate_limit: None,

            // Deprecated config fields
            enable_dapps_explorer: None,
            captcha_config: None,
            is_production: None,
            new_flow_origins: None,
        }
    }
}

/// Config fields that are synchronized between the frontend and backend.
///
/// Since the II frontend is stateless, this config is pulled from the backend
/// on page load via HTTPS. It is served as an encoded Candid value.
///
/// The fields are wrapped in `Option<>` to help evolve this API in the future,
/// since non-optional fields cannot be deprecated in Candid.
#[derive(Clone, Debug, CandidType, Deserialize, Default, Eq, PartialEq)]
pub struct InternetIdentitySynchronizedConfig {
    pub openid_configs: Option<Vec<OpenIdConfig>>,
}

impl From<&InternetIdentityInit> for InternetIdentitySynchronizedConfig {
    fn from(value: &InternetIdentityInit) -> Self {
        Self {
            openid_configs: value.openid_configs.clone(),
        }
    }
}

/// Init arguments of II which can be supplied on install and upgrade.
///
/// Each field is wrapped in `Option<>` to indicate whether the field should
/// keep the previous value or update to a new value (e.g. `None` keeps the previous value).
///
/// Some fields, like `analytics_config`, have an additional nested `Option<>`, this indicates
/// enable/disable status (e.g. `Some(None)` disables a feature while `None` leaves it untouched).
#[derive(Clone, Debug, CandidType, Deserialize, Default, Eq, PartialEq)]
pub struct InternetIdentityInit {
    pub assigned_user_number_range: Option<(AnchorNumber, AnchorNumber)>,
    pub archive_config: Option<ArchiveConfig>,
    pub canister_creation_cycles_cost: Option<u64>,
    pub register_rate_limit: Option<RateLimitConfig>,
    pub captcha_config: Option<CaptchaConfig>,
    pub related_origins: Option<Vec<String>>,
    pub new_flow_origins: Option<Vec<String>>,
    pub openid_configs: Option<Vec<OpenIdConfig>>,
    pub analytics_config: Option<Option<AnalyticsConfig>>,
    pub fetch_root_key: Option<bool>,
    pub enable_dapps_explorer: Option<bool>,
    pub is_production: Option<bool>,
    pub dummy_auth: Option<Option<DummyAuthConfig>>,
    pub backend_canister_id: Option<Principal>,
    pub backend_origin: Option<String>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct InternetIdentityStats {
    pub assigned_user_number_range: (AnchorNumber, AnchorNumber),
    pub users_registered: u64,
    pub archive_info: ArchiveInfo,
    pub canister_creation_cycles_cost: u64,
    pub storage_layout_version: u8,
    /// Aggregations of events that have been processed by the II.
    /// The map contains a key for each aggregation type, and the value is a list of tuples
    /// from aggregated sub-key (i.e. for prepare_delegation it's the frontend origin) to weight.
    pub event_aggregations: HashMap<String, Vec<(String, u64)>>,
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

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct CaptchaConfig {
    pub max_unsolved_captchas: u64,
    pub captcha_trigger: CaptchaTrigger,
}

#[derive(Clone, Debug, CandidType, Serialize, Deserialize, Eq, PartialEq)]
pub enum AnalyticsConfig {
    Plausible {
        // Config params from Plausible NPM package
        // https://www.npmjs.com/package/plausible-tracker
        domain: Option<String>,
        hash_mode: Option<bool>,
        track_localhost: Option<bool>,
        api_host: Option<String>,
    },
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum CaptchaTrigger {
    Dynamic {
        threshold_pct: u16,
        current_rate_sampling_interval_s: u64,
        reference_rate_sampling_interval_s: u64,
    },
    Static(StaticCaptchaTrigger),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum StaticCaptchaTrigger {
    CaptchaEnabled,
    CaptchaDisabled,
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

#[derive(Clone, Debug, CandidType, Deserialize)]
pub enum DeployArchiveResult {
    #[serde(rename = "success")]
    Success(Principal),
    #[serde(rename = "creation_in_progress")]
    CreationInProgress,
    #[serde(rename = "failed")]
    Failed(String),
}

#[derive(Clone, Debug, CandidType, Serialize, Deserialize, Default, Eq, PartialEq)]
pub struct OpenIdConfig {
    pub name: String,
    pub logo: String,
    pub issuer: String,
    pub client_id: String,
    pub jwks_uri: String,
    pub auth_uri: String,
    pub auth_scope: Vec<String>,
    pub fedcm_uri: Option<String>,
}

pub enum AuthorizationKey {
    DeviceKey(DeviceKey),
    OpenIdCredentialKey((OpenIdCredentialKey, Option<ConfigIss>)),
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct DeviceKeyWithAnchor {
    pub pubkey: DeviceKey,
    pub anchor_number: AnchorNumber,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct AccountInfo {
    pub account_number: Option<AccountNumber>, // None is the unreserved synthetic account
    pub origin: FrontendHostname,
    pub last_used: Option<Timestamp>,
    pub name: Option<String>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct AccountUpdate {
    pub name: Option<String>,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum CreateAccountError {
    InternalCanisterError(String),
    AccountLimitReached,
    Unauthorized(Principal),
    NameTooLong,
}

impl From<CheckMaxAccountError> for CreateAccountError {
    fn from(err: CheckMaxAccountError) -> Self {
        match err {
            CheckMaxAccountError::AccountLimitReached => Self::AccountLimitReached,
        }
    }
}

impl From<AccountNameValidationError> for CreateAccountError {
    fn from(err: AccountNameValidationError) -> Self {
        match err {
            AccountNameValidationError::NameTooLong => Self::NameTooLong,
        }
    }
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum UpdateAccountError {
    InternalCanisterError(String),
    AccountLimitReached,
    Unauthorized(Principal),
    NameTooLong,
}

impl From<CheckMaxAccountError> for UpdateAccountError {
    fn from(err: CheckMaxAccountError) -> Self {
        match err {
            CheckMaxAccountError::AccountLimitReached => Self::AccountLimitReached,
        }
    }
}

impl From<AccountNameValidationError> for UpdateAccountError {
    fn from(err: AccountNameValidationError) -> Self {
        match err {
            AccountNameValidationError::NameTooLong => Self::NameTooLong,
        }
    }
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum GetAccountsError {
    InternalCanisterError(String),
    Unauthorized(Principal),
}

#[derive(CandidType, Deserialize)]
pub struct PrepareAccountDelegation {
    pub user_key: UserKey,
    pub expiration: Timestamp,
}

#[derive(CandidType, Debug, Deserialize)]
pub enum AccountDelegationError {
    Unauthorized(Principal),
    InternalCanisterError(String),
    NoSuchDelegation,
}

#[derive(CandidType, Debug, Deserialize)]
pub enum CheckMaxAccountError {
    AccountLimitReached,
}

#[derive(CandidType, Debug, Deserialize)]
pub enum AccountNameValidationError {
    NameTooLong,
}

#[derive(Clone, Debug, CandidType, Serialize, Deserialize, Default, Eq, PartialEq)]
pub struct DummyAuthConfig {
    pub prompt_for_index: bool,
}

#[derive(CandidType, Debug, Deserialize, PartialEq, Serialize)]
pub enum GetAccountError {
    NoSuchOrigin {
        anchor_number: AnchorNumber,
    },
    NoSuchAccount {
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
    },
}

#[derive(CandidType, Debug, Deserialize, PartialEq)]
pub enum GetDefaultAccountError {
    InternalCanisterError(String),
    Unauthorized(Principal),
    NoSuchAnchor,
    NoSuchOrigin { anchor_number: AnchorNumber },
}

#[derive(CandidType, Debug, Deserialize, PartialEq)]
pub enum SetDefaultAccountError {
    InternalCanisterError(String),
    Unauthorized(Principal),
    NoSuchAnchor,
    NoSuchOrigin {
        anchor_number: AnchorNumber,
    },
    NoSuchAccount {
        anchor_number: AnchorNumber,
        origin: FrontendHostname,
    },
}
