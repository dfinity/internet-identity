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
pub mod dnssec;
pub mod doh;
pub mod email_challenge;
pub mod email_recovery;
pub mod icrc3;
pub mod openid;
pub mod smtp;
pub mod vc_mvp;
pub mod verified_email;

// re-export v2 types without the ::v2 prefix, so that this crate can be restructured once v1 is removed
// without breaking clients
pub use crate::internet_identity::types::dnssec::*;
pub use crate::internet_identity::types::doh::*;
pub use crate::internet_identity::types::email_challenge::*;
pub use crate::internet_identity::types::email_recovery::*;
pub use crate::internet_identity::types::openid::*;
pub use crate::internet_identity::types::smtp::*;
pub use crate::internet_identity::types::verified_email::*;
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

/// The delegation permissions a caller requests, mirroring the ICP protocol's
/// request-delegation `permissions` values. Passed as an optional argument to
/// `prepare_account_delegation` / `get_account_delegation` / `mcp_set_access`;
/// an omitted argument means `All` (unrestricted), preserving the pre-feature
/// behavior and matching the interface spec's default for an absent
/// `permissions` field.
#[derive(Clone, Copy, Debug, Eq, PartialEq, CandidType, Deserialize)]
pub enum Permissions {
    /// Queries-only: the issued delegation carries `permissions = "queries"`,
    /// so the IC rejects update calls authenticated through it.
    #[serde(rename = "queries")]
    Queries,
    /// Unrestricted, update-capable: the issued delegation carries no
    /// `permissions` field (the protocol's `"all"` default).
    #[serde(rename = "all")]
    All,
}

#[derive(Clone, Debug, CandidType, Deserialize)]
pub struct Delegation {
    pub pubkey: PublicKey,
    pub expiration: Timestamp,
    pub targets: Option<Vec<Principal>>,
    /// Restricts the kinds of calls the delegation permits: `"queries"`
    /// restricts the sender to query calls (the IC rejects update calls
    /// authenticated through such a delegation). `None` means unrestricted.
    pub permissions: Option<String>,
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

#[derive(Clone, Debug, CandidType, Deserialize, PartialEq)]
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
    #[serde(rename = "passkey_with_this_public_key_is_already_used")]
    PasskeyWithThisPublicKeyIsAlreadyUsed,
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
    pub fetch_root_key: Option<bool>,
    pub analytics_config: Option<Option<AnalyticsConfig>>,
    pub dummy_auth: Option<Option<DummyAuthConfig>>,
    /// Weakens the Content Security Policy to facilitate local development over HTTP.
    /// When enabled:
    /// * allows accessing II using http instead of https
    /// * allows II to connect to localhost on both http and https
    pub dev_csp: Option<bool>,
    /// Origins of apps to feature on the dashboard home. Each origin is resolved
    /// against the bundled dapps catalogue (`dapps.json`) at runtime to obtain
    /// the name, description and logo. Unknown origins are skipped.
    pub featured_dashboard_apps: Option<Vec<String>>,
    /// Frontend feature flag overrides keyed by flag name (e.g.
    /// `("EMAIL_RECOVERY", true)`). Each entry sets the deployment-level
    /// baseline for that flag; the frontend still lets `localStorage`, a flag's
    /// own init callback, and `?feature_flag_*` URL params take precedence.
    /// Names that don't match a known frontend flag are ignored by the frontend.
    pub feature_flags: Option<Vec<(String, bool)>>,
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
    /// Deploy flag relaxing the `https` requirement for SSO discovery outcalls to
    /// loopback hosts (`localhost` / `127.0.0.1`) so e2e tests can point at local
    /// mock IdPs served over plain `http`. `None` / `Some(false)` (the default)
    /// require `https` for every discovery host. Never enable in production —
    /// non-loopback hosts always require `https` regardless of this flag.
    pub sso_allow_insecure_discovery: Option<bool>,
    /// One-shot backfill of the `sso_domain` / `sso_name` fields on stored
    /// `OpenIdCredential`s (see `docs/ongoing/openid-sso-prod-readiness.md`
    /// §8.6). When `Some`, a batched timer-driven migration stamps every
    /// stored credential whose `(iss, aud)` matches an entry and whose
    /// `sso_domain` is not set yet. Idempotent — already-stamped credentials
    /// are skipped, so re-submitting (e.g. with a corrected list) is safe.
    /// When `None`, no backfill runs.
    pub sso_credential_migration: Option<Vec<SsoCredentialMigrationEntry>>,
    pub analytics_config: Option<Option<AnalyticsConfig>>,
    pub enable_dapps_explorer: Option<bool>,
    pub is_production: Option<bool>,
    pub dummy_auth: Option<Option<DummyAuthConfig>>,
    pub backend_canister_id: Option<Principal>,
    pub backend_origin: Option<String>,
    /// Deploy flag for the legacy DNSSEC email-recovery path. Defaults to
    /// off (DoH-only); `Some(true)` re-enables it. Omitting it on upgrade
    /// keeps the stored value.
    pub enable_dnssec_email_recovery: Option<bool>,
    /// DNSSEC trust anchors for any feature that verifies DNS records
    /// against the IANA-rooted DNSSEC chain (currently the email-recovery
    /// DKIM/DMARC flow, see `docs/ongoing/email-recovery.md` §7.5).
    ///
    /// Wrapped in `Option<Option<...>>` to match the same set/clear pattern
    /// as `analytics_config` and `dummy_auth`: outer `None` keeps the
    /// previously-stored value across an upgrade, `Some(None)` clears it,
    /// `Some(Some(c))` sets it to `c`.
    pub dnssec_config: Option<Option<DnssecConfig>>,
    /// DoH (DNS-over-HTTPS) fallback configuration. Allowlists the
    /// domains for which the canister may fetch DKIM / DMARC TXT
    /// records via HTTP outcalls (covers the consumer-mailbox
    /// providers whose DNS zones aren't DNSSEC-signed — see
    /// `docs/ongoing/email-recovery.md` §7.6). Same set/clear pattern
    /// as `dnssec_config`.
    pub doh_config: Option<Option<DohConfig>>,
}

/// One entry of the `sso_credential_migration` backfill (see
/// `InternetIdentityInit::sso_credential_migration`). Maps the `(iss, aud)`
/// pair of a stored SSO credential to the discovery domain and optional
/// human-readable name it resolves to.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SsoCredentialMigrationEntry {
    pub discovery_domain: String,
    /// Matches the stored credential's `iss`.
    pub issuer: String,
    /// Matches the stored credential's `aud`.
    pub client_id: String,
    /// Human-readable SSO label; stamped onto the credential's `sso_name`.
    pub name: Option<String>,
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

#[derive(Clone, Copy, Debug, CandidType, Serialize, Deserialize, Eq, PartialEq)]
pub enum OpenIdEmailVerificationScheme {
    Unknown,
    Google,
    Microsoft,
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
    pub email_verification: Option<OpenIdEmailVerificationScheme>,
    /// Optional initial set of JWKs used to seed this provider's JWK cache on
    /// install, so JWT verification works before the first `jwks_uri` fetch
    /// completes (and across upgrades — the cache is persisted in stable memory
    /// in the backend's storage layer).
    ///
    /// The outer vector is the set of JWKs; each inner vector is one JWK,
    /// represented as the list of its JSON `(field, value)` pairs. For example a
    /// single RSA key is
    /// `vec { vec { record { "kty"; "RSA" }; record { "kid"; "..." };
    ///              record { "n"; "..." }; record { "e"; "AQAB" } } }`.
    /// Each inner list is assembled into a JSON object and parsed with the same
    /// path used for fetched certs, so all string-valued JWK fields are
    /// supported. Invalid entries are skipped.
    pub seed_jwks: Option<Vec<Vec<(String, String)>>>,
}

/// SSO provider configuration that uses two-hop discovery.
///
/// The backend fetches `https://{discovery_domain}/.well-known/ii-openid-configuration`
/// to obtain `{ client_id, openid_configuration }`, then fetches the standard OIDC
/// discovery document at `openid_configuration` to resolve `issuer` and `jwks_uri`.
#[derive(Clone, Debug, CandidType, Serialize, Deserialize, Default, Eq, PartialEq)]
pub struct DiscoverableOidcConfig {
    pub discovery_domain: String,
}

/// Fully resolved SSO discovery result returned by `discover_sso` /
/// `discover_sso_query`. Carries everything the frontend needs to build the
/// authorization request: the canister resolves it from the domain's two-hop
/// discovery documents.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct SsoDiscovery {
    pub discovery_domain: String,
    /// The org's primary OIDC client.
    pub client_id: String,
    pub issuer: String,
    pub authorization_endpoint: String,
    pub scopes: Vec<String>,
    /// Human-readable SSO label, if the domain published one in its
    /// `ii-openid-configuration`.
    pub name: Option<String>,
    /// Client the frontend runs the ceremony against for the requested origin;
    /// `None` when the origin is denied.
    pub resolved_client_id: Option<String>,
}

/// Status of a domain's SSO discovery, read by `get_sso_discovery_status`. A
/// failed fetch isn't a distinct status — it reads as `Pending` and the frontend
/// times out — so the statuses are: resolved, or in flight.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum SsoDiscoveryStatus {
    /// Discovery completed; the resolved configuration.
    Resolved(SsoDiscovery),
    /// Discovery is in flight (or not yet started) — drive it with
    /// `discover_sso` and poll again.
    Pending,
}

/// Request for `get_sso_discovery_status`.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct GetSsoDiscoveryStatusRequest {
    /// The org's SSO discovery domain.
    pub org_domain: String,
    /// The gated dapp origin, when resolving which per-app client serves it.
    pub target_app_origin: Option<FrontendHostname>,
}

pub enum AuthorizationKey {
    DeviceKey(DeviceKey),
    OpenIdCredentialKey((OpenIdCredentialKey, Option<ConfigIss>)),
    /// The caller authenticated via an email-recovery delegation.
    /// Carries the lowercased canonical address whose binding on the
    /// anchor produced the matching principal — `activity_bookkeeping`
    /// uses it to bump `last_used` on the credential.
    EmailRecoveryAddress(String),
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

/// Result of `mcp_prepare_delegation`. The matching `mcp_get_delegation` must
/// use the *same* account, so we return the resolved `account_number` for the
/// server to thread back. When preparing, the MCP server may name an account
/// explicitly (one of the anchor's accounts at `target_origin`) or leave it
/// unset to use the anchor's default there; either way `account_number`
/// reports the one actually used. (Accounts are per-origin, so this is an
/// account at `target_origin` — the app being acted on; no account is chosen
/// when connecting the MCP server.) Returning it also keeps `get` from
/// independently re-resolving the *mutable* default and, if it changed in
/// between, looking under a different account's seed and returning
/// `NoSuchDelegation`.
#[derive(CandidType, Deserialize)]
pub struct McpPrepareDelegation {
    pub user_key: UserKey,
    pub expiration: Timestamp,
    pub account_number: Option<AccountNumber>,
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

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct PrepareSessionDelegation {
    pub user_key: UserKey,
    pub expiration: Timestamp,
}

#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub enum SessionDelegationError {
    InternalCanisterError(String),
    Unauthorized(Principal),
    NoSuchDelegation,
}

/// The identity's synced trusted-MCP-server configuration: a master toggle and
/// the single MCP server URL the user trusts. Persisted on-chain (keyed by
/// anchor) so it follows the identity across all of its devices — unlike the
/// device-local CLI-access toggle. Read by the Settings UI and by the `/mcp`
/// connect flow, which verifies the connecting origin against it at connect
/// time. `url` is kept verbatim so Settings can display/re-probe a path-based
/// endpoint like `https://host/mcp`; trust matching is by origin.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq, Default)]
pub struct McpConfig {
    pub enabled: bool,
    pub url: Option<String>,
}

/// Result of the internal MCP grant-binding (`mcp::register`): the expiration
/// (nanoseconds since the epoch) of the MCP session grant just registered. The
/// connect flow's `mcp_register_v2` invokes that helper and surfaces the same
/// expiration to the server as `McpRegistrationV2`. Every server-facing
/// `mcp_*` call returns `Unauthorized` once that expiry passes; the server
/// reconnects through a new consent flow.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct McpRegistration {
    pub expiration: Timestamp,
}

/// Result of `prepare_mcp_registration_delegation`: the canister-signature
/// public key the registration delegation chain is rooted at (`P_reg`; the
/// canister-signed hop delegates to the browser-held registration key `Y`),
/// and the (short) expiration of that delegation. The frontend fetches the
/// signed delegation with `get_mcp_registration_delegation`, extends the chain
/// browser-side to the MCP server's key, and delivers it to the server, which
/// redeems it via `mcp_register_v2`.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct PrepareMcpRegistrationDelegation {
    pub user_key: UserKey,
    pub expiration: Timestamp,
}

/// Result of `mcp_register_v2`: the expiration (ns since epoch) of the MCP
/// session grant just registered, plus the access level the user chose at
/// connect (`queries` = read-only, `all` = full). The server reads `permissions`
/// to learn the read-only state up front — with the v2 flow there is no
/// completion POST carrying it.
#[derive(Clone, Debug, CandidType, Deserialize, Eq, PartialEq)]
pub struct McpRegistrationV2 {
    pub expiration: Timestamp,
    pub permissions: Permissions,
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
