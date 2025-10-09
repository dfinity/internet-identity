import type { Principal } from '@dfinity/principal';
import type { ActorMethod } from '@dfinity/agent';
import type { IDL } from '@dfinity/candid';

export type AccountDelegationError = { 'NoSuchDelegation' : null } |
  { 'InternalCanisterError' : string } |
  { 'Unauthorized' : Principal };
export interface AccountInfo {
  /**
   * Configurable properties
   */
  'name' : [] | [string],
  'origin' : string,
  /**
   * Null is unreserved default account
   */
  'account_number' : [] | [AccountNumber],
  'last_used' : [] | [Timestamp],
}
export type AccountNumber = bigint;
export interface AccountUpdate { 'name' : [] | [string] }
export type AddTentativeDeviceResponse = {
    /**
     * Device registration mode is off, either due to timeout or because it was never enabled.
     */
    'device_registration_mode_off' : null
  } |
  {
    /**
     * There is another device already added tentatively
     */
    'another_device_tentatively_added' : null
  } |
  {
    /**
     * The device was tentatively added.
     */
    'added_tentatively' : {
      'verification_code' : string,
      /**
       * Expiration date, in nanos since the epoch
       */
      'device_registration_timeout' : Timestamp,
    }
  };
export type AnalyticsConfig = {
    'Plausible' : {
      'domain' : [] | [string],
      'track_localhost' : [] | [boolean],
      'hash_mode' : [] | [boolean],
      'api_host' : [] | [string],
    }
  };
export interface AnchorCredentials {
  'recovery_phrases' : Array<PublicKey>,
  'credentials' : Array<WebAuthnCredential>,
  'recovery_credentials' : Array<WebAuthnCredential>,
}
/**
 * Configuration parameters related to the archive.
 */
export interface ArchiveConfig {
  /**
   * Polling interval to fetch new entries from II (in nanoseconds).
   * Changes to this parameter will only take effect after an archive deployment.
   */
  'polling_interval_ns' : bigint,
  /**
   * Buffered archive entries limit. If reached, II will stop accepting new anchor operations
   * until the buffered operations are acknowledged by the archive.
   */
  'entries_buffer_limit' : bigint,
  /**
   * The allowed module hash of the archive canister.
   * Changing this parameter does _not_ deploy the archive, but enable archive deployments with the
   * corresponding wasm module.
   */
  'module_hash' : Uint8Array | number[],
  /**
   * The maximum number of entries to be transferred to the archive per call.
   */
  'entries_fetch_limit' : number,
}
/**
 * Information about the archive.
 */
export interface ArchiveInfo {
  /**
   * Configuration parameters related to the II archive.
   */
  'archive_config' : [] | [ArchiveConfig],
  /**
   * Canister id of the archive or empty if no archive has been deployed yet.
   */
  'archive_canister' : [] | [Principal],
}
export type Aud = string;
/**
 * The authentication methods currently supported by II.
 */
export type AuthnMethod = { 'PubKey' : PublicKeyAuthn } |
  { 'WebAuthn' : WebAuthn };
export type AuthnMethodAddError = { 'InvalidMetadata' : string };
export interface AuthnMethodConfirmationCode {
  'confirmation_code' : string,
  'expiration' : Timestamp,
}
export type AuthnMethodConfirmationError = {
    'InternalCanisterError' : string
  } |
  {
    /**
     * Authentication method registration mode is off, either due to timeout or because it was never enabled.
     */
    'RegistrationModeOff' : null
  } |
  { 'Unauthorized' : Principal } |
  {
    /**
     * There is no registered authentication method to be confirmed.
     */
    'NoAuthnMethodToConfirm' : null
  } |
  {
    /**
     * Wrong confirmation code entered. Retry with correct code.
     */
    'WrongCode' : { 'retries_left' : number }
  };
export interface AuthnMethodData {
  'security_settings' : AuthnMethodSecuritySettings,
  /**
   * contains the following fields of the DeviceWithUsage type:
   * - alias
   * - origin
   * - authenticator_attachment: data taken from key_type and reduced to "platform", "cross_platform" or absent on migration
   * - usage: data taken from key_type and reduced to "recovery_phrase", "browser_storage_key" or absent on migration
   * Note: for compatibility reasons with the v1 API, the entries above (if present)
   * must be of the `String` variant. This restriction may be lifted in the future.
   */
  'metadata' : MetadataMapV2,
  'last_authentication' : [] | [Timestamp],
  'authn_method' : AuthnMethod,
}
export type AuthnMethodMetadataReplaceError = {
    /**
     * No authentication method found with the given public key.
     */
    'AuthnMethodNotFound' : null
  } |
  { 'InvalidMetadata' : string };
/**
 * This describes whether an authentication method is "protected" or not.
 * When protected, a authentication method can only be updated or removed if the
 * user is authenticated with that very authentication method.
 */
export type AuthnMethodProtection = { 'Protected' : null } |
  { 'Unprotected' : null };
export type AuthnMethodPurpose = { 'Recovery' : null } |
  { 'Authentication' : null };
export type AuthnMethodRegisterError = {
    /**
     * Authentication method registration mode is off, either due to timeout or because it was never enabled.
     */
    'RegistrationModeOff' : null
  } |
  {
    /**
     * There is another authentication method already registered that needs to be confirmed first.
     */
    'RegistrationAlreadyInProgress' : null
  } |
  {
    /**
     * The metadata of the provided authentication method contains invalid entries.
     */
    'InvalidMetadata' : string
  };
/**
 * Extra information about registration status for new authentication methods
 */
export interface AuthnMethodRegistrationInfo {
  /**
   * The timestamp at which the identity will turn off registration mode
   * (and the authentication method will be forgotten, if any, and if not verified)
   */
  'expiration' : Timestamp,
  /**
   * If present, the user has registered a new session. This new session needs to be confirmed before
   * 'expiration' in order for it be authorized to register an authentication method to the identity.
   */
  'session' : [] | [Principal],
  /**
   * If present, the user has registered a new authentication method. This new authentication
   * method needs to be confirmed before 'expiration' in order to be added to the identity.
   */
  'authn_method' : [] | [AuthnMethodData],
}
export type AuthnMethodRegistrationModeEnterError = {
    'InvalidRegistrationId' : string
  } |
  { 'InternalCanisterError' : string } |
  { 'AlreadyInProgress' : null } |
  { 'Unauthorized' : Principal };
export type AuthnMethodRegistrationModeExitError = {
    'InternalCanisterError' : string
  } |
  { 'RegistrationModeOff' : null } |
  { 'Unauthorized' : Principal } |
  { 'InvalidMetadata' : string };
export type AuthnMethodReplaceError = {
    /**
     * No authentication method found with the given public key.
     */
    'AuthnMethodNotFound' : null
  } |
  { 'InvalidMetadata' : string };
export interface AuthnMethodSecuritySettings {
  'protection' : AuthnMethodProtection,
  'purpose' : AuthnMethodPurpose,
}
export type AuthnMethodSecuritySettingsReplaceError = {
    /**
     * No authentication method found with the given public key.
     */
    'AuthnMethodNotFound' : null
  };
export interface AuthnMethodSessionInfo { 'name' : [] | [string] }
export interface BufferedArchiveEntry {
  'sequence_number' : bigint,
  'entry' : Uint8Array | number[],
  'anchor_number' : UserNumber,
  'timestamp' : Timestamp,
}
/**
 * Captcha configuration
 * Default:
 * - max_unsolved_captchas: 500
 * - captcha_trigger: Static, CaptchaEnabled
 */
export interface CaptchaConfig {
  /**
   * Maximum number of unsolved captchas.
   */
  'max_unsolved_captchas' : bigint,
  /**
   * Configuration for when captcha protection should kick in.
   */
  'captcha_trigger' : {
      /**
       * Based on the rate of registrations compared to some reference time frame and allowing some leeway.
       */
      'Dynamic' : {
        /**
         * Length of the interval in seconds used to sample the reference rate of registrations.
         */
        'reference_rate_sampling_interval_s' : bigint,
        /**
         * Percentage of increased registration rate observed in the current rate sampling interval (compared to
         * reference rate) at which II will enable captcha for new registrations.
         */
        'threshold_pct' : number,
        /**
         * Length of the interval in seconds used to sample the current rate of registrations.
         */
        'current_rate_sampling_interval_s' : bigint,
      }
    } |
    {
      /**
       * Statically enable / disable captcha
       */
      'Static' : { 'CaptchaDisabled' : null } |
        { 'CaptchaEnabled' : null }
    },
}
export type CaptchaResult = ChallengeResult;
export interface Challenge {
  'png_base64' : string,
  'challenge_key' : ChallengeKey,
}
export type ChallengeKey = string;
export interface ChallengeResult { 'key' : ChallengeKey, 'chars' : string }
export interface CheckCaptchaArg { 'solution' : string }
export type CheckCaptchaError = {
    /**
     * No registration flow ongoing for the caller.
     */
    'NoRegistrationFlow' : null
  } |
  {
    /**
     * This call is unexpected, see next_step.
     */
    'UnexpectedCall' : { 'next_step' : RegistrationFlowNextStep }
  } |
  {
    /**
     * The supplied solution was wrong. Try again with the new captcha.
     */
    'WrongSolution' : { 'new_captcha_png_base64' : string }
  };
export type CreateAccountError = { 'AccountLimitReached' : null } |
  { 'InternalCanisterError' : string } |
  { 'Unauthorized' : Principal } |
  { 'NameTooLong' : null };
export type CredentialId = Uint8Array | number[];
export interface Delegation {
  'pubkey' : PublicKey,
  'targets' : [] | [Array<Principal>],
  'expiration' : Timestamp,
}
export type DeployArchiveResult = {
    /**
     * Initial archive creation is already in progress.
     */
    'creation_in_progress' : null
  } |
  {
    /**
     * The archive was deployed successfully and the supplied wasm module has been installed. The principal of the archive
     * canister is returned.
     */
    'success' : Principal
  } |
  {
    /**
     * Archive deployment failed. An error description is returned.
     */
    'failed' : string
  };
export interface DeviceData {
  'alias' : string,
  /**
   * Metadata map for additional device information.
   * 
   * Note: some fields above will be moved to the metadata map in the future.
   * All field names of `DeviceData` (such as 'alias', 'origin, etc.) are
   * reserved and cannot be written.
   * In addition, the keys "usage" and "authenticator_attachment" are reserved as well.
   */
  'metadata' : [] | [MetadataMap],
  'origin' : [] | [string],
  'protection' : DeviceProtection,
  'pubkey' : DeviceKey,
  'key_type' : KeyType,
  'purpose' : Purpose,
  'credential_id' : [] | [CredentialId],
}
export type DeviceKey = PublicKey;
export interface DeviceKeyWithAnchor {
  'pubkey' : DeviceKey,
  'anchor_number' : UserNumber,
}
/**
 * This describes whether a device is "protected" or not.
 * When protected, a device can only be updated or removed if the
 * user is authenticated with that very device.
 */
export type DeviceProtection = { 'unprotected' : null } |
  { 'protected' : null };
/**
 * Extra information about registration status for new devices
 */
export interface DeviceRegistrationInfo {
  /**
   * If present, the user has registered a new authentication method. This new authentication
   * method needs to be confirmed before 'expiration' in order to be added to the identity.
   */
  'tentative_device' : [] | [DeviceData],
  /**
   * The timestamp at which the anchor will turn off registration mode
   * (and the tentative device will be forgotten, if any, and if not verified)
   */
  'expiration' : Timestamp,
  /**
   * If present, the user has registered a new session. This new session needs to be confirmed before
   * 'expiration' in order for it be authorized to register an authentication method to the identity.
   */
  'tentative_session' : [] | [Principal],
}
/**
 * The same as `DeviceData` but with the `last_usage` field.
 * This field cannot be written, hence the separate type.
 */
export interface DeviceWithUsage {
  'alias' : string,
  'last_usage' : [] | [Timestamp],
  'metadata' : [] | [MetadataMap],
  'origin' : [] | [string],
  'protection' : DeviceProtection,
  'pubkey' : DeviceKey,
  'key_type' : KeyType,
  'purpose' : Purpose,
  'credential_id' : [] | [CredentialId],
}
export interface DummyAuthConfig {
  /**
   * Prompts user for a index value (0 - 255) when set to true,
   * this is used in e2e to have multiple dummy auth identities.
   */
  'prompt_for_index' : boolean,
}
export type FrontendHostname = string;
export type GetAccountsError = { 'InternalCanisterError' : string } |
  { 'Unauthorized' : Principal };
export type GetDefaultAccountError = {
    'NoSuchOrigin' : { 'anchor_number' : UserNumber }
  } |
  { 'NoSuchAnchor' : null } |
  { 'InternalCanisterError' : string } |
  { 'Unauthorized' : Principal };
export type GetDelegationResponse = {
    /**
     * The signature is not ready. Maybe retry by calling `prepare_delegation`
     */
    'no_such_delegation' : null
  } |
  {
    /**
     * The signed delegation was successfully retrieved.
     */
    'signed_delegation' : SignedDelegation
  };
export type GetIdAliasError = {
    /**
     * Internal canister error. See the error message for details.
     */
    'InternalCanisterError' : string
  } |
  {
    /**
     * The principal is not authorized to call this method with the given arguments.
     */
    'Unauthorized' : Principal
  } |
  {
    /**
     * The credential(s) are not available: may be expired or not prepared yet (call prepare_id_alias to prepare).
     */
    'NoSuchCredentials' : string
  };
/**
 * The request to retrieve the actual signed id alias credentials.
 * The field values should be equal to the values of corresponding
 * fields from the preceding `PrepareIdAliasRequest` and `PrepareIdAliasResponse`.
 */
export interface GetIdAliasRequest {
  'rp_id_alias_jwt' : string,
  'issuer' : FrontendHostname,
  'issuer_id_alias_jwt' : string,
  'relying_party' : FrontendHostname,
  'identity_number' : IdentityNumber,
}
export interface GoogleOpenIdConfig { 'client_id' : string }
export type HeaderField = [string, string];
export interface HttpRequest {
  'url' : string,
  'method' : string,
  'body' : Uint8Array | number[],
  'headers' : Array<HeaderField>,
  'certificate_version' : [] | [number],
}
export interface HttpResponse {
  'body' : Uint8Array | number[],
  'headers' : Array<HeaderField>,
  'upgrade' : [] | [boolean],
  'streaming_strategy' : [] | [StreamingStrategy],
  'status_code' : number,
}
/**
 * The signed id alias credentials for each involved party.
 */
export interface IdAliasCredentials {
  'rp_id_alias_credential' : SignedIdAlias,
  'issuer_id_alias_credential' : SignedIdAlias,
}
export interface IdRegFinishArg {
  'name' : [] | [string],
  'authn_method' : AuthnMethodData,
}
export type IdRegFinishError = {
    /**
     * No registration flow ongoing for the caller.
     */
    'NoRegistrationFlow' : null
  } |
  {
    /**
     * This call is unexpected, see next_step.
     */
    'UnexpectedCall' : { 'next_step' : RegistrationFlowNextStep }
  } |
  {
    /**
     * The supplied authn_method is not valid.
     */
    'InvalidAuthnMethod' : string
  } |
  {
    /**
     * The configured maximum number of identities has been reached.
     */
    'IdentityLimitReached' : null
  } |
  {
    /**
     * Error while persisting the new identity.
     */
    'StorageError' : string
  };
export interface IdRegFinishResult { 'identity_number' : bigint }
export interface IdRegNextStepResult {
  /**
   * The next step in the registration flow
   */
  'next_step' : RegistrationFlowNextStep,
}
export type IdRegStartError = {
    /**
     * The method was called anonymously, which is not supported.
     */
    'InvalidCaller' : null
  } |
  {
    /**
     * A registration flow is already in progress.
     */
    'AlreadyInProgress' : null
  } |
  {
    /**
     * Too many registrations. Please try again later.
     */
    'RateLimitExceeded' : null
  };
/**
 * Information about the anchor
 */
export interface IdentityAnchorInfo {
  /**
   * The name of the Internet Identity
   */
  'name' : [] | [string],
  /**
   * The timestamp at which the anchor was created
   */
  'created_at' : [] | [Timestamp],
  /**
   * All devices that can authenticate to this anchor
   */
  'devices' : Array<DeviceWithUsage>,
  /**
   * OpenID accounts linked to this anchor
   */
  'openid_credentials' : [] | [Array<OpenIdCredential>],
  /**
   * Device registration status used when adding devices, see DeviceRegistrationInfo
   */
  'device_registration' : [] | [DeviceRegistrationInfo],
}
export interface IdentityAuthnInfo {
  'authn_methods' : Array<AuthnMethod>,
  'recovery_authn_methods' : Array<AuthnMethod>,
}
export interface IdentityInfo {
  'authn_methods' : Array<AuthnMethodData>,
  /**
   * Authentication method independent metadata
   */
  'metadata' : MetadataMapV2,
  'name' : [] | [string],
  'authn_method_registration' : [] | [AuthnMethodRegistrationInfo],
  'openid_credentials' : [] | [Array<OpenIdCredential>],
}
export type IdentityInfoError = {
    /**
     * Internal canister error. See the error message for details.
     */
    'InternalCanisterError' : string
  } |
  {
    /**
     * The principal is not authorized to call this method with the given arguments.
     */
    'Unauthorized' : Principal
  };
export type IdentityMetadataReplaceError = {
    /**
     * Internal canister error. See the error message for details.
     */
    'InternalCanisterError' : string
  } |
  {
    /**
     * The principal is not authorized to call this method with the given arguments.
     */
    'Unauthorized' : Principal
  } |
  {
    /**
     * The identity including the new metadata exceeds the maximum allowed size.
     */
    'StorageSpaceExceeded' : {
      'space_required' : bigint,
      'space_available' : bigint,
    }
  };
/**
 * API V2 specific types
 * WARNING: These type are experimental and may change in the future.
 */
export type IdentityNumber = bigint;
export interface IdentityPropertiesReplace { 'name' : [] | [string] }
export type IdentityPropertiesReplaceError = {
    'InternalCanisterError' : string
  } |
  { 'Unauthorized' : Principal } |
  { 'NameTooLong' : { 'limit' : bigint } } |
  {
    'StorageSpaceExceeded' : {
      'space_required' : bigint,
      'space_available' : bigint,
    }
  };
/**
 * Init arguments of II which can be supplied on install and upgrade.
 * 
 * Each field is wrapped is `opt` to indicate whether the field should
 * keep the previous value or update to a new value (e.g. `null` keeps the previous value).
 * 
 * Some fields, like `openid_google`, have an additional nested `opt`, this indicates
 * enable/disable status (e.g. `opt null` disables a feature while `null` leaves it untouched).
 */
export interface InternetIdentityInit {
  /**
   * Configuration to fetch root key or not from frontend assets
   */
  'fetch_root_key' : [] | [boolean],
  /**
   * Configuration for OpenID Google client
   */
  'openid_google' : [] | [[] | [GoogleOpenIdConfig]],
  /**
   * Configuration to set the canister as production mode.
   * For now, this is used only to show or hide the banner.
   */
  'is_production' : [] | [boolean],
  /**
   * Configuration to show dapps explorer or not
   */
  'enable_dapps_explorer' : [] | [boolean],
  /**
   * Set lowest and highest anchor
   */
  'assigned_user_number_range' : [] | [[bigint, bigint]],
  /**
   * Configuration for New Origin Flows.
   * If present, list of origins using the new authentication flow.
   */
  'new_flow_origins' : [] | [Array<string>],
  /**
   * Configuration parameters related to the II archive.
   * Note: some parameters changes (like the polling interval) will only take effect after an archive deployment.
   * See ArchiveConfig for details.
   */
  'archive_config' : [] | [ArchiveConfig],
  /**
   * Set the amounts of cycles sent with the create canister message.
   * This is configurable because in the staging environment cycles are required.
   * The canister creation cost on mainnet is currently 100'000'000'000 cycles. If this value is higher thant the
   * canister creation cost, the newly created canister will keep extra cycles.
   */
  'canister_creation_cycles_cost' : [] | [bigint],
  /**
   * Configuration for Web Analytics
   */
  'analytics_config' : [] | [[] | [AnalyticsConfig]],
  /**
   * Configuration for Related Origins Requests.
   * If present, list of origins from where registration is allowed.
   */
  'related_origins' : [] | [Array<string>],
  /**
   * Feature flags
   */
  'feature_flag_continue_from_another_device' : [] | [boolean],
  /**
   * Configurations for OpenID clients
   */
  'openid_configs' : [] | [Array<OpenIdConfig>],
  /**
   * Configuration of the captcha in the registration flow.
   */
  'captcha_config' : [] | [CaptchaConfig],
  /**
   * Configuration for dummy authentication used in e2e tests.
   */
  'dummy_auth' : [] | [[] | [DummyAuthConfig]],
  /**
   * Rate limit for the `register` call.
   */
  'register_rate_limit' : [] | [RateLimitConfig],
}
export interface InternetIdentityStats {
  'storage_layout_version' : number,
  'users_registered' : bigint,
  'assigned_user_number_range' : [bigint, bigint],
  'archive_info' : ArchiveInfo,
  'canister_creation_cycles_cost' : bigint,
  /**
   * Map from event aggregation to a sorted list of top 100 sub-keys to their weights.
   * Example: {"prepare_delegation_count 24h ic0.app": [{"https://dapp.com", 100}, {"https://dapp2.com", 50}]}
   */
  'event_aggregations' : Array<[string, Array<[string, bigint]>]>,
}
/**
 * OpenID specific types
 */
export type Iss = string;
export type JWT = string;
export type KeyType = { 'platform' : null } |
  { 'seed_phrase' : null } |
  { 'cross_platform' : null } |
  { 'unknown' : null } |
  { 'browser_storage_key' : null };
export type LookupByRegistrationIdError = { 'InvalidRegistrationId' : string };
/**
 * Map with some variants for the value type.
 * Note, due to the Candid mapping this must be a tuple type thus we cannot name the fields `key` and `value`.
 */
export type MetadataMap = Array<
  [
    string,
    { 'map' : MetadataMap } |
      { 'string' : string } |
      { 'bytes' : Uint8Array | number[] },
  ]
>;
/**
 * Map with some variants for the value type.
 * Note, due to the Candid mapping this must be a tuple type thus we cannot name the fields `key` and `value`.
 */
export type MetadataMapV2 = Array<
  [
    string,
    { 'Map' : MetadataMapV2 } |
      { 'String' : string } |
      { 'Bytes' : Uint8Array | number[] },
  ]
>;
export interface OpenIDRegFinishArg {
  'jwt' : JWT,
  'name' : string,
  'salt' : Salt,
}
export interface OpenIdConfig {
  'auth_uri' : string,
  'jwks_uri' : string,
  'logo' : string,
  'name' : string,
  'fedcm_uri' : [] | [string],
  'issuer' : string,
  'auth_scope' : Array<string>,
  'client_id' : string,
}
export interface OpenIdCredential {
  'aud' : Aud,
  'iss' : Iss,
  'sub' : Sub,
  'metadata' : MetadataMapV2,
  'last_usage_timestamp' : [] | [Timestamp],
}
export type OpenIdCredentialAddError = {
    'OpenIdCredentialAlreadyRegistered' : null
  } |
  { 'InternalCanisterError' : string } |
  { 'JwtExpired' : null } |
  { 'Unauthorized' : Principal } |
  { 'JwtVerificationFailed' : null };
export type OpenIdCredentialKey = [Iss, Sub];
export type OpenIdCredentialRemoveError = { 'InternalCanisterError' : string } |
  { 'OpenIdCredentialNotFound' : null } |
  { 'Unauthorized' : Principal };
export type OpenIdDelegationError = { 'NoSuchDelegation' : null } |
  { 'NoSuchAnchor' : null } |
  { 'JwtExpired' : null } |
  { 'JwtVerificationFailed' : null };
export interface OpenIdPrepareDelegationResponse {
  'user_key' : UserKey,
  'expiration' : Timestamp,
  'anchor_number' : UserNumber,
}
export interface PrepareAccountDelegation {
  'user_key' : UserKey,
  'expiration' : Timestamp,
}
export type PrepareIdAliasError = {
    /**
     * Internal canister error. See the error message for details.
     */
    'InternalCanisterError' : string
  } |
  {
    /**
     * The principal is not authorized to call this method with the given arguments.
     */
    'Unauthorized' : Principal
  };
export interface PrepareIdAliasRequest {
  /**
   * Origin of the issuer in the attribute sharing flow.
   */
  'issuer' : FrontendHostname,
  /**
   * Origin of the relying party in the attribute sharing flow.
   */
  'relying_party' : FrontendHostname,
  /**
   * Identity for which the IdAlias should be generated.
   */
  'identity_number' : IdentityNumber,
}
/**
 * The prepared id alias contains two (still unsigned) credentials in JWT format,
 * certifying the id alias for the issuer resp. the relying party.
 */
export interface PreparedIdAlias {
  'rp_id_alias_jwt' : string,
  'issuer_id_alias_jwt' : string,
  'canister_sig_pk_der' : PublicKey,
}
export type PublicKey = Uint8Array | number[];
/**
 * Authentication method using generic signatures
 * See https://internetcomputer.org/docs/current/references/ic-interface-spec/#signatures for
 * supported signature schemes.
 */
export interface PublicKeyAuthn { 'pubkey' : PublicKey }
export type Purpose = { 'authentication' : null } |
  { 'recovery' : null };
/**
 * Rate limit configuration.
 * Currently only used for `register`.
 */
export interface RateLimitConfig {
  /**
   * How many tokens are at most generated (to accommodate peaks).
   */
  'max_tokens' : bigint,
  /**
   * Time it takes (in ns) for a rate limiting token to be replenished.
   */
  'time_per_token_ns' : bigint,
}
export type RegisterResponse = {
    /**
     * The challenge was not successful.
     */
    'bad_challenge' : null
  } |
  {
    /**
     * No more registrations are possible in this instance of the II service canister.
     */
    'canister_full' : null
  } |
  {
    /**
     * A new user was successfully registered.
     */
    'registered' : { 'user_number' : UserNumber }
  };
/**
 * The next step in the registration flow:
 * - CheckCaptcha: supply the solution to the captcha using `check_captcha`
 * - Finish: finish the registration using `identity_registration_finish`
 */
export type RegistrationFlowNextStep = {
    /**
     * Supply the captcha solution using check_captcha
     */
    'CheckCaptcha' : { 'captcha_png_base64' : string }
  } |
  {
    /**
     * Finish the registration using identity_registration_finish
     */
    'Finish' : null
  };
export type RegistrationId = string;
export type Salt = Uint8Array | number[];
export type SessionKey = PublicKey;
export type SetDefaultAccountError = {
    'NoSuchOrigin' : { 'anchor_number' : UserNumber }
  } |
  { 'NoSuchAnchor' : null } |
  { 'InternalCanisterError' : string } |
  { 'Unauthorized' : Principal } |
  {
    'NoSuchAccount' : {
      'origin' : FrontendHostname,
      'anchor_number' : UserNumber,
    }
  };
export interface SignedDelegation {
  'signature' : Uint8Array | number[],
  'delegation' : Delegation,
}
export interface SignedIdAlias {
  'credential_jws' : string,
  'id_alias' : Principal,
  'id_dapp' : Principal,
}
export interface StreamingCallbackHttpResponse {
  'token' : [] | [Token],
  'body' : Uint8Array | number[],
}
export type StreamingStrategy = {
    'Callback' : { 'token' : Token, 'callback' : [Principal, string] }
  };
export type Sub = string;
export type Timestamp = bigint;
export type Token = {};
export type UpdateAccountError = { 'AccountLimitReached' : null } |
  { 'InternalCanisterError' : string } |
  { 'Unauthorized' : Principal } |
  { 'NameTooLong' : null };
export type UserKey = PublicKey;
export type UserNumber = bigint;
export type VerifyTentativeDeviceResponse = {
    /**
     * Device registration mode is off, either due to timeout or because it was never enabled.
     */
    'device_registration_mode_off' : null
  } |
  {
    /**
     * The device was successfully verified.
     */
    'verified' : null
  } |
  {
    /**
     * Wrong verification code entered. Retry with correct code.
     */
    'wrong_code' : { 'retries_left' : number }
  } |
  {
    /**
     * There is no tentative device to be verified.
     */
    'no_device_to_verify' : null
  };
/**
 * Authentication method using WebAuthn signatures
 * See https://www.w3.org/TR/webauthn-2/
 * This is a separate type because WebAuthn requires to also store
 * the credential id (in addition to the public key).
 */
export interface WebAuthn {
  'pubkey' : PublicKey,
  'credential_id' : CredentialId,
}
export interface WebAuthnCredential {
  'pubkey' : PublicKey,
  'credential_id' : CredentialId,
}
export interface _SERVICE {
  'acknowledge_entries' : ActorMethod<[bigint], undefined>,
  'add' : ActorMethod<[UserNumber, DeviceData], undefined>,
  'add_tentative_device' : ActorMethod<
    [UserNumber, DeviceData],
    AddTentativeDeviceResponse
  >,
  /**
   * Adds a new authentication method to the identity.
   * Requires authentication.
   */
  'authn_method_add' : ActorMethod<
    [IdentityNumber, AuthnMethodData],
    { 'Ok' : null } |
      { 'Err' : AuthnMethodAddError }
  >,
  /**
   * Confirms a previously registered authentication method.
   * On successful confirmation, the authentication method is permanently added to the identity and can
   * subsequently be used for authentication for that identity.
   * Requires authentication.
   */
  'authn_method_confirm' : ActorMethod<
    [IdentityNumber, string],
    { 'Ok' : null } |
      { 'Err' : AuthnMethodConfirmationError }
  >,
  /**
   * Replaces the authentication method metadata map.
   * The existing metadata map will be overwritten.
   * Requires authentication.
   */
  'authn_method_metadata_replace' : ActorMethod<
    [IdentityNumber, PublicKey, MetadataMapV2],
    { 'Ok' : null } |
      { 'Err' : AuthnMethodMetadataReplaceError }
  >,
  /**
   * Registers a new authentication method to the identity.
   * This authentication method needs to be confirmed before it can be used for authentication on this identity.
   */
  'authn_method_register' : ActorMethod<
    [IdentityNumber, AuthnMethodData],
    { 'Ok' : AuthnMethodConfirmationCode } |
      { 'Err' : AuthnMethodRegisterError }
  >,
  /**
   * Enters the authentication method registration mode for the identity.
   * In this mode, a new authentication method can be registered, which then needs to be
   * confirmed before it can be used for authentication on this identity.
   * The registration mode is automatically exited after the returned expiration timestamp.
   * Requires authentication.
   */
  'authn_method_registration_mode_enter' : ActorMethod<
    [IdentityNumber, [] | [RegistrationId]],
    { 'Ok' : { 'expiration' : Timestamp } } |
      { 'Err' : AuthnMethodRegistrationModeEnterError }
  >,
  /**
   * Exits the authentication method registration mode for the identity.
   * Requires authentication.
   */
  'authn_method_registration_mode_exit' : ActorMethod<
    [IdentityNumber, [] | [AuthnMethodData]],
    { 'Ok' : null } |
      { 'Err' : AuthnMethodRegistrationModeExitError }
  >,
  /**
   * Removes the authentication method associated with the public key from the identity.
   * Requires authentication.
   */
  'authn_method_remove' : ActorMethod<
    [IdentityNumber, PublicKey],
    { 'Ok' : null } |
      { 'Err' : null }
  >,
  /**
   * Atomically replaces the authentication method matching the supplied public key with the new authentication method
   * provided.
   * Requires authentication.
   */
  'authn_method_replace' : ActorMethod<
    [IdentityNumber, PublicKey, AuthnMethodData],
    { 'Ok' : null } |
      { 'Err' : AuthnMethodReplaceError }
  >,
  /**
   * Replaces the authentication method security settings.
   * The existing security settings will be overwritten.
   * Requires authentication.
   */
  'authn_method_security_settings_replace' : ActorMethod<
    [IdentityNumber, PublicKey, AuthnMethodSecuritySettings],
    { 'Ok' : null } |
      { 'Err' : AuthnMethodSecuritySettingsReplaceError }
  >,
  /**
   * Returns session info when session is confirmed and caller matches session.
   */
  'authn_method_session_info' : ActorMethod<
    [IdentityNumber],
    [] | [AuthnMethodSessionInfo]
  >,
  /**
   * Registers a new session for the identity.
   * This session needs to be confirmed before it can be used to register an authentication method on this identity.
   */
  'authn_method_session_register' : ActorMethod<
    [IdentityNumber],
    { 'Ok' : AuthnMethodConfirmationCode } |
      { 'Err' : AuthnMethodRegisterError }
  >,
  /**
   * Check the captcha challenge
   * If successful, the registration can be finished with `identity_registration_finish`.
   */
  'check_captcha' : ActorMethod<
    [CheckCaptchaArg],
    { 'Ok' : IdRegNextStepResult } |
      { 'Err' : CheckCaptchaError }
  >,
  'config' : ActorMethod<[], InternetIdentityInit>,
  'create_account' : ActorMethod<
    [UserNumber, FrontendHostname, string],
    { 'Ok' : AccountInfo } |
      { 'Err' : CreateAccountError }
  >,
  /**
   * Legacy identity management API
   * ==============================
   */
  'create_challenge' : ActorMethod<[], Challenge>,
  'deploy_archive' : ActorMethod<[Uint8Array | number[]], DeployArchiveResult>,
  'enter_device_registration_mode' : ActorMethod<[UserNumber], Timestamp>,
  'exit_device_registration_mode' : ActorMethod<[UserNumber], undefined>,
  /**
   * Returns a batch of entries _sorted by sequence number_ to be archived.
   * This is an update call because the archive information _must_ be certified.
   * Only callable by this IIs archive canister.
   */
  'fetch_entries' : ActorMethod<[], Array<BufferedArchiveEntry>>,
  'get_account_delegation' : ActorMethod<
    [UserNumber, FrontendHostname, [] | [AccountNumber], SessionKey, Timestamp],
    { 'Ok' : SignedDelegation } |
      { 'Err' : AccountDelegationError }
  >,
  /**
   * Multiple accounts
   */
  'get_accounts' : ActorMethod<
    [UserNumber, FrontendHostname],
    { 'Ok' : Array<AccountInfo> } |
      { 'Err' : GetAccountsError }
  >,
  'get_anchor_credentials' : ActorMethod<[UserNumber], AnchorCredentials>,
  'get_anchor_info' : ActorMethod<[UserNumber], IdentityAnchorInfo>,
  'get_default_account' : ActorMethod<
    [UserNumber, FrontendHostname],
    { 'Ok' : AccountInfo } |
      { 'Err' : GetDefaultAccountError }
  >,
  'get_delegation' : ActorMethod<
    [UserNumber, FrontendHostname, SessionKey, Timestamp],
    GetDelegationResponse
  >,
  'get_id_alias' : ActorMethod<
    [GetIdAliasRequest],
    { 'Ok' : IdAliasCredentials } |
      { 'Err' : GetIdAliasError }
  >,
  'get_principal' : ActorMethod<[UserNumber, FrontendHostname], Principal>,
  /**
   * HTTP Gateway protocol
   * =====================
   */
  'http_request' : ActorMethod<[HttpRequest], HttpResponse>,
  /**
   * Returns information about the authentication methods of the identity with the given number.
   * Only returns the minimal information required for authentication without exposing any metadata such as aliases.
   */
  'identity_authn_info' : ActorMethod<
    [IdentityNumber],
    { 'Ok' : IdentityAuthnInfo } |
      { 'Err' : null }
  >,
  /**
   * Returns information about the identity with the given number.
   * Requires authentication.
   */
  'identity_info' : ActorMethod<
    [IdentityNumber],
    { 'Ok' : IdentityInfo } |
      { 'Err' : IdentityInfoError }
  >,
  /**
   * Replaces the authentication method independent metadata map.
   * The existing metadata map will be overwritten.
   * Requires authentication.
   */
  'identity_metadata_replace' : ActorMethod<
    [IdentityNumber, MetadataMapV2],
    { 'Ok' : null } |
      { 'Err' : IdentityMetadataReplaceError }
  >,
  /**
   * Replaces the identity properties.
   * The existing properties will be overwritten.
   * Requires authentication.
   */
  'identity_properties_replace' : ActorMethod<
    [IdentityNumber, IdentityPropertiesReplace],
    { 'Ok' : null } |
      { 'Err' : IdentityPropertiesReplaceError }
  >,
  /**
   * Starts the identity registration flow to create a new identity.
   */
  'identity_registration_finish' : ActorMethod<
    [IdRegFinishArg],
    { 'Ok' : IdRegFinishResult } |
      { 'Err' : IdRegFinishError }
  >,
  /**
   * V2 Identity Management API
   * ==========================
   * WARNING: The following methods are experimental and may ch 0ange in the future.
   * Starts the identity registration flow to create a new identity.
   */
  'identity_registration_start' : ActorMethod<
    [],
    { 'Ok' : IdRegNextStepResult } |
      { 'Err' : IdRegStartError }
  >,
  /**
   * Internal Methods
   * ================
   */
  'init_salt' : ActorMethod<[], undefined>,
  /**
   * Returns all devices of the user (authentication and recovery) but no information about device registrations.
   * Note: Clears out the 'alias' fields on the devices. Use 'get_anchor_info' to obtain the full information.
   * Deprecated: Use 'get_anchor_credentials' instead.
   */
  'lookup' : ActorMethod<[UserNumber], Array<DeviceData>>,
  'lookup_by_registration_mode_id' : ActorMethod<
    [RegistrationId],
    [] | [IdentityNumber]
  >,
  /**
   * Discoverable passkeys protocol
   */
  'lookup_device_key' : ActorMethod<
    [Uint8Array | number[]],
    [] | [DeviceKeyWithAnchor]
  >,
  'openid_credential_add' : ActorMethod<
    [IdentityNumber, JWT, Salt],
    { 'Ok' : null } |
      { 'Err' : OpenIdCredentialAddError }
  >,
  'openid_credential_remove' : ActorMethod<
    [IdentityNumber, OpenIdCredentialKey],
    { 'Ok' : null } |
      { 'Err' : OpenIdCredentialRemoveError }
  >,
  'openid_get_delegation' : ActorMethod<
    [JWT, Salt, SessionKey, Timestamp],
    { 'Ok' : SignedDelegation } |
      { 'Err' : OpenIdDelegationError }
  >,
  /**
   * OpenID credentials protocol
   * =========================
   */
  'openid_identity_registration_finish' : ActorMethod<
    [OpenIDRegFinishArg],
    { 'Ok' : IdRegFinishResult } |
      { 'Err' : IdRegFinishError }
  >,
  'openid_prepare_delegation' : ActorMethod<
    [JWT, Salt, SessionKey],
    { 'Ok' : OpenIdPrepareDelegationResponse } |
      { 'Err' : OpenIdDelegationError }
  >,
  'prepare_account_delegation' : ActorMethod<
    [
      UserNumber,
      FrontendHostname,
      [] | [AccountNumber],
      SessionKey,
      [] | [bigint],
    ],
    { 'Ok' : PrepareAccountDelegation } |
      { 'Err' : AccountDelegationError }
  >,
  /**
   * Authentication protocol
   * =======================
   */
  'prepare_delegation' : ActorMethod<
    [UserNumber, FrontendHostname, SessionKey, [] | [bigint]],
    [UserKey, Timestamp]
  >,
  /**
   * Attribute Sharing MVP API
   * =========================
   * The methods below are used to generate ID-alias credentials during attribute sharing flow.
   */
  'prepare_id_alias' : ActorMethod<
    [PrepareIdAliasRequest],
    { 'Ok' : PreparedIdAlias } |
      { 'Err' : PrepareIdAliasError }
  >,
  'register' : ActorMethod<
    [DeviceData, ChallengeResult, [] | [Principal]],
    RegisterResponse
  >,
  'remove' : ActorMethod<[UserNumber, DeviceKey], undefined>,
  /**
   * Atomically replace device matching the device key with the new device data
   */
  'replace' : ActorMethod<[UserNumber, DeviceKey, DeviceData], undefined>,
  'set_default_account' : ActorMethod<
    [UserNumber, FrontendHostname, [] | [AccountNumber]],
    { 'Ok' : AccountInfo } |
      { 'Err' : SetDefaultAccountError }
  >,
  'stats' : ActorMethod<[], InternetIdentityStats>,
  'update' : ActorMethod<[UserNumber, DeviceKey, DeviceData], undefined>,
  'update_account' : ActorMethod<
    [UserNumber, FrontendHostname, [] | [AccountNumber], AccountUpdate],
    { 'Ok' : AccountInfo } |
      { 'Err' : UpdateAccountError }
  >,
  'verify_tentative_device' : ActorMethod<
    [UserNumber, string],
    VerifyTentativeDeviceResponse
  >,
}
export declare const idlFactory: IDL.InterfaceFactory;
export declare const init: (args: { IDL: typeof IDL }) => IDL.Type[];
