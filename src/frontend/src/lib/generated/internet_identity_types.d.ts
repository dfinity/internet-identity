import type { Principal } from '@icp-sdk/core/principal';
import type { ActorMethod } from '@icp-sdk/core/agent';
import type { IDL } from '@icp-sdk/core/candid';

export type Aaguid = Uint8Array | number[];
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
     * Passkey with this public key is already used
     */
    'passkey_with_this_public_key_is_already_used' : null
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
/**
 * A specification of an attribute to be certified.
 */
export interface AttributeSpec {
  /**
   * `<attribute_scope>:<attribute_name>` where `<attribute_scope>` is
   * either `openid:<issuer>` or `sso:<domain>`.
   * 
   * Examples:
   * - `openid:https://accounts.google.com:email`
   * - `sso:dfinity.org:email`
   */
  'key' : string,
  /**
   * If `value` is set, this attribute should be certified only if it matches
   * the current value. This enables the II frontend to request a certificate
   * for a set of attributes that is guaranteed to be approved by the user.
   */
  'value' : [] | [Uint8Array | number[]],
  /**
   * Whether the attribute scope should be omitted before certification.
   * Certifying unscoped attributes is useful, e.g., when the user wants
   * to share their preferred email without revealing which OpenID
   * provider they use with II.
   * 
   * Examples (e.g., `key = "openid:https://accounts.google.com:email"`):
   * 1. If `omit_scope = true`, then the certified attribute key is `email`.
   * 2. Otherwise, the certified attribute key is literally the value of `key`.
   */
  'omit_scope' : boolean,
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
     * Passkey with this public key is already used
     */
    'PasskeyWithThisPublicKeyIsAlreadyUsed' : null
  } |
  {
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
     * The caller's principal is not self-authenticating.
     */
    'NotSelfAuthenticating' : Principal
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
    'PasskeyWithThisPublicKeyIsAlreadyUsed' : null
  } |
  { 'InternalCanisterError' : string } |
  { 'RegistrationModeOff' : null } |
  { 'Unauthorized' : Principal } |
  { 'InvalidMetadata' : string };
export type AuthnMethodReplaceError = {
    /**
     * Passkey with this public key is already used
     */
    'PasskeyWithThisPublicKeyIsAlreadyUsed' : null
  } |
  {
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
export interface AuthnMethodSessionInfo {
  'name' : [] | [string],
  'created_at' : [] | [Timestamp],
}
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
export interface CertifiedAttribute {
  'key' : string,
  'signature' : Uint8Array | number[],
  'value' : Uint8Array | number[],
}
export interface CertifiedAttributes {
  'expires_at_timestamp_ns' : Timestamp,
  'certified_attributes' : Array<CertifiedAttribute>,
}
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
  /**
   * Restricts the kinds of calls the delegation permits: `"queries"`
   * restricts the sender to query calls (the IC rejects update calls
   * authenticated through such a delegation). Absent means unrestricted.
   */
  'permissions' : [] | [string],
  'pubkey' : PublicKey,
  'targets' : [] | [Array<Principal>],
  'expiration' : Timestamp,
}
export interface DelegationChain { 'links' : Array<DelegationLink> }
export interface DelegationLink {
  'child_dnskey' : SignedRRset,
  'child_ds' : SignedRRset,
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
  'aaguid' : [] | [Aaguid],
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
  'aaguid' : [] | [Aaguid],
  'purpose' : Purpose,
  'credential_id' : [] | [CredentialId],
}
export interface DnsProofBundle {
  'root_dnskey' : SignedRRset,
  /**
   * The RRsets being authenticated, in CNAME-resolution order.
   * Single-leaf case: one hop. CNAME case: intermediate CNAMEs,
   * then the final TXT.
   */
  'hops' : Array<SignedRRset>,
  /**
   * One delegation chain per signing zone the bundle touches.
   * Single-zone direct case (Gmail, iCloud, …): one chain.
   * Cross-zone CNAME case (Proton, Tutanota, M365 custom domains):
   * one chain per signing zone touched.
   */
  'chains' : Array<DelegationChain>,
}
/**
 * DNSSEC trust-anchor list. Any feature that needs DNSSEC-verified DNS
 * records consumes the same anchors; not specific to email recovery.
 */
export interface DnssecConfig {
  /**
   * IANA root KSK trust anchors. Multiple are accepted simultaneously so
   * KSK rollover is a single config change in the next upgrade arg.
   */
  'root_anchors' : Array<DnssecRootAnchor>,
}
/**
 * One IANA root KSK trust anchor, in the same shape IANA publishes at
 * `data.iana.org/root-anchors/root-anchors.xml`. Only `digest_type = 2`
 * (SHA-256) is accepted; the legacy SHA-1 form is rejected at the
 * verifier boundary.
 */
export interface DnssecRootAnchor {
  'algorithm' : number,
  'key_tag' : number,
  'digest_type' : number,
  'digest' : Uint8Array | number[],
}
/**
 * DoH (DNS-over-HTTPS) fallback configuration.
 * 
 * The canister will only fetch DKIM/DMARC TXT records via HTTP outcalls
 * for an FQDN whose registered domain is in `allowed_domains`. Cache
 * entries are populated on demand and re-used until `max_cache_age_secs`
 * elapses (default 3600s when null, capped at 24h).
 */
export interface DohConfig {
  'max_cache_age_secs' : [] | [bigint],
  'allowed_domains' : Array<string>,
}
/**
 * Why a DoH resolution failed, as a typed discriminant rather than a
 * free-form string. The FE reads this directly to segment the
 * `doh_reason` analytics property — no string parsing.
 */
export type DohFailureReason = { 'AllProvidersFailed' : null } |
  { 'ResponseMalformed' : string } |
  { 'QuorumFailed' : { 'total' : number, 'agreeing' : number } };
export interface DummyAuthConfig {
  /**
   * Prompts user for a index value (0 - 255) when set to true,
   * this is used in e2e to have multiple dummy auth identities.
   */
  'prompt_for_index' : boolean,
}
export interface EmailChallenge { 'nonce' : string, 'expires_at' : Timestamp }
/**
 * Strictly-public, user-copyable diagnostics for one pending challenge
 * (see email_challenge_diagnostics). Intended for a support ticket so a
 * case can be lined up across the SMTP gateway logs and the canister's
 * production logs via message_id. NO email address, anchor, principal,
 * delegation/seed, or inner error string — reason_code is the failing
 * variant's name only.
 */
export interface EmailChallengeDiagnostics {
  'created_at' : Timestamp,
  'verification_path' : VerificationPath,
  'message_id' : [] | [string],
  'reason_code' : string,
}
export interface EmailChallengeDnsInput {
  'dns_proof' : [] | [DnsProofBundle],
  'address' : string,
}
/**
 * Shared by both flows (recovery + verified emails). The variants
 * describe inbound-DKIM-challenge failure modes; none are
 * recovery-specific.
 */
export type EmailChallengeError = { 'EmailVerificationFailed' : string } |
  { 'DkimLeafMismatch' : null } |
  { 'InternalCanisterError' : string } |
  { 'NonceUnknown' : null } |
  { 'DohFetchFailed' : DohFailureReason } |
  { 'NoDkimLeafExpected' : null } |
  {
    /**
     * The anchor has reached its per-bucket cap (currently only fires
     * for the verified-emails bucket). FE shows a "limit reached"
     * notice; user must remove an existing entry to add another.
     */
    'LimitReached' : { 'limit' : number }
  } |
  { 'DomainNotSupported' : string } |
  { 'AddressNotRegistered' : null } |
  {
    /**
     * email_challenge_submit_dkim_leaf was called with an empty `hops`
     * vector; an FE that can't walk DNSSEC must drive
     * email_challenge_resolve_via_doh instead.
     */
    'EmptyDkimLeafHops' : null
  } |
  { 'Unauthorized' : Principal } |
  { 'NonceExpired' : null } |
  { 'AddressMismatch' : null } |
  {
    /**
     * The submitted address didn't pass shape validation (missing `@`,
     * empty local-part or domain, whitespace, oversized parts). Distinct
     * from DomainNotSupported, which is about a valid address whose
     * registered domain can't be verified.
     */
    'InvalidEmailAddress' : string
  } |
  { 'DomainNotAllowlisted' : string } |
  { 'SubjectNotSigned' : null } |
  { 'AddressAlreadyRegistered' : null };
/**
 * Argument to email_challenge_resolve_via_doh. Wrapped in a record (like
 * EmailChallengeSubmitDkimLeafArg) so the method can grow fields without a
 * breaking interface change; nonce is the lookup key and is always
 * required.
 */
export interface EmailChallengeResolveViaDohArg { 'nonce' : string }
/**
 * Polling status returned by `email_challenge_status` — shared between
 * the recovery flow and the verified-emails flow. `RegistrationSucceeded`
 * covers both "recovery email bound" and "verified email bound";
 * `RecoveryReady` is recovery-only and only emitted when the pending
 * entry's `PendingKind` is `Recover`.
 */
export type EmailChallengeStatus = { 'Failed' : EmailChallengeError } |
  { 'ResolvingDoh' : null } |
  { 'NeedDkimLeaf' : { 'selector' : string } } |
  {
    'RecoveryReady' : {
      'user_key' : UserKey,
      'expiration' : Timestamp,
      'anchor_number' : IdentityNumber,
    }
  } |
  { 'RegistrationSucceeded' : null } |
  { 'Expired' : null } |
  { 'Pending' : null };
/**
 * Used by both the recovery flow and the verified-emails flow. The
 * pending entry's `PendingKind` dispatches to the right anchor sink
 * after the cryptographic check; the argument itself is flow-neutral.
 */
export interface EmailChallengeSubmitDkimLeafArg {
  /**
   * Delegation chains for signed zones touched by `hops` that
   * weren't already covered by the skeleton chain anchored at
   * prepare time. Empty for same-zone resolution.
   */
  'extra_chains' : Array<DelegationChain>,
  /**
   * The DKIM resolution chain in CNAME order, ending in a TXT. At
   * least one hop required; bounded by `MAX_CNAME_HOPS = 4` at the
   * canister side. For the Gmail-style direct-TXT case this is a
   * single-element vec.
   * 
   * When the FE cannot walk a fully-signed DNSSEC resolution for the
   * leaf — the DKIM record CNAMEs into an unsigned zone (e.g.
   * `selector1._domainkey.outlook.com` is a signed CNAME into the
   * unsigned `outbound.protection.outlook.com`) — it must NOT submit
   * an empty vec here; it drives `email_challenge_resolve_via_doh`
   * instead, which resolves the key over the canister's DoH path.
   */
  'hops' : Array<SignedRRset>,
  'nonce' : string,
}
/**
 * Email-recovery types
 * ====================
 * See `docs/ongoing/email-recovery.md` for the full design. Covers
 * both halves of the flow: setup (binding a recovery email to an
 * anchor) and recovery (proving control of a previously-bound
 * address to obtain a signed delegation).
 */
export interface EmailRecoveryCredential {
  'created_at' : Timestamp,
  'address' : string,
  'last_used' : [] | [Timestamp],
}
export interface EmailRecoveryGetDelegationArgs {
  'session_key' : SessionKey,
  'expiration' : Timestamp,
  'nonce' : string,
}
export type FrontendHostname = string;
export type GetAccountError = {
    'NoSuchOrigin' : { 'anchor_number' : UserNumber }
  } |
  {
    'NoSuchAccount' : {
      'origin' : FrontendHostname,
      'anchor_number' : UserNumber,
    }
  };
export type GetAccountsError = { 'InternalCanisterError' : string } |
  { 'Unauthorized' : Principal };
export type GetAttributesError = { 'AuthorizationError' : Principal } |
  { 'ValidationError' : { 'problems' : Array<string> } } |
  { 'GetAccountError' : GetAccountError };
export interface GetAttributesRequest {
  /**
   * Origin of the relying party in the attribute sharing flow.
   */
  'origin' : FrontendHostname,
  /**
   * II account for the relying party.
   */
  'account_number' : [] | [AccountNumber],
  /**
   * The attribute to be retrieved, must be a subset of certified_attributes from
   * the prepare_attributes response.
   */
  'attributes' : Array<[string, Uint8Array | number[]]>,
  /**
   * Timestamp received from the prepare_attributes call.
   */
  'issued_at_timestamp_ns' : Timestamp,
  /**
   * Identity for which the attributes should be prepared.
   */
  'identity_number' : IdentityNumber,
}
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
export type GetIcrc3AttributeError = { 'AuthorizationError' : Principal } |
  { 'NoSuchSignature' : null } |
  { 'ValidationError' : { 'problems' : Array<string> } } |
  { 'GetAccountError' : GetAccountError };
export interface GetIcrc3AttributeRequest {
  /**
   * Origin of the relying party in the attribute sharing flow.
   */
  'origin' : FrontendHostname,
  /**
   * II account for the relying party.
   */
  'account_number' : [] | [AccountNumber],
  /**
   * The message blob from PrepareIcrc3AttributeResponse.
   */
  'message' : Uint8Array | number[],
  /**
   * Identity for which the attributes were prepared.
   */
  'identity_number' : IdentityNumber,
}
export interface GetIcrc3AttributeResponse {
  'signature' : Uint8Array | number[],
}
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
/**
 * Request for `get_sso_discovery_status`.
 */
export interface GetSsoDiscoveryStatusRequest {
  'target_app_origin' : [] | [FrontendHostname],
  'org_domain' : string,
}
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
  'status_code' : number,
}
/**
 * ICRC-3 attribute sharing types
 * ==============================
 */
export type Icrc3Value = { 'Int' : bigint } |
  { 'Map' : Array<[string, Icrc3Value]> } |
  { 'Nat' : bigint } |
  { 'Blob' : Uint8Array | number[] } |
  { 'Text' : string } |
  { 'Array' : Array<Icrc3Value> };
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
     * A gated SSO login for a non-`sub` org: the user must first sign in
     * through the org's primary client.
     */
    'SsoNormalLoginRequired' : null
  } |
  {
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
   * Verified emails bound to this anchor (absent when none is
   * configured). Capped at MAX_VERIFIED_EMAILS_PER_ANCHOR; the FE
   * shows a "limit reached" notice in the wizard when adding
   * beyond the cap.
   */
  'verified_emails' : [] | [Array<VerifiedEmail>],
  /**
   * Authentication method independent metadata
   */
  'metadata' : MetadataMapV2,
  'name' : [] | [string],
  /**
   * Email-recovery credentials bound to this anchor (absent when
   * none is configured). The canister API currently caps the list
   * at one entry — the FE renders the recovery-email card from
   * the first one — but exposing it as a `vec` lets future
   * multi-credential support land without a candid schema bump.
   */
  'email_recovery' : [] | [Array<EmailRecoveryCredential>],
  /**
   * The timestamp at which the anchor was created
   */
  'created_at' : [] | [Timestamp],
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
 * Some fields, like `analytics_config`, have an additional nested `opt`, this indicates
 * enable/disable status (e.g. `opt null` disables a feature while `null` leaves it untouched).
 */
export interface InternetIdentityInit {
  /**
   * DoH (DNS-over-HTTPS) fallback configuration. Allowlists the
   * domains for which the canister may fetch DKIM/DMARC TXT records
   * via HTTP outcalls when no DNSSEC chain is available — see
   * `docs/ongoing/email-recovery.md` §7.6. Same set/clear pattern.
   */
  'doh_config' : [] | [[] | [DohConfig]],
  /**
   * One-shot backfill of the `sso_domain` / `sso_name` fields on stored
   * OpenID credentials. When set, a batched timer-driven migration stamps
   * every stored credential whose (iss, aud) matches an entry and whose
   * `sso_domain` is not set yet. Idempotent — already-stamped credentials
   * are skipped, so re-submitting (e.g. with a corrected list) is safe.
   * When unset, no backfill runs.
   */
  'sso_credential_migration' : [] | [Array<SsoCredentialMigrationEntry>],
  /**
   * Configuration to set the canister as production mode.
   * For now, this is used only to show or hide the banner.
   */
  'is_production' : [] | [boolean],
  /**
   * Backend canister ID, needed for backward compatibility.
   */
  'backend_canister_id' : [] | [Principal],
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
   * DNSSEC verification configuration. Trust anchors used by any feature
   * that verifies DNS records against the IANA-rooted DNSSEC chain
   * (currently the email-recovery DKIM/DMARC flow). See
   * `docs/ongoing/email-recovery.md` §7.5.
   * 
   * Wrapped in `opt opt` to match the same set/clear pattern as
   * `analytics_config` / `dummy_auth`: outer null keeps the previously
   * stored value across an upgrade, `opt null` clears it, `opt opt c`
   * sets it to `c`.
   */
  'dnssec_config' : [] | [[] | [DnssecConfig]],
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
   * Deploy flag for the legacy DNSSEC email-recovery path. Defaults to
   * off (DoH-only); `opt true` re-enables it.
   */
  'enable_dnssec_email_recovery' : [] | [boolean],
  /**
   * Configuration for Related Origins Requests.
   * If present, list of origins from where registration is allowed.
   */
  'related_origins' : [] | [Array<string>],
  /**
   * Configurations for OpenID clients
   */
  'openid_configs' : [] | [Array<OpenIdConfig>],
  /**
   * Backend origin, needed to sync configuration with frontend.
   */
  'backend_origin' : [] | [string],
  /**
   * Configuration of the captcha in the registration flow.
   */
  'captcha_config' : [] | [CaptchaConfig],
  /**
   * Configuration for dummy authentication used in e2e tests.
   */
  'dummy_auth' : [] | [[] | [DummyAuthConfig]],
  /**
   * Deploy flag relaxing the `https` requirement for SSO discovery outcalls to
   * loopback hosts (`localhost` / `127.0.0.1`) so e2e tests can point at local
   * mock IdPs served over plain `http`. Unset / `false` (the default) require
   * `https` for every discovery host. Never enable in production — non-loopback
   * hosts always require `https` regardless.
   */
  'sso_allow_insecure_discovery' : [] | [boolean],
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
export type ListAvailableAttributesError = {
    'AuthorizationError' : Principal
  } |
  { 'ValidationError' : { 'problems' : Array<string> } };
/**
 * List available attributes
 */
export interface ListAvailableAttributesRequest {
  /**
   * Optional list of attribute keys to filter by.
   * Each key is either a fully-scoped key (e.g.,
   * `"openid:https://accounts.google.com:email"` or
   * `"sso:dfinity.org:email"`) or an unscoped attribute name (e.g.,
   * `"email"`) which matches all scopes.
   * If not provided, all available attributes are returned.
   */
  'attributes' : [] | [Array<string>],
  /**
   * Identity for which available attributes should be listed.
   */
  'identity_number' : IdentityNumber,
}
export type ListAvailableAttributesResponse = Array<
  [string, Uint8Array | number[]]
>;
export type LookupByRegistrationIdError = { 'InvalidRegistrationId' : string };
/**
 * The identity's synced trusted-MCP-server configuration: a master toggle and
 * the single MCP server URL the user trusts. Persisted on-chain (keyed by
 * anchor), so it follows the identity across all of its devices — unlike the
 * device-local CLI-access toggle. `url` is kept verbatim so the Settings UI can
 * display/re-probe a path-based endpoint; the connect flow matches trust by
 * origin.
 */
export interface McpConfig { 'url' : [] | [string], 'enabled' : boolean }
/**
 * Result of mcp_prepare_delegation. Carries the account_number the canister
 * used (the one named in the request, or the anchor's default account at
 * target_origin when none was named) so the MCP server can thread the same
 * account into mcp_get_delegation — the default is mutable, so re-resolving
 * it in `get` could otherwise diverge and yield NoSuchDelegation.
 */
export interface McpPrepareDelegation {
  'user_key' : UserKey,
  'account_number' : [] | [AccountNumber],
  'expiration' : Timestamp,
}
/**
 * Result of mcp_register_v2: the expiration (ns since epoch) of the MCP session
 * grant, plus the access level the user chose at connect (queries = read-only,
 * all = full). The server reads permissions to learn the read-only state up
 * front (the v2 flow has no completion POST carrying it).
 */
export interface McpRegistrationV2 {
  'permissions' : Permissions,
  'expiration' : Timestamp,
}
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
  /**
   * Target dapp origin, set only for a first gated SSO login.
   */
  'origin' : [] | [string],
  'salt' : Salt,
  /**
   * SSO discovery domain the JWT was obtained through, or null for a direct
   * provider (Google / Microsoft / Apple). Selects the JWK source.
   */
  'discovery_domain' : [] | [string],
}
export interface OpenIdConfig {
  'auth_uri' : string,
  'jwks_uri' : string,
  'logo' : string,
  'name' : string,
  'fedcm_uri' : [] | [string],
  'email_verification' : [] | [OpenIdEmailVerification],
  'issuer' : string,
  'auth_scope' : Array<string>,
  /**
   * Optional initial set of JWKs used to seed this provider's JWK cache on
   * install, so JWT verification works before the first jwks_uri fetch and
   * across upgrades (the cache is persisted in stable memory). The outer vec
   * is the set of JWKs; each inner vec is one JWK, given as the list of its
   * JSON (field, value) pairs, e.g. a single RSA key is
   * vec { vec { record { "kty"; "RSA" }; record { "kid"; "..." };
   * record { "n"; "..." }; record { "e"; "AQAB" } } }.
   */
  'seed_jwks' : [] | [Array<Array<[string, string]>>],
  'client_id' : string,
}
export interface OpenIdCredential {
  'aud' : Aud,
  'iss' : Iss,
  'sub' : Sub,
  'metadata' : MetadataMapV2,
  /**
   * SSO discovery domain this credential was verified through. `None` for
   * direct-provider credentials (Google / Apple / Microsoft).
   */
  'sso_domain' : [] | [string],
  /**
   * Human-readable SSO name from the domain's
   * `/.well-known/ii-openid-configuration`. `None` when the domain
   * doesn't publish one — callers should fall back to `sso_domain`
   * for the label.
   */
  'sso_name' : [] | [string],
  'last_usage_timestamp' : [] | [Timestamp],
}
export type OpenIdCredentialAddError = {
    'OpenIdCredentialAlreadyRegistered' : null
  } |
  { 'InternalCanisterError' : string } |
  { 'JwtExpired' : null } |
  { 'Unauthorized' : Principal } |
  { 'JwtVerificationFailed' : null };
export type OpenIdCredentialKey = [Iss, Sub, Aud];
export type OpenIdCredentialRemoveError = { 'InternalCanisterError' : string } |
  { 'OpenIdCredentialNotFound' : null } |
  { 'Unauthorized' : Principal };
export type OpenIdDelegationError = { 'NoSuchDelegation' : null } |
  { 'NoSuchAnchor' : null } |
  { 'JwtExpired' : null } |
  { 'JwtVerificationFailed' : null };
export type OpenIdEmailVerification = { 'Google' : null } |
  { 'Unknown' : null } |
  { 'Microsoft' : null };
export interface OpenIdPrepareDelegationResponse {
  'user_key' : UserKey,
  'expiration' : Timestamp,
  'anchor_number' : UserNumber,
}
/**
 * The delegation permissions a caller requests, mirroring the ICP protocol's
 * request-delegation `permissions` values. `queries` yields a queries-only
 * delegation (the issued `Delegation` carries `permissions = "queries"`);
 * `all` yields an unrestricted, update-capable delegation. Passed as an
 * optional argument; an absent argument means `all`, preserving the
 * pre-feature behavior and matching the interface spec's default for an
 * absent `permissions` field.
 */
export type Permissions = { 'all' : null } |
  { 'queries' : null };
export interface PrepareAccountDelegation {
  'user_key' : UserKey,
  'expiration' : Timestamp,
}
export type PrepareAttributeError = { 'AuthorizationError' : Principal } |
  { 'ValidationError' : { 'problems' : Array<string> } } |
  { 'GetAccountError' : GetAccountError };
export interface PrepareAttributeRequest {
  /**
   * Origin of the relying party in the attribute sharing flow.
   */
  'origin' : FrontendHostname,
  /**
   * The attributes to be prepared. Each key has the form
   * `<scope>:<attribute_name>`, where `<scope>` is either
   * `openid:<issuer>` (e.g. `openid:https://accounts.google.com:email`)
   * or `sso:<domain>` (e.g. `sso:dfinity.org:email`).
   * 
   * Each linked credential is addressable via exactly one scope:
   * credentials obtained through SSO two-hop discovery are reachable only
   * via `sso:<domain>`; credentials from hardcoded OIDC providers (Google,
   * Microsoft, …) are reachable only via `openid:<issuer>`. Under `sso:`
   * only `email` and `name` are supported;
   * under `openid:` `email`, `name`, and `verified_email` are supported.
   */
  'attribute_keys' : Array<string>,
  /**
   * II account for the relying party.
   */
  'account_number' : [] | [AccountNumber],
  /**
   * Identity for which the attributes should be prepared.
   */
  'identity_number' : IdentityNumber,
}
export interface PrepareAttributeResponse {
  'attributes' : Array<[string, Uint8Array | number[]]>,
  'issued_at_timestamp_ns' : Timestamp,
}
export type PrepareIcrc3AttributeError = { 'AuthorizationError' : Principal } |
  { 'ValidationError' : { 'problems' : Array<string> } } |
  { 'GetAccountError' : GetAccountError } |
  { 'AttributeMismatch' : { 'problems' : Array<string> } };
export interface PrepareIcrc3AttributeRequest {
  /**
   * The relying party's actual origin, before the legacy `icp0.io →
   * ic0.app` remap that `origin` may have gone through. When set, this
   * value is what gets certified as `implicit:origin` instead of `origin`,
   * so that an RP signed in via the icp0.io domain sees its real origin
   * in the certified message. The canister verifies that mapping
   * `unmapped_origin` through the same legacy remap yields `origin`,
   * so an RP can't certify an arbitrary value here.
   */
  'unmapped_origin' : [] | [FrontendHostname],
  /**
   * Origin of the relying party in the attribute sharing flow. May have
   * been remapped from `<sub>.icp0.io` to `<sub>.ic0.app` for principal
   * stability — see `unmapped_origin`.
   */
  'origin' : FrontendHostname,
  /**
   * II account for the relying party.
   */
  'account_number' : [] | [AccountNumber],
  /**
   * The attributes to be certified.
   */
  'attributes' : Array<AttributeSpec>,
  /**
   * The nonce is a 32-byte value generated by the relying party.
   * The purpose of the nonce is to prevent replay attacks and
   * enable the relying party to expire attributes issued for a given flow.
   * The value of the nonce will be included into the signed ICRC-3
   * message as a separate attribute with the key `implicit:nonce`.
   */
  'nonce' : Uint8Array | number[],
  /**
   * Identity for which the attributes should be prepared.
   */
  'identity_number' : IdentityNumber,
}
export interface PrepareIcrc3AttributeResponse {
  /**
   * Candid-encoded ICRC-3 Value map. Pass this to get_icrc3_attributes.
   */
  'message' : Uint8Array | number[],
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
 * Result of prepare_mcp_registration_delegation: the canister-signature public
 * key the registration delegation is rooted at (P_reg), and the (short)
 * expiration of that delegation. The frontend fetches the signed delegation via
 * get_mcp_registration_delegation and delivers the chain to the trusted MCP
 * server, which redeems it with mcp_register_v2.
 */
export interface PrepareMcpRegistrationDelegation {
  'user_key' : UserKey,
  'expiration' : Timestamp,
}
export interface PrepareSessionDelegation {
  'user_key' : UserKey,
  'expiration' : Timestamp,
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
/**
 * DNSSEC proof bundle and supporting types — see
 * `internet_identity_interface::types::dnssec`.
 */
export interface Rrsig {
  'algorithm' : number,
  'signature' : Uint8Array | number[],
  'original_ttl' : number,
  'signer_name' : Uint8Array | number[],
  'labels' : number,
  'inception' : number,
  'expiration' : number,
  'key_tag' : number,
  'type_covered' : number,
}
export type Salt = Uint8Array | number[];
export type SessionDelegationError = { 'NoSuchDelegation' : null } |
  { 'InternalCanisterError' : string } |
  { 'Unauthorized' : Principal };
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
export interface SignedRRset {
  'ttl' : number,
  'name' : Uint8Array | number[],
  'rdata' : Array<Uint8Array | number[]>,
  'rrsig' : Rrsig,
  'rtype' : number,
}
export interface SmtpAddress { 'domain' : string, 'user' : string }
export interface SmtpEnvelope {
  /**
   * SMTP allows multiple `RCPT TO` recipients per envelope, so this
   * is a vec at the wire level. For the recovery flows this canister
   * serves, however, we require *exactly one* recipient and it must
   * be `register@<domain>` or `recover@<domain>` — a legitimate
   * recovery email never targets a CC/BCC alongside us, so any
   * additional recipient can only come from a phishy forwarder
   * trying to exfiltrate the user's canister-signed challenge.
   * Multi-recipient envelopes (and empty ones) are rejected with
   * code 551 ("User not local"); single-recipient envelopes whose
   * recipient isn't one of our reserved mailboxes get 550 ("No
   * such user here"). The vec is also capped at 100 entries (RFC
   * 5321 §4.5.3.1.10); envelopes exceeding the cap are rejected
   * with code 555.
   */
  'to' : Array<SmtpAddress>,
  'from' : SmtpAddress,
}
/**
 * SMTP gateway types — see `internet_identity_interface::smtp`. Carried
 * forward from PoC #3760 surface (the existing gateway can target this
 * canister without changes).
 */
export interface SmtpHeader { 'value' : string, 'name' : string }
export interface SmtpMessage {
  'body' : Uint8Array | number[],
  'headers' : Array<SmtpHeader>,
}
export interface SmtpRequest {
  'envelope' : [] | [SmtpEnvelope],
  'message' : [] | [SmtpMessage],
  'gateway_flags' : [] | [Array<string>],
  /**
   * Optional gateway-supplied correlation id for one inbound message
   * (e.g. the RFC 5322 Message-ID or a gateway-assigned tracking id).
   * The canister does not interpret it; it lets a reported case be
   * lined up across the SMTP gateway logs and the canister's production
   * logs during support investigations. Capped at 256 bytes; oversize
   * values are rejected with code 555.
   */
  'message_id' : [] | [string],
}
/**
 * Error returned by `smtp_request` / `smtp_request_validate`.
 * 
 * `code` mirrors the SMTP reply codes the off-chain gateway should
 * emit upstream:
 * - `550` (mailbox unavailable) — "No such user here". Returned when
 * the envelope carries exactly one recipient but it isn't a mailbox
 * this canister handles (i.e. neither `register@<domain>` nor
 * `recover@<domain>` for any `<domain>` in `related_origins`).
 * - `551` (user not local) — envelope-shape rejection. Returned for
 * empty `to` and for multi-recipient envelopes, even when one of
 * the recipients is ours. Distinct from 550 so the gateway can tell
 * "this envelope shape isn't accepted" from "we don't know this
 * user". Recovery emails never legitimately address a CC/BCC
 * alongside `register@…` / `recover@…`.
 * - `555` (syntax error) — the request shape itself is malformed
 * (e.g. missing envelope, oversize address/header/body, recipient
 * list exceeds the 100-entry cap).
 */
export interface SmtpRequestError { 'code' : bigint, 'message' : string }
export type SmtpResponse = { 'Ok' : {} } |
  { 'Err' : SmtpRequestError };
/**
 * One entry of the `sso_credential_migration` backfill. Maps the
 * (iss, aud) pair of a stored SSO credential to the discovery domain and
 * optional human-readable name it resolves to.
 */
export interface SsoCredentialMigrationEntry {
  /**
   * Human-readable SSO label; stamped onto the credential's `sso_name`.
   */
  'name' : [] | [string],
  /**
   * Matches the stored credential's `iss`.
   */
  'issuer' : string,
  'discovery_domain' : string,
  /**
   * Matches the stored credential's `aud`.
   */
  'client_id' : string,
}
/**
 * Fully resolved SSO discovery result for the sign-in initiation flow,
 * returned by `discover_sso` / `discover_sso_query`. The canister resolves it
 * from the domain's two-hop discovery documents, on demand and cached.
 */
export interface SsoDiscovery {
  'scopes' : Array<string>,
  'name' : [] | [string],
  'authorization_endpoint' : string,
  'issuer' : string,
  /**
   * Client the target origin runs its ceremony against; `null` when denied.
   */
  'resolved_client_id' : [] | [string],
  'discovery_domain' : string,
  /**
   * The org's primary OIDC client.
   */
  'client_id' : string,
}
/**
 * Status of a domain's SSO discovery, read by `get_sso_discovery_status`. A
 * failed fetch isn't a distinct status — it reads as `Pending` and the frontend
 * times out — so the statuses are: resolved, or in flight.
 */
export type SsoDiscoveryStatus = { 'Resolved' : SsoDiscovery } |
  { 'Pending' : null };
/**
 * Request for `sso_get_delegation`.
 */
export interface SsoGetDelegationRequest {
  'jwt' : JWT,
  'session_key' : SessionKey,
  'salt' : Salt,
  'sso_attr_bundle' : Uint8Array | number[],
  'target_app_origin' : FrontendHostname,
  'expiration' : Timestamp,
  'org_domain' : string,
}
/**
 * Response of `sso_get_delegation`.
 */
export interface SsoGetDelegationResponse {
  'signed_delegation' : SignedDelegation,
  'sso_attr_bundle_signature' : Uint8Array | number[],
}
/**
 * Request for `sso_prepare_delegation`.
 */
export interface SsoPrepareDelegationRequest {
  'jwt' : JWT,
  'session_key' : SessionKey,
  'salt' : Salt,
  'target_app_origin' : FrontendHostname,
  'org_domain' : string,
}
/**
 * Response of `sso_prepare_delegation`.
 */
export interface SsoPrepareDelegationResponse {
  'user_key' : UserKey,
  'sso_attr_bundle' : Uint8Array | number[],
  'expiration' : Timestamp,
  'anchor_number' : UserNumber,
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
/**
 * Which trust path the canister used (or will use) to verify the
 * challenge email. Public — already chosen by the FE and derivable
 * from the public deploy config.
 */
export type VerificationPath = { 'Doh' : null } |
  { 'Dnssec' : null };
/**
 * A verified email address bound to an anchor. Parallel to
 * `EmailRecoveryCredential` but used as an attribute source (and
 * surfaced in the smart-routing UI) rather than a recovery
 * credential. The `verified_at` timestamp is when DKIM/DMARC
 * verification completed successfully.
 */
export interface VerifiedEmail { 'address' : string, 'verified_at' : Timestamp }
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
  /**
   * Authenticator Attestation Global Unique Identifier (AAGUID)
   */
  'aaguid' : [] | [Aaguid],
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
  /**
   * SSO discovery for the sign-in initiation flow. The frontend polls
   * `get_sso_discovery_status` (query) and, while it reads `Pending`, drives the
   * on-demand two-hop discovery fetch with `discover_sso` (update); once the
   * fetch completes the query returns `Resolved` with the config.
   * 
   * The optional second argument is the target dapp origin; when supplied,
   * `resolved_client_id` reports the client that origin must use (`null` = denied).
   */
  'discover_sso' : ActorMethod<[string], undefined>,
  'email_challenge_diagnostics' : ActorMethod<
    [string],
    [] | [EmailChallengeDiagnostics]
  >,
  'email_challenge_resolve_via_doh' : ActorMethod<
    [EmailChallengeResolveViaDohArg],
    { 'Ok' : null } |
      { 'Err' : EmailChallengeError }
  >,
  /**
   * FE-side polling — the wizard / panel calls these repeatedly to
   * drive the "waiting for your email" UI. Status flips through
   * `Pending` → `ResolvingDoh` / `NeedDkimLeaf` → terminal.
   * Diagnostics returns strictly-public, user-copyable failure
   * detail for support tickets (no PII, no address, no anchor).
   */
  'email_challenge_status' : ActorMethod<[string], EmailChallengeStatus>,
  /**
   * DNSSEC-path completion (`submit_dkim_leaf`) and DoH-path
   * completion (`resolve_via_doh`). One or the other runs per
   * challenge depending on which path the canister picked at
   * prepare-time. Both are polled by the FE while the status is
   * `NeedDkimLeaf` / `ResolvingDoh`.
   */
  'email_challenge_submit_dkim_leaf' : ActorMethod<
    [EmailChallengeSubmitDkimLeafArg],
    { 'Ok' : null } |
      { 'Err' : EmailChallengeError }
  >,
  /**
   * ===================================================================
   * Email-recovery flow (recovery-as-login)
   * ===================================================================
   * Recovery-specific surface built on top of the shared challenge
   * primitive above. See `docs/ongoing/email-recovery.md` for the
   * full design.
   * - Setup: `credential_prepare_add` (authenticated) →
   * `smtp_request` for `register@<domain>` → credential bound to
   * the anchor's `email_recovery`. Removed via `credential_remove`.
   * - Recovery: `prepare_delegation` (anonymous, bound to a
   * `session_key`) → `smtp_request` for `recover@<domain>` →
   * canister stamps a signed delegation seed. The FE then calls
   * `get_delegation` to retrieve the `SignedDelegation`.
   */
  'email_recovery_credential_prepare_add' : ActorMethod<
    [IdentityNumber, EmailChallengeDnsInput],
    { 'Ok' : EmailChallenge } |
      { 'Err' : EmailChallengeError }
  >,
  'email_recovery_credential_remove' : ActorMethod<
    [IdentityNumber, string],
    { 'Ok' : null } |
      { 'Err' : EmailChallengeError }
  >,
  'email_recovery_diagnostics' : ActorMethod<
    [string],
    [] | [EmailChallengeDiagnostics]
  >,
  'email_recovery_get_delegation' : ActorMethod<
    [EmailRecoveryGetDelegationArgs],
    { 'Ok' : SignedDelegation } |
      { 'Err' : EmailChallengeError }
  >,
  'email_recovery_prepare_delegation' : ActorMethod<
    [EmailChallengeDnsInput, SessionKey],
    { 'Ok' : EmailChallenge } |
      { 'Err' : EmailChallengeError }
  >,
  'email_recovery_resolve_via_doh' : ActorMethod<
    [EmailChallengeResolveViaDohArg],
    { 'Ok' : null } |
      { 'Err' : EmailChallengeError }
  >,
  /**
   * ===================================================================
   * DEPRECATED — remove in a follow-up PR
   * ===================================================================
   * Legacy aliases for the four `email_challenge_*` methods, kept so
   * a stale FE bundle in a browser cache — or any FE build that
   * lands before this canister's renamed methods — can still drive
   * the inbound-DKIM flow without a "method not found" break
   * mid-verification.
   * 
   * The wire bytes are identical to the new methods (Candid is
   * structurally typed; the renamed return types match the old
   * types' shapes field-for-field), so old clients with bindings
   * against the old type names deserialize successfully.
   * 
   * **All methods below must be removed together in a single
   * follow-up `chore(be): remove deprecated email_recovery_* method
   * aliases` PR**, once every deployed FE has refreshed to the
   * `email_challenge_*` names. See `TASKS.md` for the tracked
   * follow-up.
   */
  'email_recovery_status' : ActorMethod<[string], EmailChallengeStatus>,
  'email_recovery_submit_dkim_leaf' : ActorMethod<
    [EmailChallengeSubmitDkimLeafArg],
    { 'Ok' : null } |
      { 'Err' : EmailChallengeError }
  >,
  'enter_device_registration_mode' : ActorMethod<[UserNumber], Timestamp>,
  'exit_device_registration_mode' : ActorMethod<[UserNumber], undefined>,
  /**
   * Returns a batch of entries _sorted by sequence number_ to be archived.
   * This is an update call because the archive information _must_ be certified.
   * Only callable by this IIs archive canister.
   */
  'fetch_entries' : ActorMethod<[], Array<BufferedArchiveEntry>>,
  'get_account_delegation' : ActorMethod<
    [
      UserNumber,
      FrontendHostname,
      [] | [AccountNumber],
      SessionKey,
      Timestamp,
      [] | [Permissions],
    ],
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
  'get_attributes' : ActorMethod<
    [GetAttributesRequest],
    { 'Ok' : CertifiedAttributes } |
      { 'Err' : GetAttributesError }
  >,
  'get_default_account' : ActorMethod<
    [UserNumber, FrontendHostname],
    { 'Ok' : AccountInfo } |
      { 'Err' : GetDefaultAccountError }
  >,
  'get_delegation' : ActorMethod<
    [UserNumber, FrontendHostname, SessionKey, Timestamp],
    GetDelegationResponse
  >,
  'get_icrc3_attributes' : ActorMethod<
    [GetIcrc3AttributeRequest],
    { 'Ok' : GetIcrc3AttributeResponse } |
      { 'Err' : GetIcrc3AttributeError }
  >,
  'get_id_alias' : ActorMethod<
    [GetIdAliasRequest],
    { 'Ok' : IdAliasCredentials } |
      { 'Err' : GetIdAliasError }
  >,
  /**
   * Fetch the signed registration delegation prepared above, to deliver to the
   * trusted MCP server. Authenticated as the identity, like the prepare call.
   * user_key is the value the prepare call returned; the seed is recovered
   * from it, so no consent parameters need re-passing.
   */
  'get_mcp_registration_delegation' : ActorMethod<
    [UserNumber, SessionKey, PublicKey, Timestamp],
    { 'Ok' : SignedDelegation } |
      { 'Err' : string }
  >,
  'get_principal' : ActorMethod<[UserNumber, FrontendHostname], Principal>,
  'get_session_delegation' : ActorMethod<
    [UserNumber, SessionKey, Timestamp],
    { 'Ok' : SignedDelegation } |
      { 'Err' : SessionDelegationError }
  >,
  'get_sso_discovery_status' : ActorMethod<
    [GetSsoDiscoveryStatusRequest],
    SsoDiscoveryStatus
  >,
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
  'list_available_attributes' : ActorMethod<
    [ListAvailableAttributesRequest],
    { 'Ok' : ListAvailableAttributesResponse } |
      { 'Err' : ListAvailableAttributesError }
  >,
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
   * Looks up identity number when called with a recovery phrase
   */
  'lookup_caller_identity_by_recovery_phrase' : ActorMethod<
    [],
    [] | [IdentityNumber]
  >,
  /**
   * Discoverable passkeys protocol
   */
  'lookup_device_key' : ActorMethod<
    [Uint8Array | number[]],
    [] | [DeviceKeyWithAnchor]
  >,
  /**
   * Called by the MCP server (anchor recovered from caller()'s grant): list
   * the anchor's accounts at target_origin so the agent can pick which
   * account_number to request a delegation for via mcp_prepare_delegation.
   */
  'mcp_get_accounts' : ActorMethod<
    [FrontendHostname],
    { 'Ok' : Array<AccountInfo> } |
      { 'Err' : AccountDelegationError }
  >,
  /**
   * Read the identity's synced trusted-MCP-server config (master toggle + the
   * trusted server URL). Persisted on-chain, so it follows the identity across
   * devices. Read by the Settings UI and the /mcp connect flow (which verifies
   * the connecting origin against it). Returns the disabled, no-server default
   * for an unauthorized caller or an anchor that never wrote a config.
   */
  'mcp_get_config' : ActorMethod<[UserNumber], McpConfig>,
  /**
   * Fetch the delegation prepared above; the anchor is recovered from
   * caller()'s grant. account_number and expiration must be the values
   * returned by the matching mcp_prepare_delegation, else this returns
   * NoSuchDelegation.
   */
  'mcp_get_delegation' : ActorMethod<
    [FrontendHostname, [] | [AccountNumber], SessionKey, Timestamp],
    { 'Ok' : SignedDelegation } |
      { 'Err' : AccountDelegationError }
  >,
  /**
   * Called by the MCP server, signed with its registered session key and
   * authorized by caller()'s unexpired grant; the anchor is recovered from
   * the caller. Mints a per-app delegation at target_origin. account_number
   * names one of the anchor's accounts there to act as (discover them with
   * mcp_get_accounts), and null uses the anchor's default account there; an
   * account_number that isn't the anchor's at target_origin is rejected as
   * Unauthorized. max_ttl is the requested lifetime in ns, defaulting to and
   * capped at 1 hour, and never outliving the session grant. The resolved
   * account_number is returned in McpPrepareDelegation so it can be threaded
   * into mcp_get_delegation (the default account at an origin is mutable).
   */
  'mcp_prepare_delegation' : ActorMethod<
    [FrontendHostname, [] | [AccountNumber], SessionKey, [] | [bigint]],
    { 'Ok' : McpPrepareDelegation } |
      { 'Err' : AccountDelegationError }
  >,
  /**
   * Called by the trusted MCP server, authenticated by the registration
   * delegation chain: bind the server's long-lived session_key to the
   * consenting anchor. The entire consent — anchor, read-only choice, grant
   * lifetime — is recovered from the entry keyed by caller() (the registration
   * principal), so the server passes only session_key: it cannot name a
   * different anchor, upgrade the access level or stretch the grant, and never
   * learns the anchor number. A trusted-server switch or disable since consent
   * invalidates the delegation. Usable until the registration delegation
   * expires (~5 min); re-registration within that window replaces the anchor's
   * single MCP session.
   */
  'mcp_register_v2' : ActorMethod<
    [SessionKey],
    { 'Ok' : McpRegistrationV2 } |
      { 'Err' : string }
  >,
  /**
   * Persist the identity's trusted-MCP-server config so it syncs across the
   * identity's devices. Authenticated as the identity, so only the user — never
   * a page that initiates a connect request — can change what it trusts.
   * Disabling MCP or changing the trusted server URL revokes the identity's
   * active MCP session in the same message.
   */
  'mcp_set_config' : ActorMethod<
    [UserNumber, McpConfig],
    { 'Ok' : null } |
      { 'Err' : string }
  >,
  /**
   * The trailing `opt text` is the SSO discovery domain (null for a direct
   * provider). For SSO sign-ins a cold discovery/JWKS cache yields the
   * `Pending` result arm — a retry signal, not an error: the caller re-calls
   * the method (and for delegations, polls `openid_get_delegation`, re-calling
   * `openid_prepare_delegation` on a `Pending` poll result).
   */
  'openid_credential_add' : ActorMethod<
    [IdentityNumber, JWT, Salt, [] | [string]],
    { 'Ok' : null } |
      { 'Err' : OpenIdCredentialAddError } |
      { 'Pending' : null }
  >,
  'openid_credential_remove' : ActorMethod<
    [IdentityNumber, OpenIdCredentialKey],
    { 'Ok' : null } |
      { 'Err' : OpenIdCredentialRemoveError }
  >,
  'openid_get_delegation' : ActorMethod<
    [JWT, Salt, SessionKey, Timestamp, [] | [string]],
    { 'Ok' : SignedDelegation } |
      { 'Err' : OpenIdDelegationError } |
      { 'Pending' : null }
  >,
  /**
   * OpenID credentials protocol
   * ===========================
   */
  'openid_identity_registration_finish' : ActorMethod<
    [OpenIDRegFinishArg],
    { 'Ok' : IdRegFinishResult } |
      { 'Err' : IdRegFinishError } |
      { 'Pending' : null }
  >,
  'openid_prepare_delegation' : ActorMethod<
    [JWT, Salt, SessionKey, [] | [string]],
    { 'Ok' : OpenIdPrepareDelegationResponse } |
      { 'Err' : OpenIdDelegationError } |
      { 'Pending' : null }
  >,
  'prepare_account_delegation' : ActorMethod<
    [
      UserNumber,
      FrontendHostname,
      [] | [AccountNumber],
      SessionKey,
      [] | [bigint],
      [] | [Permissions],
    ],
    { 'Ok' : PrepareAccountDelegation } |
      { 'Err' : AccountDelegationError }
  >,
  /**
   * Attribute sharing protocol
   * ==========================
   */
  'prepare_attributes' : ActorMethod<
    [PrepareAttributeRequest],
    { 'Ok' : PrepareAttributeResponse } |
      { 'Err' : PrepareAttributeError }
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
   * ICRC-3 Attribute sharing protocol
   * ==================================
   */
  'prepare_icrc3_attributes' : ActorMethod<
    [PrepareIcrc3AttributeRequest],
    { 'Ok' : PrepareIcrc3AttributeResponse } |
      { 'Err' : PrepareIcrc3AttributeError }
  >,
  /**
   * Old Verifiable Credentials API
   * ==============================
   * The methods below are used to generate ID-alias credentials during attribute sharing flow.
   */
  'prepare_id_alias' : ActorMethod<
    [PrepareIdAliasRequest],
    { 'Ok' : PreparedIdAlias } |
      { 'Err' : PrepareIdAliasError }
  >,
  /**
   * Mint a short-lived MCP registration delegation (P_reg -> registration_key).
   * Authenticated as the identity (only the consenting user can create one).
   * registration_key is an ephemeral key the II frontend generates for this
   * connect (browser-held — the canister never delegates to a key taken from
   * the connect link; the frontend extends the chain to the server's key
   * browser-side). P_reg is derived from a fresh random nonce, and the whole
   * consent — the anchor, permissions (read-only choice), max_ttl (session-
   * grant lifetime), and the identity's current trusted-server URL — is
   * recorded on an index entry keyed by P_reg, so mcp_register_v2 recovers it
   * server-side and the server cannot alter any of it.
   */
  'prepare_mcp_registration_delegation' : ActorMethod<
    [UserNumber, SessionKey, [] | [Permissions], [] | [bigint]],
    { 'Ok' : PrepareMcpRegistrationDelegation } |
      { 'Err' : string }
  >,
  'prepare_session_delegation' : ActorMethod<
    [UserNumber, SessionKey, [] | [bigint]],
    { 'Ok' : PrepareSessionDelegation } |
      { 'Err' : SessionDelegationError }
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
  /**
   * ===================================================================
   * Email-challenge protocol (shared inbound-DKIM primitive)
   * ===================================================================
   * The flow-neutral surface — used by both the email-recovery flow
   * and the verified-emails flow. Methods here are keyed by the
   * canister-issued nonce; dispatch to the right anchor sink happens
   * inside the canister via `PendingKind` on the pending entry.
   * 
   * The off-chain SMTP gateway forwards every inbound message via
   * `smtp_request`. The canister verifies the email cryptographically
   * and dispatches by recipient: `register@<domain>` → setup
   * completion (either flow), `recover@<domain>` → recovery
   * delegation stamping. Always returns Ok — the gateway shouldn't
   * get a per-message verification signal back. The FE sees outcomes
   * via `email_challenge_status`.
   */
  'smtp_request' : ActorMethod<[SmtpRequest], SmtpResponse>,
  /**
   * Called by the gateway at RCPT TO time to decide whether to
   * accept the connection before pulling the message body. Returns
   * Ok for `register@<domain>` / `recover@<domain>` (case-
   * insensitive), and 550 (mailbox unavailable) for everything else.
   */
  'smtp_request_validate' : ActorMethod<[SmtpRequest], SmtpResponse>,
  'sso_get_delegation' : ActorMethod<
    [SsoGetDelegationRequest],
    { 'Ok' : SsoGetDelegationResponse } |
      { 'Err' : OpenIdDelegationError } |
      { 'Pending' : null }
  >,
  /**
   * SSO sign-in path. Mints a delegation only if the JWT's `aud` matches the
   * client the target origin resolves to. `Pending` means the discovery/JWKS
   * cache is cold — re-call to drive the fetch.
   */
  'sso_prepare_delegation' : ActorMethod<
    [SsoPrepareDelegationRequest],
    { 'Ok' : SsoPrepareDelegationResponse } |
      { 'Err' : OpenIdDelegationError } |
      { 'Pending' : null }
  >,
  'stats' : ActorMethod<[], InternetIdentityStats>,
  'update' : ActorMethod<[UserNumber, DeviceKey, DeviceData], undefined>,
  'update_account' : ActorMethod<
    [UserNumber, FrontendHostname, [] | [AccountNumber], AccountUpdate],
    { 'Ok' : AccountInfo } |
      { 'Err' : UpdateAccountError }
  >,
  /**
   * ===================================================================
   * Verified-emails flow (attribute source)
   * ===================================================================
   * Parallel to the recovery flow but the verified address is stored
   * on `Anchor::verified_emails` rather than `Anchor::email_recovery`.
   * Reuses the same SMTP gateway, DKIM verifier and DMARC alignment,
   * but issues nonces with the `II-Verify-` prefix so an inbound
   * challenge can never be cross-applied between the two flows.
   * Capped at MAX_VERIFIED_EMAILS_PER_ANCHOR (5) addresses per anchor.
   */
  'verified_email_prepare_add' : ActorMethod<
    [IdentityNumber, EmailChallengeDnsInput],
    { 'Ok' : EmailChallenge } |
      { 'Err' : EmailChallengeError }
  >,
  'verified_email_remove' : ActorMethod<
    [IdentityNumber, string],
    { 'Ok' : null } |
      { 'Err' : EmailChallengeError }
  >,
  'verify_tentative_device' : ActorMethod<
    [UserNumber, string],
    VerifyTentativeDeviceResponse
  >,
  'whoami' : ActorMethod<[], Principal>,
}
export declare const idlFactory: IDL.InterfaceFactory;
export declare const init: (args: { IDL: typeof IDL }) => IDL.Type[];
