import type { Principal } from '@dfinity/principal';
import type { ActorMethod } from '@dfinity/agent';

export interface ActiveAnchorCounter {
  'counter' : bigint,
  'start_timestamp' : Timestamp,
}
export interface ActiveAnchorStatistics {
  'completed' : CompletedActiveAnchorStats,
  'ongoing' : OngoingActiveAnchorStats,
}
export type AddTentativeAuthResponse = {
    'another_auth_tentatively_added' : null
  } |
  {
    'added_tentatively' : {
      'verification_code' : string,
      'device_registration_timeout' : Timestamp,
    }
  } |
  { 'auth_registration_mode_off' : null };
export interface ArchiveConfig {
  'polling_interval_ns' : bigint,
  'entries_buffer_limit' : bigint,
  'module_hash' : Uint8Array | number[],
  'entries_fetch_limit' : number,
}
export interface ArchiveInfo {
  'archive_config' : [] | [ArchiveConfig],
  'archive_canister' : [] | [Principal],
}
export type Auth = {
    'webauthn_key' : {
      'direct_signature' : WebAuthnDirectSigMode,
      'pubkey' : PublicKey,
      'credential_id' : CredentialId,
    }
  } |
  { 'generic_key' : { 'pubkey' : PublicKey } };
export type AuthProtection = { 'unprotected' : null } |
  { 'protected' : null };
export interface AuthRecord {
  'last_usage' : [] | [Timestamp],
  'metadata' : MetadataMap,
  'auth' : Auth,
  'protection' : AuthProtection,
}
export interface AuthRegistrationInfo {
  'expiration' : Timestamp,
  'tentative_auth' : [] | [AuthRecord],
}
export interface AuthSettings {
  'direct_signature' : WebAuthnDirectSigMode,
  'protection' : AuthProtection,
}
export interface BufferedArchiveEntry {
  'sequence_number' : bigint,
  'entry' : Uint8Array | number[],
  'anchor_number' : IdentityNumber,
  'timestamp' : Timestamp,
}
export interface Challenge {
  'png_base64' : string,
  'challenge_key' : ChallengeKey,
}
export type ChallengeKey = string;
export interface ChallengeResult { 'key' : ChallengeKey, 'chars' : string }
export interface CompletedActiveAnchorStats {
  'monthly_active_anchors' : [] | [ActiveAnchorCounter],
  'daily_active_anchors' : [] | [ActiveAnchorCounter],
}
export type CredentialId = Uint8Array | number[];
export interface Delegation {
  'pubkey' : PublicKey,
  'targets' : [] | [Array<Principal>],
  'expiration' : Timestamp,
}
export type DeployArchiveResult = { 'creation_in_progress' : null } |
  { 'success' : Principal } |
  { 'failed' : string };
export interface DomainActiveAnchorCounter {
  'start_timestamp' : Timestamp,
  'internetcomputer_org_counter' : bigint,
  'ic0_app_counter' : bigint,
  'both_ii_domains_counter' : bigint,
}
export interface DomainActiveAnchorStatistics {
  'completed' : DomainCompletedActiveAnchorStats,
  'ongoing' : DomainOngoingActiveAnchorStats,
}
export interface DomainCompletedActiveAnchorStats {
  'monthly_active_anchors' : [] | [DomainActiveAnchorCounter],
  'daily_active_anchors' : [] | [DomainActiveAnchorCounter],
}
export interface DomainOngoingActiveAnchorStats {
  'monthly_active_anchors' : Array<DomainActiveAnchorCounter>,
  'daily_active_anchors' : DomainActiveAnchorCounter,
}
export type FrontendHostname = string;
export type GetDelegationResponse = { 'no_such_delegation' : null } |
  { 'signed_delegation' : SignedDelegation };
export type HeaderField = [string, string];
export interface HttpRequest {
  'url' : string,
  'method' : string,
  'body' : Uint8Array | number[],
  'headers' : Array<HeaderField>,
}
export interface HttpResponse {
  'body' : Uint8Array | number[],
  'headers' : Array<HeaderField>,
  'upgrade' : [] | [boolean],
  'streaming_strategy' : [] | [StreamingStrategy],
  'status_code' : number,
}
export interface IdentityAuthInfo {
  'auth' : Array<Auth>,
  'recovery' : Array<Auth>,
}
export interface IdentityInfo {
  'auth_records' : Array<AuthRecord>,
  'auth_registration' : [] | [AuthRegistrationInfo],
  'recovery_records' : Array<AuthRecord>,
}
export type IdentityNumber = bigint;
export interface InternetIdentityInit {
  'max_num_latest_delegation_origins' : [] | [bigint],
  'assigned_user_number_range' : [] | [[bigint, bigint]],
  'archive_config' : [] | [ArchiveConfig],
  'canister_creation_cycles_cost' : [] | [bigint],
  'register_rate_limit' : [] | [RateLimitConfig],
}
export interface InternetIdentityStats {
  'storage_layout_version' : number,
  'users_registered' : bigint,
  'domain_active_anchor_stats' : [] | [DomainActiveAnchorStatistics],
  'max_num_latest_delegation_origins' : bigint,
  'assigned_user_number_range' : [bigint, bigint],
  'latest_delegation_origins' : Array<FrontendHostname>,
  'archive_info' : ArchiveInfo,
  'canister_creation_cycles_cost' : bigint,
  'active_anchor_stats' : [] | [ActiveAnchorStatistics],
}
export type MetadataMap = Array<
  [
    string,
    { 'map' : MetadataMap } |
      { 'string' : string } |
      { 'bytes' : Uint8Array | number[] },
  ]
>;
export interface OngoingActiveAnchorStats {
  'monthly_active_anchors' : Array<ActiveAnchorCounter>,
  'daily_active_anchors' : ActiveAnchorCounter,
}
export type PublicKey = Uint8Array | number[];
export interface RateLimitConfig {
  'max_tokens' : bigint,
  'time_per_token_ns' : bigint,
}
export type RegisterResponse = { 'bad_challenge' : null } |
  { 'canister_full' : null } |
  { 'registered' : { 'identity_number' : IdentityNumber } };
export type SessionKey = PublicKey;
export interface SignedDelegation {
  'signature' : Uint8Array | number[],
  'delegation' : Delegation,
}
export interface StreamingCallbackHttpResponse {
  'token' : [] | [Token],
  'body' : Uint8Array | number[],
}
export type StreamingStrategy = {
    'Callback' : { 'token' : Token, 'callback' : [Principal, string] }
  };
export type Timestamp = bigint;
export type Token = {};
export type VerifyTentativeAuthResponse = {
    'device_registration_mode_off' : null
  } |
  { 'verified' : null } |
  { 'wrong_code' : { 'retries_left' : number } } |
  { 'no_device_to_verify' : null };
export type WebAuthnDirectSigMode = { 'optional' : null } |
  { 'mandatory' : null };
export type result = { 'ok' : null };
export interface _SERVICE {
  'acknowledge_entries' : ActorMethod<[bigint], undefined>,
  'add_auth' : ActorMethod<
    [IdentityNumber, AuthRecord, [] | [Uint8Array | number[]]],
    [] | [result]
  >,
  'add_recovery' : ActorMethod<
    [IdentityNumber, AuthRecord, [] | [Uint8Array | number[]]],
    [] | [result]
  >,
  'add_tentative_auth' : ActorMethod<
    [IdentityNumber, Auth, MetadataMap],
    [] | [{ 'ok' : AddTentativeAuthResponse }]
  >,
  'create_captcha' : ActorMethod<[], [] | [{ 'ok' : Challenge }]>,
  'deploy_archive' : ActorMethod<[Uint8Array | number[]], DeployArchiveResult>,
  'enter_auth_registration_mode' : ActorMethod<
    [IdentityNumber],
    [] | [{ 'ok' : Timestamp }]
  >,
  'exit_auth_registration_mode' : ActorMethod<[IdentityNumber], [] | [result]>,
  'fetch_entries' : ActorMethod<[], Array<BufferedArchiveEntry>>,
  'get_delegation' : ActorMethod<
    [IdentityNumber, FrontendHostname, SessionKey, Timestamp],
    GetDelegationResponse
  >,
  'get_principal' : ActorMethod<[IdentityNumber, FrontendHostname], Principal>,
  'http_request' : ActorMethod<[HttpRequest], HttpResponse>,
  'http_request_update' : ActorMethod<[HttpRequest], HttpResponse>,
  'identity_auth_info' : ActorMethod<
    [IdentityNumber],
    [] | [{ 'ok' : IdentityAuthInfo }]
  >,
  'identity_info' : ActorMethod<
    [IdentityNumber],
    [] | [{ 'ok' : IdentityInfo }]
  >,
  'init_salt' : ActorMethod<[], undefined>,
  'prepare_delegation' : ActorMethod<
    [IdentityNumber, FrontendHostname, SessionKey, [] | [bigint]],
    [PublicKey, Timestamp]
  >,
  'register_identity' : ActorMethod<
    [AuthRecord, ChallengeResult, [] | [Principal]],
    [] | [RegisterResponse]
  >,
  'remove_auth' : ActorMethod<
    [IdentityNumber, PublicKey, [] | [Uint8Array | number[]]],
    [] | [result]
  >,
  'replace_auth' : ActorMethod<
    [IdentityNumber, PublicKey, AuthRecord, [] | [Uint8Array | number[]]],
    [] | [result]
  >,
  'stats' : ActorMethod<[], InternetIdentityStats>,
  'update_auth_metadata' : ActorMethod<
    [IdentityNumber, PublicKey, MetadataMap, [] | [Timestamp]],
    [] | [result]
  >,
  'update_auth_settings' : ActorMethod<
    [IdentityNumber, PublicKey, AuthSettings, [] | [Uint8Array | number[]]],
    [] | [result]
  >,
  'verify_tentative_auth' : ActorMethod<
    [IdentityNumber, string, [] | [Uint8Array | number[]]],
    [] | [{ 'ok' : VerifyTentativeAuthResponse }]
  >,
}
