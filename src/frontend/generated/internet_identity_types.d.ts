import type { Principal } from '@dfinity/principal';
export type AddTentativeDeviceResponse = {
    'device_registration_mode_off' : null
  } |
  { 'another_device_tentatively_added' : null } |
  {
    'added_tentatively' : {
      'verification_code' : string,
      'device_registration_timeout' : Timestamp,
    }
  };
export interface Challenge {
  'png_base64' : string,
  'challenge_key' : ChallengeKey,
}
export type ChallengeKey = string;
export interface ChallengeResult { 'key' : ChallengeKey, 'chars' : string }
export type CredentialId = Array<number>;
export interface Delegation {
  'pubkey' : PublicKey,
  'targets' : [] | [Array<Principal>],
  'expiration' : Timestamp,
}
export interface DeviceData {
  'alias' : string,
  'pubkey' : DeviceKey,
  'key_type' : KeyType,
  'purpose' : Purpose,
  'credential_id' : [] | [CredentialId],
}
export type DeviceKey = PublicKey;
export interface DeviceRegistrationInfo {
  'tentative_device' : [] | [DeviceData],
  'expiration' : Timestamp,
}
export type FrontendHostname = string;
export type GetDelegationResponse = { 'no_such_delegation' : null } |
  { 'signed_delegation' : SignedDelegation };
export type HeaderField = [string, string];
export interface HttpRequest {
  'url' : string,
  'method' : string,
  'body' : Array<number>,
  'headers' : Array<HeaderField>,
}
export interface HttpResponse {
  'body' : Array<number>,
  'headers' : Array<HeaderField>,
  'streaming_strategy' : [] | [StreamingStrategy],
  'status_code' : number,
}
export interface IdentityAnchorInfo {
  'devices' : Array<DeviceData>,
  'device_registration' : [] | [DeviceRegistrationInfo],
}
export interface InternetIdentityInit {
  'assigned_user_number_range' : [bigint, bigint],
}
export interface InternetIdentityStats {
  'users_registered' : bigint,
  'assigned_user_number_range' : [bigint, bigint],
}
export type KeyType = { 'platform' : null } |
  { 'seed_phrase' : null } |
  { 'cross_platform' : null } |
  { 'unknown' : null };
export type PublicKey = Array<number>;
export type Purpose = { 'authentication' : null } |
  { 'recovery' : null };
export type RegisterResponse = { 'bad_challenge' : null } |
  { 'canister_full' : null } |
  { 'registered' : { 'user_number' : UserNumber } };
export type SessionKey = PublicKey;
export interface SignedDelegation {
  'signature' : Array<number>,
  'delegation' : Delegation,
}
export interface StreamingCallbackHttpResponse {
  'token' : [] | [Token],
  'body' : Array<number>,
}
export type StreamingStrategy = {
    'Callback' : { 'token' : Token, 'callback' : [Principal, string] }
  };
export type Timestamp = bigint;
export type Token = {};
export type UserKey = PublicKey;
export type UserNumber = bigint;
export type VerifyTentativeDeviceResponse = {
    'device_registration_mode_off' : null
  } |
  { 'verified' : null } |
  { 'wrong_code' : { 'retries_left' : number } } |
  { 'no_device_to_verify' : null };
export interface _SERVICE {
  'add' : (arg_0: UserNumber, arg_1: DeviceData) => Promise<undefined>,
  'add_tentative_device' : (arg_0: UserNumber, arg_1: DeviceData) => Promise<
      AddTentativeDeviceResponse
    >,
  'create_challenge' : () => Promise<Challenge>,
  'enter_device_registration_mode' : (arg_0: UserNumber) => Promise<Timestamp>,
  'exit_device_registration_mode' : (arg_0: UserNumber) => Promise<undefined>,
  'get_anchor_info' : (arg_0: UserNumber) => Promise<IdentityAnchorInfo>,
  'get_delegation' : (
      arg_0: UserNumber,
      arg_1: FrontendHostname,
      arg_2: SessionKey,
      arg_3: Timestamp,
    ) => Promise<GetDelegationResponse>,
  'get_principal' : (arg_0: UserNumber, arg_1: FrontendHostname) => Promise<
      Principal
    >,
  'http_request' : (arg_0: HttpRequest) => Promise<HttpResponse>,
  'init_salt' : () => Promise<undefined>,
  'lookup' : (arg_0: UserNumber) => Promise<Array<DeviceData>>,
  'prepare_delegation' : (
      arg_0: UserNumber,
      arg_1: FrontendHostname,
      arg_2: SessionKey,
      arg_3: [] | [bigint],
    ) => Promise<[UserKey, Timestamp]>,
  'register' : (arg_0: DeviceData, arg_1: ChallengeResult) => Promise<
      RegisterResponse
    >,
  'remove' : (arg_0: UserNumber, arg_1: DeviceKey) => Promise<undefined>,
  'stats' : () => Promise<InternetIdentityStats>,
  'verify_tentative_device' : (arg_0: UserNumber, arg_1: string) => Promise<
      VerifyTentativeDeviceResponse
    >,
}
