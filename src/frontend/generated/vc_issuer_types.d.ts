import type { Principal } from '@dfinity/principal';
import type { ActorMethod } from '@dfinity/agent';

export interface CredentialSpec { 'info' : string }
export interface GetCredentialRequest {
  'signed_id_alias' : SignedIdAlias,
  'prepared_context' : [] | [Uint8Array | number[]],
  'credential_spec' : CredentialSpec,
}
export type GetCredentialResponse = { 'ok' : IssuedCredentialData } |
  { 'err' : IssueCredentialError };
export interface Icrc21ConsentInfo {
  'consent_message' : string,
  'language' : string,
}
export interface Icrc21ConsentMessageRequest {
  'arg' : Uint8Array | number[],
  'method' : string,
  'preferences' : Icrc21ConsentPreferences,
}
export type Icrc21ConsentMessageResponse = { 'ok' : Icrc21ConsentInfo } |
  { 'err' : Icrc21Error };
export interface Icrc21ConsentPreferences { 'language' : string }
export type Icrc21Error = { 'generic_error' : Icrc21ErrorInfo } |
  { 'forbidden' : Icrc21ErrorInfo } |
  { 'not_supported' : Icrc21ErrorInfo } |
  { 'malformed_call' : Icrc21ErrorInfo };
export interface Icrc21ErrorInfo {
  'description' : string,
  'error_code' : bigint,
}
export type IssueCredentialError = { 'unauthorized_subject' : string } |
  { 'internal' : string } |
  { 'signature_not_found' : string } |
  { 'unknown_subject' : string } |
  { 'invalid_id_alias' : string };
export interface IssuedCredentialData { 'vc_jws' : string }
export interface PrepareCredentialRequest {
  'signed_id_alias' : SignedIdAlias,
  'credential_spec' : CredentialSpec,
}
export type PrepareCredentialResponse = { 'ok' : PreparedCredentialData } |
  { 'err' : IssueCredentialError };
export interface PreparedCredentialData {
  'prepared_context' : [] | [Uint8Array | number[]],
}
export interface SignedIdAlias {
  'credential_jws' : string,
  'id_alias' : Principal,
  'id_dapp' : Principal,
}
export interface _SERVICE {
  'add_employee' : ActorMethod<[Principal], string>,
  'consent_message' : ActorMethod<
    [Icrc21ConsentMessageRequest],
    Icrc21ConsentMessageResponse
  >,
  'get_credential' : ActorMethod<[GetCredentialRequest], GetCredentialResponse>,
  'prepare_credential' : ActorMethod<
    [PrepareCredentialRequest],
    PrepareCredentialResponse
  >,
}
