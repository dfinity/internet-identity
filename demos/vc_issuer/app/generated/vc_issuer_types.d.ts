import type { Principal } from '@dfinity/principal';
import type { ActorMethod } from '@dfinity/agent';
import type { IDL } from '@dfinity/candid';

export type ArgumentValue = { 'Int' : number } |
  { 'String' : string };
export interface CredentialSpec {
  'arguments' : [] | [Array<[string, ArgumentValue]>],
  'credential_type' : string,
}
export interface GetCredentialRequest {
  'signed_id_alias' : SignedIdAlias,
  'prepared_context' : [] | [Uint8Array | number[]],
  'credential_spec' : CredentialSpec,
}
export type GetCredentialResponse = { 'Ok' : IssuedCredentialData } |
  { 'Err' : IssueCredentialError };
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
  'status_code' : number,
}
export interface Icrc21ConsentInfo {
  'consent_message' : string,
  'language' : string,
}
export type Icrc21ConsentMessageResponse = { 'Ok' : Icrc21ConsentInfo } |
  { 'Err' : Icrc21Error };
export interface Icrc21ConsentPreferences { 'language' : string }
export type Icrc21Error = { 'GenericError' : Icrc21ErrorInfo } |
  { 'UnsupportedCanisterCall' : Icrc21ErrorInfo } |
  { 'ConsentMessageUnavailable' : Icrc21ErrorInfo };
export interface Icrc21ErrorInfo {
  'description' : string,
  'error_code' : bigint,
}
export interface Icrc21VcConsentMessageRequest {
  'preferences' : Icrc21ConsentPreferences,
  'credential_spec' : CredentialSpec,
}
export type IssueCredentialError = { 'Internal' : string } |
  { 'SignatureNotFound' : string } |
  { 'InvalidIdAlias' : string } |
  { 'UnauthorizedSubject' : string } |
  { 'UnknownSubject' : string } |
  { 'UnsupportedCredentialSpec' : string };
export interface IssuedCredentialData { 'vc_jws' : string }
export interface IssuerConfig {
  'idp_canister_ids' : Array<Principal>,
  'ic_root_key_der' : Uint8Array | number[],
}
export interface PrepareCredentialRequest {
  'signed_id_alias' : SignedIdAlias,
  'credential_spec' : CredentialSpec,
}
export type PrepareCredentialResponse = { 'Ok' : PreparedCredentialData } |
  { 'Err' : IssueCredentialError };
export interface PreparedCredentialData {
  'prepared_context' : [] | [Uint8Array | number[]],
}
export interface SignedIdAlias { 'credential_jws' : string }
export interface _SERVICE {
  'add_employee' : ActorMethod<[Principal], string>,
  'add_graduate' : ActorMethod<[Principal], string>,
  'configure' : ActorMethod<[IssuerConfig], undefined>,
  'get_credential' : ActorMethod<[GetCredentialRequest], GetCredentialResponse>,
  'http_request' : ActorMethod<[HttpRequest], HttpResponse>,
  'prepare_credential' : ActorMethod<
    [PrepareCredentialRequest],
    PrepareCredentialResponse
  >,
  'vc_consent_message' : ActorMethod<
    [Icrc21VcConsentMessageRequest],
    Icrc21ConsentMessageResponse
  >,
}
export declare const idlFactory: IDL.InterfaceFactory;
