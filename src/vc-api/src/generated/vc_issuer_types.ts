import type { Principal } from '@dfinity/principal';
import type { ActorMethod } from '@dfinity/agent';
import type { IDL } from '@dfinity/candid';

export type ArgumentValue = { 'Int' : number } |
  { 'String' : string };
export interface CredentialSpec {
  'arguments' : [] | [Array<[string, ArgumentValue]>],
  'credential_type' : string,
}
export interface DerivationOriginData { 'origin' : string }
export type DerivationOriginError = { 'Internal' : string } |
  { 'UnsupportedOrigin' : string };
export interface DerivationOriginRequest { 'frontend_hostname' : string }
export interface GetCredentialRequest {
  'signed_id_alias' : SignedIdAlias,
  'prepared_context' : [] | [Uint8Array | number[]],
  'credential_spec' : CredentialSpec,
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
  'status_code' : number,
}
export interface Icrc21ConsentInfo {
  'consent_message' : string,
  'language' : string,
}
export interface Icrc21ConsentPreferences { 'language' : string }
export type Icrc21Error = {
    'GenericError' : { 'description' : string, 'error_code' : bigint }
  } |
  { 'UnsupportedCanisterCall' : Icrc21ErrorInfo } |
  { 'ConsentMessageUnavailable' : Icrc21ErrorInfo };
export interface Icrc21ErrorInfo { 'description' : string }
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
  'derivation_origin' : string,
  'idp_canister_ids' : Array<Principal>,
  'ic_root_key_der' : [] | [Uint8Array | number[]],
  'frontend_hostname' : string,
}
export interface PrepareCredentialRequest {
  'signed_id_alias' : SignedIdAlias,
  'credential_spec' : CredentialSpec,
}
export interface PreparedCredentialData {
  'prepared_context' : [] | [Uint8Array | number[]],
}
export interface SignedIdAlias { 'credential_jws' : string }
export interface _SERVICE {
  'add_adult' : ActorMethod<[Principal], string>,
  'add_employee' : ActorMethod<[Principal], string>,
  'add_graduate' : ActorMethod<[Principal], string>,
  'configure' : ActorMethod<[IssuerConfig], undefined>,
  'derivation_origin' : ActorMethod<
    [DerivationOriginRequest],
    { 'Ok' : DerivationOriginData } |
      { 'Err' : DerivationOriginError }
  >,
  'get_credential' : ActorMethod<
    [GetCredentialRequest],
    { 'Ok' : IssuedCredentialData } |
      { 'Err' : IssueCredentialError }
  >,
  'http_request' : ActorMethod<[HttpRequest], HttpResponse>,
  'prepare_credential' : ActorMethod<
    [PrepareCredentialRequest],
    { 'Ok' : PreparedCredentialData } |
      { 'Err' : IssueCredentialError }
  >,
  'set_alternative_origins' : ActorMethod<[string], undefined>,
  'set_derivation_origin' : ActorMethod<[string, string], undefined>,
  'vc_consent_message' : ActorMethod<
    [Icrc21VcConsentMessageRequest],
    { 'Ok' : Icrc21ConsentInfo } |
      { 'Err' : Icrc21Error }
  >,
}
export declare const idlFactory: IDL.InterfaceFactory;
export declare const init: (args: { IDL: typeof IDL }) => IDL.Type[];
