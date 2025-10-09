import type { Principal } from "@dfinity/principal";
import type { ActorMethod } from "@dfinity/agent";
import type { IDL } from "@dfinity/candid";

export type ArgumentValue = { "Int": number } |
  { "String": string };

/**
 * Candid interface of the example VC issuer canister.
 * The interface contains both the functionality required by the VC-spec
 * (https://github.com/dfinity/internet-identity/blob/main/docs/vc-spec.md)
 * and additional APIs for configuring the canister and using it for testing.
 * Specification of a requested credential.
 */
export interface CredentialSpec {
  /**
   * arguments are optional, and specific to the credential_name
   */
  "arguments": [] | [Array<[string, ArgumentValue]>],
  "credential_type": string,
}

export interface DerivationOriginData {"origin": string;}

export type DerivationOriginError = { "Internal": string } |
  { "UnsupportedOrigin": string };

/**
 * Types for `derivation_origin`.
 */
export interface DerivationOriginRequest {"frontend_hostname": string;}

/**
 * Types for `get_credential`.
 */
export interface GetCredentialRequest {
  "signed_id_alias": SignedIdAlias,
  "prepared_context": [] | [Uint8Array | number[]],
  "credential_spec": CredentialSpec,
}

/**
 * Options related to HTTP handling
 */
export type HeaderField = [string, string];

export interface HttpRequest {
  "url": string,
  "method": string,
  "body": Uint8Array | number[],
  "headers": Array<HeaderField>,
  "certificate_version": [] | [number],
}

export interface HttpResponse {
  "body": Uint8Array | number[],
  "headers": Array<HeaderField>,
  "status_code": number,
}

/**
 * Types for ICRC-21 consent message, cf.
 * https://github.com/dfinity/wg-identity-authentication/blob/main/topics/icrc_21_consent_msg.md
 */
export interface Icrc21ConsentInfo {
  "consent_message": string,
  "language": string,
}

export interface Icrc21ConsentPreferences {"language": string;}

export type Icrc21Error = {
  "GenericError": { "description": string, "error_code": bigint }
} |
  { "UnsupportedCanisterCall": Icrc21ErrorInfo } |
  { "ConsentMessageUnavailable": Icrc21ErrorInfo };

export interface Icrc21ErrorInfo {"description": string;}

export interface Icrc21VcConsentMessageRequest {
  "preferences": Icrc21ConsentPreferences,
  "credential_spec": CredentialSpec,
}

export type IssueCredentialError = { "Internal": string } |
  {
    /**
     * Internal errors, indicate malfunctioning of the issuer.
     */
    "SignatureNotFound": string
  } |
  {
    /**
     * The id_alias credential provided by the identity provider is invalid.
     */
    "InvalidIdAlias": string
  } |
  {
    /**
     * The caller is not authorized to obtain the requested credential.  Caller requested a credential
     * for a different principal, or the issuer does not have sufficient knowledge about the caller
     * to issue the requested credential.
     */
    "UnauthorizedSubject": string
  } |
  {
    /**
     * The caller is not known to the issuer.  Caller should register first with the issuer before retrying.
     */
    "UnknownSubject": string
  } |
  {
    /**
     * The issuer does not issue credentials described in the credential spec.
     */
    "UnsupportedCredentialSpec": string
  };

export interface IssuedCredentialData {"vc_jws": string;}

/**
 * Configuration specific to this issuer.
 */
export interface IssuerConfig {
  /**
   * The derivation origin to be used by the issuer.
   */
  "derivation_origin": string,
  /**
   * List of canister ids that are allowed to provide id alias credentials.
   */
  "idp_canister_ids": Array<Principal>,
  /**
   * Root of trust for checking canister signatures.
   */
  "ic_root_key_der": [] | [Uint8Array | number[]],
  /**
   * Frontend hostname be used by the issuer.
   */
  "frontend_hostname": string,
}

/**
 * Types for requesting issuance of a credential.
 * The issuance proceeds in two steps:
 * - `prepare_credential`, and
 * - `get_credential`
 * where the split of work between the two steps depends on the specifics of the issuer,
 * and the second second step returns the actual credential (if any).
 * The two steps can use `prepared_context`-value to transfer information between them.
 * Types for `prepare_credential`.
 */
export interface PrepareCredentialRequest {
  "signed_id_alias": SignedIdAlias,
  "credential_spec": CredentialSpec,
}

export interface PreparedCredentialData {
  "prepared_context": [] | [Uint8Array | number[]],
}

export interface SignedIdAlias {"credential_jws": string;}

export interface _SERVICE {
  "add_adult": ActorMethod<[Principal], string>,
  /**
   * API for obtaining information about users, for testing only.
   * In a real-world issuer the data acquisition functionality should be more elaborate and authenticated.
   */
  "add_employee": ActorMethod<[Principal], string>,
  "add_graduate": ActorMethod<[Principal], string>,
  /**
   * Configure the issuer (e.g. set the root key), used for deployment/testing.
   */
  "configure": ActorMethod<[IssuerConfig], undefined>,
  "derivation_origin": ActorMethod<
    [DerivationOriginRequest],
    { "Ok": DerivationOriginData } |
    { "Err": DerivationOriginError }
  >,
  "get_credential": ActorMethod<
    [GetCredentialRequest],
    { "Ok": IssuedCredentialData } |
    { "Err": IssueCredentialError }
  >,
  /**
   * Serve the app
   */
  "http_request": ActorMethod<[HttpRequest], HttpResponse>,
  "prepare_credential": ActorMethod<
    [PrepareCredentialRequest],
    { "Ok": PreparedCredentialData } |
    { "Err": IssueCredentialError }
  >,
  /**
   * Sets the content of the alternative origins file.
   */
  "set_alternative_origins": ActorMethod<[string], undefined>,
  "set_derivation_origin": ActorMethod<[string, string], undefined>,
  /**
   * VC-flow API.
   */
  "vc_consent_message": ActorMethod<
    [Icrc21VcConsentMessageRequest],
    { "Ok": Icrc21ConsentInfo } |
    { "Err": Icrc21Error }
  >,
}

export declare const idlFactory: IDL.InterfaceFactory;
export declare const init: (args: { IDL: typeof IDL }) => IDL.Type[];
