import type { Principal } from "@dfinity/agent";
export type Alias = string;
export type CredentialId = Array<number>;
export interface Delegation {
  pubkey: PublicKey;
  targets: [] | [Array<Principal>];
  expiration: Timestamp;
}
export type GetDelegationResponse =
  | { delegation: SignedDelegation }
  | { request_delegation_explicitly: null };
export type HeaderField = [string, string];
export interface HttpRequest {
  url: string;
  method: string;
  body: Array<number>;
  headers: Array<HeaderField>;
}
export interface HttpResponse {
  body: Array<number>;
  headers: Array<HeaderField>;
  streaming_strategy: [] | [StreamingStrategy];
  status_code: number;
}
export type PublicKey = Array<number>;
export interface SignedDelegation {
  signature: Array<number>;
  delegation: Delegation;
}
export interface StreamingCallbackHttpResponse {
  token: [] | [Token];
  body: Array<number>;
}
export type StreamingStrategy = {
  Callback: { token: Token; callback: [Principal, string] };
};
export type Timestamp = bigint;
export interface Token {}
export type UserId = bigint;
export default interface _SERVICE {
  add: (
    arg_0: UserId,
    arg_1: Alias,
    arg_2: PublicKey,
    arg_3: [] | [CredentialId]
  ) => Promise<undefined>;
  get_delegation: (
    arg_0: UserId,
    arg_1: PublicKey
  ) => Promise<GetDelegationResponse>;
  http_request: (arg_0: HttpRequest) => Promise<HttpResponse>;
  lookup: (
    arg_0: UserId
  ) => Promise<Array<[Alias, PublicKey, Timestamp, [] | [CredentialId]]>>;
  register: (
    arg_0: Alias,
    arg_1: PublicKey,
    arg_2: [] | [CredentialId]
  ) => Promise<UserId>;
  remove: (arg_0: UserId, arg_1: PublicKey) => Promise<undefined>;
  request_delegation: (arg_0: UserId, arg_1: PublicKey) => Promise<undefined>;
}
