import type { Principal } from "@dfinity/agent";
import type BigNumber from "bignumber.js";
export type Alias = string;
export type CredentialId = Array<number>;
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
  status_code: number;
}
export type PublicKey = Array<number>;
export type UserId = BigNumber;
export default interface _SERVICE {
  add: (
    arg_0: UserId,
    arg_1: Alias,
    arg_2: PublicKey,
    arg_3: [] | [CredentialId]
  ) => Promise<undefined>;
  http_request: (arg_0: HttpRequest) => Promise<HttpResponse>;
  lookup: (
    arg_0: UserId
  ) => Promise<Array<[Alias, PublicKey, [] | [CredentialId]]>>;
  register: (
    arg_0: UserId,
    arg_1: Alias,
    arg_2: PublicKey,
    arg_3: [] | [CredentialId]
  ) => Promise<undefined>;
  remove: (arg_0: UserId, arg_1: PublicKey) => Promise<undefined>;
}
