declare module "dfx-generated/idp_service" {
  import { Principal, PublicKey } from "@dfinity/agent";
  import BigNumber from "bignumber.js";

  export interface HttpResponse {
    method: string;
    url: string;
    headers: { [key: string]: string }[];
    body: Blob;
  }
  export interface HttpResponse {
    status_code: number;
    headers: { [key: string]: string }[];
    body: Blob;
  }

  export interface LookUp {
    alias: string;
    publicKey: number[];
    credentialId?: number[];
  }
  interface IDPService {
    register: (
      userId: string,
      publicKey: number[],
      credentialId?: number[]
    ) => Promise<null>;
    add: (
      userId: string,
      publicKey: number[],
      credentialId?: number[]
    ) => Promise<null>;
    remove: (userId: string, publicKey: number[]) => Promise<null>;
    lookup: (userId: string) => Promise<any>;
    http_request: () => Promise<any>;
  }
  const IDPService: IDPService;
  export default IDPService;
}
