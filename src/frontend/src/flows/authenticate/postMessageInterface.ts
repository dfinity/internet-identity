import { Principal } from "@dfinity/principal";

export interface AuthRequest {
  kind: "authorize-client";
  sessionPublicKey: Uint8Array;
  maxTimeToLive?: bigint;
}

export interface AuthResponseSuccess {
  kind: "authorize-client-success";
  delegations: {
    delegation: {
      pubkey: Uint8Array;
      expiration: bigint;
      targets?: Principal[];
    };
    signature: Uint8Array;
  }[];
  userPublicKey: Uint8Array;
}

export class AuthContext {
  constructor(
    public authRequest: AuthRequest,
    public requestOrigin: string,
    public postMessageCallback: (message: AuthResponseSuccess) => void
  ) {}
}

// A message to signal that the II is ready to receive authorization requests.
export const READY_MESSAGE = {
  kind: "authorize-ready",
};
