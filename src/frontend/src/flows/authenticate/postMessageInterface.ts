import { Principal } from "@dfinity/principal";

export interface AuthRequest {
  kind: "authorize-client";
  sessionPublicKey: Uint8Array;
  maxTimeToLive?: bigint;
}

export interface Delegation {
  delegation: {
    pubkey: Uint8Array;
    expiration: bigint;
    targets?: Principal[];
  };
  signature: Uint8Array;
}

export interface AuthResponseSuccess {
  kind: "authorize-client-success";
  delegations: Delegation[];
  userPublicKey: Uint8Array;
}

export interface AuthResponseFailure {
  kind: "authorize-client-failure";
  text: string;
}

export type AuthResponse = AuthResponseSuccess | AuthResponseFailure;

// A message to signal that the II is ready to receive authorization requests.
export const READY_MESSAGE = {
  kind: "authorize-ready",
};
