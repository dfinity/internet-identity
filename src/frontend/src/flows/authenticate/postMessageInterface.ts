// Types and functions related to the window post message interface used by
// applications that want to authenticate the user using Internet Identity
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

/**
 * All information required to process an authentication request received from
 * a client application.
 */
export class AuthContext {
  constructor(
    /**
     * Information sent by the client application.
     */
    public authRequest: AuthRequest,
    /**
     * Origin of the message.
     */
    public requestOrigin: string,
    /**
     * Callback to send a result back to the sender. We currently only send
     * either a success message or nothing at all.
     */
    public postMessageCallback: (message: AuthResponseSuccess) => void
  ) {}
}

// A message to signal that the II is ready to receive authorization requests.
export const READY_MESSAGE = {
  kind: "authorize-ready",
};

export function delegationMessage(
  parsed_signed_delegation: Delegation,
  userKey: Array<number>
): AuthResponseSuccess {
  return {
    kind: "authorize-client-success",
    delegations: [parsed_signed_delegation],
    userPublicKey: Uint8Array.from(userKey),
  };
}

/**
 * Set up an event listener for window post messages and wait for the authorize
 * authentication request from the client application.
 */
export default async function waitForAuthRequest(): Promise<AuthContext | null> {
  const result = new Promise<AuthContext>((resolve, reject) => {
    // Set up an event listener for receiving messages from the client.
    window.addEventListener("message", async (event) => {
      const message = event.data;
      if (message.kind === "authorize-client") {
        console.log("Handling authorize-client request.");
        resolve(
          new AuthContext(message, event.origin, (responseMessage) =>
            (event.source as Window).postMessage(responseMessage, event.origin)
          )
        );
      } else {
        console.error(
          `Message of unknown kind received: ${JSON.stringify(message)}`
        );
        reject();
      }
    });
  });

  // Send a message to indicate we're ready.
  // NOTE: Because `window.opener.origin` cannot be accessed, this message
  // is sent with "*" as the target origin. This is safe as no sensitive
  // information is being communicated here.
  if (window.opener !== null) {
    window.opener.postMessage(READY_MESSAGE, "*");
  } else {
    // If there's no `window.opener` a user has manually navigated to "/#authorize".
    // Signal that there will never be an authentication request incoming.
    return Promise.resolve(null);
  }
  return result;
}
