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
 * Set up an event listener to and wait for the authorize request from the client.
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
    return Promise.resolve(null);
  }
  return result;
}
