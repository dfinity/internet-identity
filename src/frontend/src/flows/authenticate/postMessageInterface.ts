// Types and functions related to the window post message interface used by
// applications that want to authenticate the user using Internet Identity
import { Principal } from "@dfinity/principal";

import { validateDerivationOrigin } from "./validateDerivationOrigin";

import { PublicKey } from "../../../generated/internet_identity_types";
import { AuthenticatedConnection } from "../../utils/iiConnection";

export interface AuthSuccess {
  userNumber: bigint;
  connection: AuthenticatedConnection;
  parsedSignedDelegation: Delegation;
  userKey: PublicKey;
}

export interface AuthResponseSuccess {
  kind: "authorize-client-success";
  delegations: Delegation[];
  userPublicKey: Uint8Array;
}

export interface Delegation {
  delegation: {
    pubkey: Uint8Array;
    expiration: bigint;
    targets?: Principal[];
  };
  signature: Uint8Array;
}

/**
 * All information required to process an authentication request received from
 * a client application.
 */
export interface AuthContext {
  /**
   * Information sent by the client application.
   */
  authRequest: AuthRequest;
  /**
   * Origin of the message.
   */
  requestOrigin: string;
}

export interface AuthRequest {
  kind: "authorize-client";
  sessionPublicKey: Uint8Array;
  maxTimeToLive?: bigint;
  derivationOrigin?: string;
}

/**
 * The postMessage-based authentication protocol.
 */
export async function authenticationProtocol({
  authenticate,
  onInvalidOrigin,
  onProgress,
}: {
  /** The callback used to get auth data (i.e. select or create anchor) */
  authenticate: (authContext: AuthContext) => Promise<AuthSuccess>;
  /** Callback used to show an "invalid origin" error. At this point the authentication protocol is not over so we use a callback to regain control afterwards. */
  onInvalidOrigin: (opts: {
    authContext: AuthContext;
    message: string;
  }) => Promise<void>;
  /* Progress update messages to let the user know what's happening. */
  onProgress: (state: "waiting" | "validating") => void;
}): Promise<"orphan" | "success" | "failure"> {
  if (window.opener === null) {
    // If there's no `window.opener` a user has manually navigated to "/#authorize".
    // Signal that there will never be an authentication request incoming.
    return "orphan";
  }

  // Send a message to indicate we're ready.
  // NOTE: Because `window.opener.origin` cannot be accessed, this message
  // is sent with "*" as the target origin. This is safe as no sensitive
  // information is being communicated here.
  window.opener.postMessage({ kind: "authorize-ready" }, "*");

  onProgress("waiting");

  const authContext = await waitForAuthRequest();

  onProgress("validating");

  const validationResult = await validateDerivationOrigin(
    authContext.requestOrigin,
    authContext.authRequest.derivationOrigin
  );

  if (validationResult.result === "invalid") {
    await onInvalidOrigin({ message: validationResult.message, authContext });
    // notify the client application
    // do this after showing the error because the client application might close the window immediately after receiving the message and might not show the user what's going on
    window.opener.postMessage(
      {
        kind: "authorize-client-failure",
        text: `Invalid derivation origin: ${validationResult.message}`,
      },
      authContext.requestOrigin
    );

    return "failure";
  }

  const authSuccess = await authenticate(authContext);

  window.opener.postMessage(
    {
      kind: "authorize-client-success",
      delegations: [authSuccess.parsedSignedDelegation],
      userPublicKey: Uint8Array.from(authSuccess.userKey),
    },
    authContext.requestOrigin
  );

  return "success";
}

/**
 * Wait for client to request authentication.
 */
const waitForAuthRequest = (): Promise<AuthContext> =>
  new Promise<AuthContext>((resolve) => {
    const eventHandler = async (event: MessageEvent) => {
      const message = event.data;
      if (message.kind === "authorize-client") {
        window.removeEventListener("message", eventHandler);
        console.log(
          `Handling authorize-client request ${JSON.stringify(message, (_, v) =>
            typeof v === "bigint" ? v.toString() : v
          )}`
        );
        resolve({
          authRequest: message,
          requestOrigin: event.origin,
        });
      } else {
        console.warn(
          `Message of unknown kind received: ${JSON.stringify(message)}`
        );
      }
    };

    // Set up an event listener for receiving messages from the client.
    window.addEventListener("message", eventHandler);
  });
