// Types and functions related to the window post message interface used by
// applications that want to authenticate the user using Internet Identity
import { Principal } from "@dfinity/principal";
import { fetchDelegation } from "./fetchDelegation";
import { validateDerivationOrigin } from "./validateDerivationOrigin";
import { unknownToRecord } from "../../utils/utils";
import { LoginData } from "../../utils/flowResult";

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

/** Try to read unknown data as authentication request */
const asAuthRequest = (msg: unknown): AuthRequest | string => {
  const obj = unknownToRecord(msg);

  if (obj === undefined) {
    return "request is undefined";
  }

  if (!("kind" in obj)) {
    return "request does not have 'kind'";
  }

  if (obj.kind !== "authorize-client") {
    return "'kind' is not 'authorize-client'";
  }

  if (!("sessionPublicKey" in obj)) {
    return "request does not have 'sessionPublicKey'";
  }

  if (!(obj.sessionPublicKey instanceof Uint8Array)) {
    return "'sessionPublicKey' is not 'Uint8Array'";
  }

  // Temporary work around for clients that use 'number' instead of 'bigint'
  // https://github.com/dfinity/internet-identity/issues/1050
  let maxTimeToLive = obj.maxTimeToLive;
  if (typeof maxTimeToLive === "number") {
    console.warn(
      "maxTimeToLive is 'number' but should be 'bigint', this will be an error in the future"
    );
    maxTimeToLive = BigInt(maxTimeToLive);
  }

  if (
    typeof maxTimeToLive !== "undefined" &&
    typeof maxTimeToLive !== "bigint"
  ) {
    return "'maxTimeToLive' is not 'bigint'";
  }

  const derivationOrigin = obj.derivationOrigin;
  if (
    typeof derivationOrigin !== "undefined" &&
    typeof derivationOrigin !== "string"
  ) {
    return "'derivationOrigin' is not 'string'";
  }

  return {
    kind: obj.kind,
    sessionPublicKey: obj.sessionPublicKey,
    maxTimeToLive,
    derivationOrigin,
  };
};

/**
 * The postMessage-based authentication protocol.
 */
export async function authenticationProtocol({
  authenticate,
  onInvalidOrigin,
  onProgress,
}: {
  /** The callback used to get auth data (i.e. select or create anchor) */
  authenticate: (authContext: AuthContext) => Promise<LoginData>;
  /** Callback used to show an "invalid origin" error. At this point the authentication protocol is not over so we use a callback to regain control afterwards. */
  onInvalidOrigin: (opts: {
    authContext: AuthContext;
    message: string;
  }) => Promise<void>;
  /* Progress update messages to let the user know what's happening. */
  onProgress: (state: "waiting" | "validating" | "fetching delegation") => void;
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

  onProgress("fetching delegation");

  const [userKey, parsed_signed_delegation] = await fetchDelegation(
    authSuccess.userNumber,
    authSuccess.connection,
    authContext
  );

  window.opener.postMessage(
    {
      kind: "authorize-client-success",
      delegations: [parsed_signed_delegation],
      userPublicKey: Uint8Array.from(userKey),
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
      const message: unknown = event.data; // Drop assumptions about event.data (an 'any')
      const authRequest = asAuthRequest(message);
      if (typeof authRequest !== "string") {
        window.removeEventListener("message", eventHandler);
        console.log(
          `Handling authorize-client request ${JSON.stringify(
            authRequest,
            (_, v) => (typeof v === "bigint" ? v.toString() : v)
          )}`
        );
        resolve({
          authRequest,
          requestOrigin: event.origin,
        });
      } else {
        console.warn(
          `Bad authentication request received: ${authRequest}`,
          message
        );
      }
    };

    // Set up an event listener for receiving messages from the client.
    window.addEventListener("message", eventHandler);
  });
