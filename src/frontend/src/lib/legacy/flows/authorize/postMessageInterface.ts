// Types and functions related to the window post message interface used by
// applications that want to authenticate the user using Internet Identity
import {
  AuthenticationV2Events,
  authenticationV2Funnel,
} from "$lib/utils/analytics/authenticationV2Funnel";
import {
  authorizeClientFunnel,
  AuthorizeClientEvents,
} from "$lib/utils/analytics/authorizeClientFunnel";
import { loginFunnel } from "$lib/utils/analytics/loginFunnel";
import { registrationFunnel } from "$lib/utils/analytics/registrationFunnel";
import { type SignedDelegation as FrontendSignedDelegation } from "@dfinity/identity";
import { Principal } from "@dfinity/principal";
import { z } from "zod";

// The type of messages that kick start the flow (II -> RP)
export const AuthReady = {
  kind: "authorize-ready",
};

// If the relying party hasn't sent a request in 10 seconds, we should assume
// something went wrong and that we're likely aren't going to receive any.
const TIMEOUT_WAIT_FOR_REQUEST = 10000;

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

const zodPrincipal = z.string().transform((val, ctx) => {
  let principal;
  try {
    principal = Principal.fromText(val);
  } catch {
    ctx.addIssue({ code: z.ZodIssueCode.custom, message: "Not a principal " });
    return z.NEVER;
  }
  return principal;
});

export const AuthRequest = z.object({
  kind: z.literal("authorize-client"),
  sessionPublicKey: z.instanceof(Uint8Array),
  maxTimeToLive: z
    .optional(z.union([z.number(), z.bigint()]))
    .transform((val) => {
      if (typeof val === "number") {
        // Temporary work around for clients that use 'number' instead of 'bigint'
        // https://github.com/dfinity/internet-identity/issues/1050
        console.warn(
          "maxTimeToLive is 'number' but should be 'bigint', this will be an error in the future",
        );
        return BigInt(val);
      }
      return val;
    }),
  derivationOrigin: z.optional(z.string()),
  allowPinAuthentication: z.optional(z.boolean()),
  autoSelectionPrincipal: z.optional(zodPrincipal),
});

export type AuthRequest = z.output<typeof AuthRequest>;

export type AuthResponse =
  | {
      kind: "authorize-client-failure";
      text: string;
    }
  | {
      kind: "authorize-client-success";
      delegations: FrontendSignedDelegation[];
      userPublicKey: Uint8Array;
      authnMethod: "pin" | "passkey" | "recovery";
    };

/**
 * The postMessage-based authentication protocol.
 */
export async function authenticationProtocol({
  authenticate,
  onProgress,
}: {
  /** The callback used to get auth data (i.e. select or create anchor) */
  authenticate: (authContext: {
    authRequest: AuthRequest;
    requestOrigin: string;
  }) => Promise<
    | {
        kind: "success";
        delegations: FrontendSignedDelegation[];
        userPublicKey: Uint8Array;
        authnMethod: "pin" | "passkey" | "recovery";
      }
    | { kind: "failure"; text: string }
    | { kind: "unverified-origin"; text: string }
  >;
  /* Progress update messages to let the user know what's happening. */
  onProgress: (state: "waiting" | "validating") => void;
}): Promise<
  "orphan" | "closed" | "invalid" | "success" | "failure" | "unverified-origin"
> {
  authorizeClientFunnel.init();

  if (window.opener === null) {
    if (window.history.length > 1) {
      // If there's no `window.opener` and a user has manually navigated to "/#authorize".
      // Signal that there will never be an authentication request incoming.
      authorizeClientFunnel.trigger(AuthorizeClientEvents.Orphan);
      return "orphan";
    }
    // Else signal that the connection has been unexpectedly closed.
    authorizeClientFunnel.trigger(AuthorizeClientEvents.Closed);
    return "closed";
  }

  // Send a message to indicate we're ready.
  // NOTE: Because `window.opener.origin` cannot be accessed, this message
  // is sent with "*" as the target origin. This is safe as no sensitive
  // information is being communicated here.
  window.opener.postMessage(AuthReady, "*");

  onProgress("waiting");

  const requestResult = await waitForRequest();
  if (requestResult.kind === "timeout") {
    authorizeClientFunnel.trigger(AuthorizeClientEvents.RequestTimeout);
    return "closed";
  }
  if (requestResult.kind === "invalid") {
    authorizeClientFunnel.trigger(AuthorizeClientEvents.RequestInvalid);
    return "invalid";
  }
  void (requestResult.kind satisfies "received");
  authorizeClientFunnel.trigger(AuthorizeClientEvents.RequestReceived);
  const requestOrigin =
    requestResult.request.derivationOrigin ?? requestResult.origin;
  loginFunnel.init({ origin: requestOrigin });
  registrationFunnel.init({ origin: requestOrigin });
  authenticationV2Funnel.init({ origin: requestOrigin });

  const authContext = {
    authRequest: requestResult.request,
    requestOrigin: requestResult.origin,
  };

  onProgress("validating");

  let authenticateResult;
  // This should not fail, but there is a big drop-off in the funnel here.
  // It most probably means users closing the window, but we should investigate.
  try {
    authenticateResult = await authenticate(authContext);
    authorizeClientFunnel.trigger(AuthorizeClientEvents.Authenticate);
    authenticationV2Funnel.trigger(AuthenticationV2Events.AuthSuccess);
  } catch (error: unknown) {
    console.error("Unexpected error during authentication", error);
    authenticateResult = {
      kind: "failure" as const,
      text: "There was an unexpected error, please try again.",
    };
  }

  if (
    authenticateResult.kind === "failure" ||
    authenticateResult.kind === "unverified-origin"
  ) {
    authorizeClientFunnel.trigger(AuthorizeClientEvents.AuthenticateError, {
      origin: requestOrigin,
      failureReason: authenticateResult.text,
    });
    window.opener.postMessage({
      kind: "authorize-client-failure",
      text: authenticateResult.text,
    } satisfies AuthResponse);
    return authenticateResult.kind;
  }
  void (authenticateResult.kind satisfies "success");
  authorizeClientFunnel.trigger(AuthorizeClientEvents.AuthenticateSuccess);

  window.opener.postMessage(
    {
      kind: "authorize-client-success",
      delegations: authenticateResult.delegations,
      userPublicKey: authenticateResult.userPublicKey,
      authnMethod: authenticateResult.authnMethod,
    } satisfies AuthResponse,
    authContext.requestOrigin,
  );

  return "success";
}

// Wait for a request to kickstart the flow
const waitForRequest = (): Promise<
  | {
      kind: "received";
      request: AuthRequest;
      origin: string;
    }
  | { kind: "timeout" }
  | { kind: "invalid" }
> => {
  return new Promise((resolve) => {
    const timeout = setTimeout(
      () => resolve({ kind: "timeout" }),
      TIMEOUT_WAIT_FOR_REQUEST,
    );
    const messageEventHandler = (event: MessageEvent) => {
      if (event.origin === window.location.origin) {
        // Ignore messages from own origin (e.g. from browser extensions)
        console.warn("Ignoring message from own origin", event);
        return;
      }
      const message: unknown = event.data;
      const result = AuthRequest.safeParse(message);

      if (!result.success) {
        const message = `Unexpected error: flow request ` + result.error;
        console.error(message);

        // The relying party is not made aware that the request is invalid,
        // so we should assume the relying party will wait forever.
        //
        // Let's at least indicate to the user that the request was invalid,
        // so they can communicate this with the relying party developer.
        clearTimeout(timeout);
        window.removeEventListener("message", messageEventHandler);

        resolve({ kind: "invalid" });
        return;
      }

      clearTimeout(timeout);
      window.removeEventListener("message", messageEventHandler);

      resolve({ kind: "received", request: result.data, origin: event.origin });
    };

    // Set up an event listener for receiving messages from the client.
    window.addEventListener("message", messageEventHandler);
  });
};
