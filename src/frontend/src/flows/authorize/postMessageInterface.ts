// Types and functions related to the window post message interface used by
// applications that want to authenticate the user using Internet Identity
import { Principal } from "@dfinity/principal";
import { z } from "zod";
import { Delegation } from "./fetchDelegation";

// The type of messages that kick start the flow (II -> RP)
export const AuthReady = {
  kind: "authorize-ready",
};

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
          "maxTimeToLive is 'number' but should be 'bigint', this will be an error in the future"
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
      delegations: Delegation[];
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
        delegations: Delegation[];
        userPublicKey: Uint8Array;
        authnMethod: "pin" | "passkey" | "recovery";
      }
    | { kind: "failure"; text: string }
  >;
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
  window.opener.postMessage(AuthReady, "*");

  onProgress("waiting");

  const { origin, request } = await waitForRequest();
  const authContext = { authRequest: request, requestOrigin: origin };

  onProgress("validating");

  const result = await authenticate(authContext);

  if (result.kind === "failure") {
    window.opener.postMessage({
      kind: "authorize-client-failure",
      text: result.text,
    } satisfies AuthResponse);
    return "failure";
  }

  result.kind satisfies "success";

  window.opener.postMessage(
    {
      kind: "authorize-client-success",
      delegations: result.delegations,
      userPublicKey: result.userPublicKey,
      authnMethod: result.authnMethod,
    } satisfies AuthResponse,
    authContext.requestOrigin
  );

  return "success";
}

// Wait for a request to kickstart the flow
const waitForRequest = (): Promise<{
  request: AuthRequest;
  origin: string;
}> => {
  return new Promise((resolve) => {
    const messageEventHandler = (evnt: MessageEvent) => {
      if (evnt.origin === window.location.origin) {
        // Ignore messages from own origin (e.g. from browser extensions)
        console.warn("Ignoring message from own origin", evnt);
        return;
      }
      const message: unknown = evnt.data;
      const result = AuthRequest.safeParse(message);

      if (!result.success) {
        const message = `Unexpected error: flow request ` + result.error;
        console.error(message);
        // XXX: here we just wait further assuming the opener might recover
        // and send a correct request
        return;
      }

      window.removeEventListener("message", messageEventHandler);

      resolve({ request: result.data, origin: evnt.origin });
    };

    // Set up an event listener for receiving messages from the client.
    window.addEventListener("message", messageEventHandler);
  });
};
