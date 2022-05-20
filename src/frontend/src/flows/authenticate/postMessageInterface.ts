import { Principal } from "@dfinity/principal";
import { UserNumber } from "../../../generated/internet_identity_types";
import { IIConnection } from "../../utils/iiConnection";
import { handleAuthRequest } from "./fetchDelegation";

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

export interface AuthResponseFailure {
  kind: "authorize-client-failure";
  text: string;
}

export type AuthResponse = AuthResponseSuccess | AuthResponseFailure;

// A message to signal that the II is ready to receive authorization requests.
const READY_MESSAGE = {
  kind: "authorize-ready",
};

/**
 * Setup an event listener to listen to authorize requests from the client.
 *
 * This method expects to be called after the login flow.
 */
export default async function setup(
  userNumber: UserNumber,
  connection: IIConnection
): Promise<void> {
  // Set up an event listener for receiving messages from the client.
  window.addEventListener("message", async (event) => {
    const message = event.data;
    if (message.kind === "authorize-client") {
      console.log("Handling authorize-client request.");
      const response = await handleAuthRequest(
        connection,
        userNumber,
        message,
        event.origin
      );
      (event.source as Window).postMessage(response, event.origin);
    } else {
      console.log(`Message of unknown kind received: ${message}`);
    }
  });

  // Send a message to indicate we're ready.
  // NOTE: Because `window.opener.origin` cannot be accessed, this message
  // is sent with "*" as the target origin. This is safe as no sensitive
  // information is being communicated here.
  if (window.opener !== null) {
    window.opener.postMessage(READY_MESSAGE, "*");
  } else {
    // If there's no `window.opener` a user has manually navigated to "/#authorize". Let's
    // redirect them to the non-hash version.
    window.location.hash = "";
    window.location.reload();
  }
}
