import { BinaryBlob, blobFromUint8Array, Principal } from "@dfinity/agent";
import { FrontendHostname, GetDelegationResponse, PublicKey, SignedDelegation, UserNumber } from "../generated/idp_types";
import { withLoader } from "./components/loader";
import { confirmRedirect } from "./flows/confirmRedirect";
import { IDPActor } from "./utils/idp_actor";

interface AuthRequest {
    kind: "authorize-client";
    sessionPublicKey: Uint8Array;
    maxTimeToLive?: BigInt;
}

interface AuthResponseSuccess {
    kind: "authorize-client-success";
    delegations: {
      delegation: {
        pubkey: Uint8Array;
        expiration: BigInt;
        targets?: Principal[];
      };
      signature: Uint8Array;
    }[];
    userPublicKey: Uint8Array;
}

interface AuthResponseFailure {
    kind: "authorize-client-failure";
    text: string;
}

type AuthResponse = AuthResponseSuccess | AuthResponseFailure;

// A message to signal that the IDP is ready to receive authorization requests.
const READY_MESSAGE = {
  kind: "authorize-ready"
};

/**
 * Setup an event listener to listen to authorize requests from the client.
 *
 * This method expects to be called after the login flow.
 */
export default async function setup(userNumber: UserNumber, connection: IDPActor) {
  // Set up an event listener for receiving messages from the client.
  window.addEventListener("message", async (event) => {
    const message = event.data;
    if (message.kind === "authorize-client") {
      console.log("Handling authorize-client request.");
      const response = await handleAuthRequest(connection, userNumber, message, event.origin);
      (event.source as Window).postMessage(response, event.origin);
    } else {
      console.log(`Message of unknown kind received: ${message}`)
    }
  });

  // Send a message to indicate we're ready.
  // NOTE: Because `window.opener.origin` cannot be accessed, this message
  // is sent with "*" as the target origin. This is safe as no sensitive
  // information is being communicated here.
  window.opener.postMessage(READY_MESSAGE, "*");
}

async function handleAuthRequest(
    connection: IDPActor,
    userNumber: UserNumber,
    request: AuthRequest,
    hostname: FrontendHostname): Promise<AuthResponse> {

  if (!await confirmRedirect(hostname)) {
    return {
        kind: "authorize-client-failure",
        text: `User did not grant access to ${hostname}.`
    };
  }

  return await withLoader(async () => {
    const sessionKey = Array.from(blobFromUint8Array(request.sessionPublicKey));
    const prepRes = await connection.prepareDelegation(
        userNumber,
        hostname,
        sessionKey
    );
    if (!prepRes || prepRes.length != 2) {
        throw new Error(
        `Error preparing the delegation. Result received: ${prepRes}`
        );
    }

    const [userKey, timestamp] = prepRes;

    // TODO: Signal failure to retrieve the delegation. Error page, or maybe redirect back with error?
    const signed_delegation = await retryGetDelegation(
      connection,
      userNumber,
      hostname,
      sessionKey,
      timestamp
    );

    // Parse the candid SignedDelegation into a format that `DelegationChain` understands.
    const parsed_signed_delegation = {
        delegation: {
            pubkey: Uint8Array.from(signed_delegation.delegation.pubkey),
            expiration: BigInt(signed_delegation.delegation.expiration),
            targets: undefined,
        },
        signature: Uint8Array.from(signed_delegation.signature),
    };

    return {
        kind: "authorize-client-success",
        delegations: [
            parsed_signed_delegation
        ],
        userPublicKey: Uint8Array.from(userKey),
    };
  });
}

const retryGetDelegation = async (
  connection: IDPActor,
  userNumber: bigint,
  hostname: string,
  sessionKey: PublicKey,
  timestamp: bigint,
  maxRetries: number = 5,
): Promise<SignedDelegation> => {
  let lastErr = new Error(`Failed to retrieve a delegation after ${maxRetries} retries.`);
  for (let i = 0; i < maxRetries; i++) {
    try {
      // Linear backoff
      await new Promise(resolve => { setInterval(resolve, 1000 * i) });
      const res = await connection.getDelegation(
        userNumber,
        hostname,
        sessionKey,
        timestamp
      );
      if (isDelegationResponse(res)) {
        return res.signed_delegation
      }
    } catch (err) {
      lastErr = err;
    }
  };
  throw lastErr;
};

function isDelegationResponse(
  x: GetDelegationResponse
): x is { signed_delegation: SignedDelegation } {
  return x && x.hasOwnProperty("signed_delegation");
}
