import { IIConnection } from "../../utils/iiConnection";
import { AuthContext, Delegation } from "./postMessageInterface";
import {
  PublicKey,
  SignedDelegation,
} from "../../../generated/internet_identity_types";
import { hasOwnProperty } from "../../utils/utils";

/**
 * Prepares and fetches a delegation valid for the authenticated user and the application information contained in
 * authContext.
 * @param loginResult User number and authenticated II connection resulting from successful authentication.
 * @param authContext Information about the authentication request received from the application via window post message.
 * @return Tuple of PublicKey and matching delegation.
 */
export const fetchDelegation = async (
  loginResult: {
    userNumber: bigint;
    connection: IIConnection;
  },
  authContext: AuthContext
): Promise<[PublicKey, Delegation]> => {
  const sessionKey = Array.from(authContext.authRequest.sessionPublicKey);
  const [userKey, timestamp] = await loginResult.connection.prepareDelegation(
    loginResult.userNumber,
    authContext.requestOrigin,
    sessionKey,
    authContext.authRequest.maxTimeToLive
  );

  const signed_delegation = await retryGetDelegation(
    loginResult.connection,
    loginResult.userNumber,
    authContext.requestOrigin,
    sessionKey,
    timestamp
  );

  // Parse the candid SignedDelegation into a format that `DelegationChain` understands.
  return [
    userKey,
    {
      delegation: {
        pubkey: Uint8Array.from(signed_delegation.delegation.pubkey),
        expiration: BigInt(signed_delegation.delegation.expiration),
        targets: undefined,
      },
      signature: Uint8Array.from(signed_delegation.signature),
    },
  ];
};

const retryGetDelegation = async (
  connection: IIConnection,
  userNumber: bigint,
  hostname: string,
  sessionKey: PublicKey,
  timestamp: bigint,
  maxRetries = 5
): Promise<SignedDelegation> => {
  for (let i = 0; i < maxRetries; i++) {
    // Linear backoff
    await new Promise((resolve) => {
      setInterval(resolve, 1000 * i);
    });
    const res = await connection.getDelegation(
      userNumber,
      hostname,
      sessionKey,
      timestamp
    );
    if (hasOwnProperty(res, "signed_delegation")) {
      return res.signed_delegation;
    }
  }
  throw new Error(
    `Failed to retrieve a delegation after ${maxRetries} retries.`
  );
};
