import { IIConnection } from "../../utils/iiConnection";
import {
  FrontendHostname,
  PublicKey,
  SignedDelegation,
  UserNumber,
} from "../../../generated/internet_identity_types";
import { withLoader } from "../../components/loader";
import { confirmRedirect } from "../confirmRedirect";
import { hasOwnProperty } from "../../utils/utils";
import { AuthRequest, AuthResponse } from "./postMessageInterface";

export async function handleAuthRequest(
  connection: IIConnection,
  userNumber: UserNumber,
  request: AuthRequest,
  hostname: FrontendHostname
): Promise<AuthResponse> {
  const userPrincipal = await withLoader(() =>
    connection.getPrincipal(userNumber, hostname)
  );

  if (!(await confirmRedirect(hostname, userPrincipal.toString()))) {
    return {
      kind: "authorize-client-failure",
      text: `User did not grant access to ${hostname}.`,
    };
  }

  return await withLoader(async () => {
    const sessionKey = Array.from(request.sessionPublicKey);
    const prepRes = await connection.prepareDelegation(
      userNumber,
      hostname,
      sessionKey,
      request.maxTimeToLive
    );
    if (prepRes.length !== 2) {
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
      delegations: [parsed_signed_delegation],
      userPublicKey: Uint8Array.from(userKey),
    };
  });
}

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
