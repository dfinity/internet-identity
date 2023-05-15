import {
  PublicKey,
  SignedDelegation,
} from "$generated/internet_identity_types";
import { toast } from "$src/components/toast";
import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { unknownToString } from "$src/utils/utils";
import { nonNullish } from "@dfinity/utils";
import { AuthContext, Delegation } from "./postMessageInterface";

/**
 * Prepares and fetches a delegation valid for the authenticated user and the application information contained in
 * authContext.
 * @param userNumber User number resulting from successful authentication.
 * @param connection authenticated II connection resulting from successful authentication.
 * @param authContext Information about the authentication request received from the application via window post message.
 * @return Tuple of PublicKey and matching delegation.
 */
export const fetchDelegation = async (
  userNumber: bigint,
  connection: AuthenticatedConnection,
  authContext: AuthContext
): Promise<[PublicKey, Delegation] | { error: unknown }> => {
  const sessionKey = Array.from(authContext.authRequest.sessionPublicKey);

  // at this point, derivationOrigin is either validated or undefined
  let derivationOrigin = nonNullish(authContext.authRequest.derivationOrigin)
    ? authContext.authRequest.derivationOrigin
    : authContext.requestOrigin;

  // In order to give dapps a stable principal regardless whether they use the legacy (ic0.app) or the new domain (icp0.io)
  // we map back the derivation origin to the ic0.app domain.
  const ORIGIN_MAPPING_REGEX =
    /^https:\/\/(?<subdomain>[\w-]+(?:\.raw)?)\.icp0\.io$/;
  const match = derivationOrigin.match(ORIGIN_MAPPING_REGEX);
  const subdomain = match?.groups?.subdomain;
  if (nonNullish(subdomain)) {
    derivationOrigin = `https://${subdomain}.ic0.app`;
  }

  const result = await connection.prepareDelegation(
    derivationOrigin,
    sessionKey,
    authContext.authRequest.maxTimeToLive
  );

  if ("error" in result) {
    return result;
  }

  const [userKey, timestamp] = result;

  const signed_delegation = await retryGetDelegation(
    connection,
    userNumber,
    derivationOrigin,
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
  connection: AuthenticatedConnection,
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
    const res = await connection.getDelegation(hostname, sessionKey, timestamp);
    if ("no_such_delegation" in res) {
      continue;
    }

    if ("error" in res) {
      toast.error(
        "Error while fetching delegation: " +
          unknownToString(res.error, "unknown error")
      );
      continue;
    }

    return res.signed_delegation;
  }
  throw new Error(
    `Failed to retrieve a delegation after ${maxRetries} retries.`
  );
};
