import type {
  PublicKey,
  SignedDelegation,
} from "$lib/generated/internet_identity_types";
import { toast } from "$lib/templates/toast";
import { AuthenticatedConnection } from "$lib/utils/iiConnection";
import { transformSignedDelegation, unknownToString } from "$lib/utils/utils";
import type { SignedDelegation as FrontendSignedDelegation } from "@dfinity/identity";

/**
 * Prepares and fetches a delegation valid for the authenticated user and the derivation.
 * @param connection authenticated II connection resulting from successful authentication.
 * @param derivationOrigin the origin for which to create the delegation
 * @param publicKey the key to delegate to
 * @param maxTimeToLive until when the delegation is valid (nanoseconds since now)
 * @return Tuple of PublicKey and matching delegation.
 */
export const fetchDelegation = async ({
  connection,
  derivationOrigin,
  publicKey,
  maxTimeToLive,
}: {
  connection: AuthenticatedConnection;
  derivationOrigin: string;
  publicKey: Uint8Array;
  maxTimeToLive?: bigint;
}): Promise<[PublicKey, FrontendSignedDelegation] | { error: unknown }> => {
  const result = await connection.prepareDelegation(
    derivationOrigin,
    publicKey,
    maxTimeToLive,
  );

  if ("error" in result) {
    return result;
  }

  const [userKey, timestamp] = result;

  const signed_delegation = await retryGetDelegation(
    connection,
    derivationOrigin,
    publicKey,
    timestamp,
  );

  // Parse the candid SignedDelegation into a format that `DelegationChain` understands.
  return [userKey, transformSignedDelegation(signed_delegation)];
};

const retryGetDelegation = async (
  connection: AuthenticatedConnection,
  hostname: string,
  publicKey: PublicKey,
  timestamp: bigint,
  maxRetries = 5,
): Promise<SignedDelegation> => {
  for (let i = 0; i < maxRetries; i++) {
    // Linear backoff
    await new Promise((resolve) => {
      setInterval(resolve, 1000 * i);
    });
    const res = await connection.getDelegation(hostname, publicKey, timestamp);
    if ("no_such_delegation" in res) {
      continue;
    }

    if ("error" in res) {
      toast.error(
        "Error while fetching delegation: " +
          unknownToString(res.error, "unknown error"),
      );
      continue;
    }

    return res.signed_delegation;
  }
  throw new Error(
    `Failed to retrieve a delegation after ${maxRetries} retries.`,
  );
};
