import {
  PublicKey,
  SignedDelegation,
} from "$generated/internet_identity_types";
import { toast } from "$src/components/toast";
import { AuthenticatedConnection } from "$src/utils/iiConnection";
import { unknownToString } from "$src/utils/utils";
import { Signature } from "@dfinity/agent";
import { nonNullish } from "@dfinity/utils";
import { Delegation } from "./postMessageInterface";

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
}): Promise<[PublicKey, Delegation] | { error: unknown }> => {
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
    publicKey,
    maxTimeToLive
  );

  if ("error" in result) {
    return result;
  }

  const [userKey, timestamp] = result;

  const signed_delegation = await retryGetDelegation(
    connection,
    derivationOrigin,
    publicKey,
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
      signature: Uint8Array.from(
        signed_delegation.signature
      ) as unknown as Signature,
    },
  ];
};

const retryGetDelegation = async (
  connection: AuthenticatedConnection,
  hostname: string,
  publicKey: PublicKey,
  timestamp: bigint,
  maxRetries = 5
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
