import type { Authenticated } from "$lib/stores/authentication.store";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import {
  fromBase64URL,
  retryFor,
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";

/** Derivation origin used when the CLI signs in to II itself (no `app=`). */
export const CLI_GENERIC_DERIVATION_ORIGIN = "https://cli.id.ai";

interface CliAuthorizeInput {
  authenticated: Authenticated;
  /** base64url-encoded DER session pubkey supplied by the CLI. */
  publicKey: string;
  /** Hostname of the app the CLI is being authorized for, or undefined for
   *  generic mode. */
  appHost?: string;
  /** Lifetime in minutes. */
  ttlMinutes: number;
  /** Loopback URL to POST the delegation chain to. */
  callback: string;
}

const derivationOrigin = (appHost: string | undefined): string =>
  appHost === undefined ? CLI_GENERIC_DERIVATION_ORIGIN : `https://${appHost}`;

/**
 * Builds a two-hop delegation chain rooted at the user's identity and ending
 * at the CLI's public key, then POSTs it to the CLI's loopback callback.
 *
 * The canister only ever signs a delegation to a freshly-generated,
 * non-extractable browser key — never to the public_key from the URL
 * fragment (which is attacker-controllable). The CLI's public key only
 * enters the chain via a sub-delegation signed by the ephemeral key.
 */
export const cliAuthorize = async ({
  authenticated,
  publicKey,
  appHost,
  ttlMinutes,
  callback,
}: CliAuthorizeInput): Promise<void> => {
  const { identityNumber, actor } = authenticated;
  const effectiveOrigin = derivationOrigin(appHost);
  const maxTimeToLiveNanos = BigInt(ttlMinutes) * BigInt(60) * BigInt(1e9);

  const ephemeralIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const ephemeralPublicKey = new Uint8Array(
    ephemeralIdentity.getPublicKey().toDer(),
  );

  const { user_key, expiration } = await actor
    .prepare_account_delegation(
      identityNumber,
      effectiveOrigin,
      [],
      ephemeralPublicKey,
      [maxTimeToLiveNanos],
    )
    .then(throwCanisterError);

  const canisterChain = await retryFor(5, () =>
    actor
      .get_account_delegation(
        identityNumber,
        effectiveOrigin,
        [],
        ephemeralPublicKey,
        expiration,
      )
      .then(throwCanisterError)
      .then(transformSignedDelegation)
      .then((delegation) =>
        DelegationChain.fromDelegations([delegation], new Uint8Array(user_key)),
      ),
  );

  // Sub-delegate from the ephemeral key to the CLI's public key. The
  // expiration matches the canister-signed delegation so the chain
  // expires as a whole.
  const cliPubKey = fromBase64URL(publicKey);
  const expirationDate = new Date(Number(expiration / BigInt(1_000_000)));
  const chain = await DelegationChain.create(
    ephemeralIdentity,
    { toDer: () => cliPubKey },
    expirationDate,
    { previous: canisterChain },
  );

  const response = await fetch(callback, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(chain.toJSON()),
    redirect: "error",
  });
  if (!response.ok) {
    throw new Error(
      `Callback returned ${response.status} ${response.statusText}`,
    );
  }
};
