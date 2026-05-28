import type { Authenticated } from "$lib/stores/authentication.store";
import { DelegationChain } from "@icp-sdk/core/identity";
import {
  fromHex,
  retryFor,
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";

/**
 * Derivation origin to use for generic (non-dapp) CLI sign-in. Kept stable
 * across the migration from the standalone `cli.id.ai` site to the built-in
 * `id.ai/cli` route so existing CLI principals stay valid.
 */
export const CLI_GENERIC_DERIVATION_ORIGIN = "https://cli.id.ai";

interface CliAuthorizeInput {
  authenticated: Authenticated;
  /** Hex-encoded DER session pubkey supplied by the CLI. */
  publicKeyHex: string;
  /** Hostname of the dapp the CLI is being authorized for, or undefined for
   *  generic mode. */
  appHost?: string;
  /** Lifetime in minutes. */
  ttlMinutes: number;
  /** Loopback URL to POST the delegation chain to. */
  callback: string;
}

/**
 * Derivation origin used for `prepare_account_delegation` / `get_account_delegation`.
 * - Generic CLI sign-in uses `cli.id.ai` so existing principals (issued by the
 *   standalone cli.id.ai site) stay valid after the migration.
 * - Dapp mode uses the dapp's own origin.
 */
const derivationOrigin = (appHost: string | undefined): string =>
  appHost === undefined ? CLI_GENERIC_DERIVATION_ORIGIN : `https://${appHost}`;

/**
 * Builds a delegation against the supplied session key and POSTs the full
 * chain to the CLI's loopback callback. Mirrors the cli.id.ai protocol so
 * existing CLI binaries can switch endpoints without changes.
 */
export const cliAuthorize = async ({
  authenticated,
  publicKeyHex,
  appHost,
  ttlMinutes,
  callback,
}: CliAuthorizeInput): Promise<void> => {
  const { identityNumber, actor } = authenticated;
  const sessionPublicKey = fromHex(publicKeyHex);
  const effectiveOrigin = derivationOrigin(appHost);
  // Convert minutes to nanoseconds.
  const maxTimeToLiveNanos = BigInt(ttlMinutes) * BigInt(60) * BigInt(1e9);

  const { user_key, expiration } = await actor
    .prepare_account_delegation(
      identityNumber,
      effectiveOrigin,
      [],
      sessionPublicKey,
      [maxTimeToLiveNanos],
    )
    .then(throwCanisterError);

  const delegationChain = await retryFor(5, () =>
    actor
      .get_account_delegation(
        identityNumber,
        effectiveOrigin,
        [],
        sessionPublicKey,
        expiration,
      )
      .then(throwCanisterError)
      .then(transformSignedDelegation)
      .then((delegation) =>
        DelegationChain.fromDelegations([delegation], new Uint8Array(user_key)),
      ),
  );

  const response = await fetch(callback, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(delegationChain.toJSON()),
    redirect: "error",
  });
  if (!response.ok) {
    throw new Error(
      `Callback returned ${response.status} ${response.statusText}`,
    );
  }
};
