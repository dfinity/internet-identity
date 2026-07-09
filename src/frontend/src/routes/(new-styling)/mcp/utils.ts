import { toPermissionsArg, type AccessLevel } from "$lib/utils/accessLevel";
import type { Authenticated } from "$lib/stores/authentication.store";
import { DelegationChain } from "@icp-sdk/core/identity";
import {
  throwTextCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";

interface McpAuthorizeInput {
  authenticated: Authenticated;
  /** Lifetime in seconds for the session grant (already clamped to
   *  [10 min, 30 days]; the backend clamps again). */
  ttlSeconds: number;
  /** Whether the whole session is read-only: when read-only, every per-app
   *  delegation the server later mints is restricted to query calls. Chosen
   *  once at connect and recorded on the registration delegation's index entry,
   *  so the server cannot upgrade a read-only session to full access. */
  accessLevel: AccessLevel;
  /** The trusted server's declared connect callback: exact-matched by the
   *  caller against the allow-list the server hosts at a fixed well-known
   *  path on its (trusted) origin (see `matchDeclaredCallback`) — the
   *  (attacker-craftable) connect link only ever selects among the entries
   *  the server declares. Always https (the trusted origin is). This is where
   *  II delivers the registration delegation (a top-level navigation carrying
   *  it in the fragment). */
  callback: string;
  /** Opaque value the server issued for this connect, delivered back alongside
   *  the registration delegation so the server can tie it to the connect it
   *  started. */
  state: string;
  /** The MCP server's registration public key `X` (DER) for this connect. II
   *  mints a single-use `P_reg -> X` delegation for it; nothing secret. */
  registrationKey: Uint8Array;
}

/**
 * Connects the MCP server by minting a single-use *registration delegation* and
 * handing it to the server, instead of fetching the server's key and calling
 * `mcp_register` on its behalf. The flow:
 *
 *  1. `prepare_mcp_registration_delegation` — authenticated as the user, mint a
 *     short-lived canister-signed delegation `P_reg -> X` (where `X` is the
 *     server's per-connect registration key from the link) and record the
 *     consent (this anchor, the read-only choice, the grant TTL) on an index
 *     entry keyed by `P_reg`. The anchor and access level live on that entry,
 *     never in an argument the server controls.
 *  2. `get_mcp_registration_delegation` — fetch the certified `P_reg -> X`
 *     delegation and assemble the chain.
 *  3. Deliver the chain to the trusted server over a URL fragment (a top-level
 *     navigation to the callback it declared — see `matchDeclaredCallback`).
 *     The server, holding `X`'s private key, redeems the chain by calling
 *     `mcp_register_v2` with its long-lived session key `S`; the backend binds
 *     `S` to the recorded anchor with the recorded access level. II never binds
 *     a key it merely received, and the registration delegation is single-use.
 *
 * Nothing is delivered anywhere but a callback the trusted origin declares
 * (the origin is verified against the synced config, and the callback against
 * the server's allow-list, by the caller), and the fragment carries only the
 * server's own public key material — no secret. Resolves with the delivery URL
 * the caller should navigate the tab to; the server's callback finishes the
 * flow on its side (e.g. redeeming the chain and handing an OAuth code back to
 * an MCP client).
 */
export const mcpAuthorize = async ({
  authenticated,
  ttlSeconds,
  accessLevel,
  callback,
  state,
  registrationKey,
}: McpAuthorizeInput): Promise<string> => {
  const { identityNumber, actor } = authenticated;

  // The session-grant lifetime the user chose (the backend clamps again). The
  // access level is recorded on the index entry, not folded into the signature.
  // A backend refusal (MCP disabled, unauthenticated, a duplicate in-flight
  // key, ...) throws here and fails the connect before anything is delivered.
  const grantTtlNanos = BigInt(ttlSeconds) * BigInt(1e9);
  const { user_key, expiration } = await actor
    .prepare_mcp_registration_delegation(
      identityNumber,
      registrationKey,
      toPermissionsArg(accessLevel),
      [grantTtlNanos],
    )
    .then(throwTextCanisterError);

  const signed = await actor
    .get_mcp_registration_delegation(
      identityNumber,
      registrationKey,
      expiration,
    )
    .then(throwTextCanisterError);

  // Assemble the `P_reg -> X` chain the server will redeem. `user_key` is
  // `P_reg`'s DER public key, so the server's `mcp_register_v2` call is seen by
  // the backend as `caller() == P_reg`.
  const chain = DelegationChain.fromDelegations(
    [transformSignedDelegation(signed)],
    new Uint8Array(user_key),
  );

  // Deliver over the fragment (never sent to the server in the HTTP request):
  // the trusted server's endpoint reads it client-side, reconstructs the chain,
  // and redeems it. `state` rides along so the server can correlate.
  const fragment = new URLSearchParams();
  fragment.set("delegation", JSON.stringify(chain.toJSON()));
  fragment.set("state", state);
  return `${callback}#${fragment.toString()}`;
};
