import {
  toPermissionsArg,
  toPermissionsString,
  type AccessLevel,
} from "$lib/utils/accessLevel";
import type { Authenticated } from "$lib/stores/authentication.store";
import type { PublicKey } from "@icp-sdk/core/agent";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
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
   *  once at connect and folded into the registration principal's derivation,
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
  /** The MCP server's registration public key `X` (DER) for this connect, from
   *  the link. The *browser-signed* final hop of the registration chain targets
   *  it (see `mcpAuthorize` — the canister never delegates to it directly);
   *  nothing secret. */
  registrationKey: Uint8Array;
}

/**
 * Connects the MCP server by minting a short-lived *registration delegation
 * chain* and handing it to the server, instead of fetching the server's key
 * and calling `mcp_register` on its behalf. The flow:
 *
 *  1. Generate an ephemeral registration key `Y` for this connect; its private
 *     half never leaves this page.
 *  2. `prepare_mcp_registration_delegation` — authenticated as the user, mint a
 *     short-lived canister-signed delegation `P_reg -> Y`. `P_reg` is *derived*
 *     from the consent tuple (this anchor, the read-only choice, the grant TTL,
 *     the trusted server URL); the canister stores only the anchor (keyed by
 *     `P_reg`) so it can recover it at redemption, while the read-only choice
 *     and TTL are re-derived and compared rather than trusted from a
 *     `register_v2` argument the server invents.
 *  3. `get_mcp_registration_delegation` — fetch the certified `P_reg -> Y`
 *     delegation (passing the same consent parameters, which determine the
 *     derivation), then extend the chain *locally* with a second hop `Y -> X`
 *     signed by `priv(Y)`, where `X` is the server's per-connect registration
 *     key from the link.
 *  4. Deliver the full chain — with the echoed consent (`permissions`, `ttl`)
 *     alongside — to the trusted server over a URL fragment (a top-level
 *     navigation to the callback it declared — see `matchDeclaredCallback`).
 *     The server, holding `X`'s private key, redeems it by calling
 *     `mcp_register_v2` with its long-lived session key `S`, echoing those two
 *     values; the backend recovers the anchor from the registration entry
 *     (so the anchor is never delivered to or seen by the server), re-derives
 *     `P_reg` from (recovered anchor, echoed values, current config) and binds
 *     `S` only if it lands exactly on `caller()`. II never binds a key it
 *     merely received, and an altered echo (upgraded access, stretched TTL)
 *     derives a different principal and is rejected.
 *
 * The two-hop shape is load-bearing: what the canister signs transits the IC
 * (the `get` query response passes the answering replica and API boundary
 * nodes), so it must be inert on its own — it delegates to the browser-held
 * `Y`, never to the link-supplied `X` an attacker could have planted. The only
 * redeemable artifact, the full `P_reg -> Y -> X` chain, is assembled inside
 * this page and leaves it exclusively via the fragment navigation to the
 * declared callback. Nothing is delivered anywhere but a callback the trusted
 * origin declares (the origin is verified against the synced config, and the
 * callback against the server's allow-list, by the caller). Resolves with the
 * delivery URL the caller should navigate the tab to; the server's callback
 * finishes the flow on its side (e.g. redeeming the chain and handing an OAuth
 * code back to an MCP client).
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

  // The ephemeral registration key `Y` for this connect. The canister-signed
  // hop targets this browser-held key — never the link-supplied `X` — so the
  // delegation that transits the IC is inert to any transport-level observer:
  // only this page holds `priv(Y)`. Fresh per attempt so each connect signs its
  // own second hop (`P_reg` itself is derived from the consent, not from `Y`).
  const registrationIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const browserKey = new Uint8Array(
    registrationIdentity.getPublicKey().toDer(),
  );

  // The session-grant lifetime the user chose (the backend clamps again). The
  // access level and TTL are folded into `P_reg`'s derivation, so `get` takes
  // them too (the seed is re-derived from arguments; the canister stores only
  // the anchor). A backend refusal (MCP disabled, unauthenticated, ...) throws
  // here and fails the connect before anything is delivered.
  const grantTtlNanos = BigInt(ttlSeconds) * BigInt(1e9);
  const { user_key, expiration } = await actor
    .prepare_mcp_registration_delegation(
      identityNumber,
      browserKey,
      toPermissionsArg(accessLevel),
      [grantTtlNanos],
    )
    .then(throwTextCanisterError);

  const signed = await actor
    .get_mcp_registration_delegation(
      identityNumber,
      browserKey,
      toPermissionsArg(accessLevel),
      [grantTtlNanos],
      expiration,
    )
    .then(throwTextCanisterError);

  // Hop 1, canister-signed: `P_reg -> Y`. `user_key` is `P_reg`'s DER public
  // key, so the server's eventual `mcp_register_v2` call is seen by the
  // backend as `caller() == P_reg`.
  const canisterHop = DelegationChain.fromDelegations(
    [transformSignedDelegation(signed)],
    new Uint8Array(user_key),
  );

  // Hop 2, signed here with `priv(Y)`: extend the chain to the server's `X`
  // from the link, with the same expiration as the canister hop. Only now does
  // a redeemable chain exist — inside this page — and it leaves only via the
  // fragment navigation below.
  const chain = await DelegationChain.create(
    registrationIdentity,
    { toDer: () => registrationKey } as unknown as PublicKey,
    new Date(Number(expiration / BigInt(1_000_000))),
    { previous: canisterHop },
  );

  // Deliver over the fragment (never sent to the server in the HTTP request):
  // the trusted server's endpoint reads it client-side, reconstructs the chain,
  // and redeems it. The echoed consent rides along — `permissions`
  // ("queries"/"all") and `ttl` (grant lifetime in ns) are what the server must
  // pass back to `mcp_register_v2` (the derivation authenticates the echo; a
  // tampered value simply fails to redeem). The anchor is deliberately *not*
  // delivered: the canister recovers it server-side from the registration
  // entry, so the server never learns the user's II anchor number. `state`
  // lets the server correlate.
  const fragment = new URLSearchParams();
  fragment.set("delegation", JSON.stringify(chain.toJSON()));
  fragment.set("state", state);
  fragment.set("permissions", toPermissionsString(accessLevel));
  fragment.set("ttl", grantTtlNanos.toString());
  return `${callback}#${fragment.toString()}`;
};
