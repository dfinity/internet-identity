import { toPermissionsArg, type AccessLevel } from "$lib/utils/accessLevel";
import { originOf, type McpConfig } from "$lib/utils/mcpConfig";
import { matchDeclaredCallback } from "$lib/utils/authCallbacks";
import type { BackendCanisterConfig } from "$lib/globals";
import type { Authenticated } from "$lib/stores/authentication.store";
import type { PublicKey } from "@icp-sdk/core/agent";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import {
  throwTextCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";

/** The exact refusal `prepare_mcp_registration_delegation` returns when the
 *  identity has no trusted MCP server (the feature is off or was never
 *  configured). Kept in sync with the canister (`mcp_registration.rs`); matched
 *  exactly, not as a substring, so only this refusal — never an unrelated error
 *  that merely contains the phrase — routes the connect to the untrusted
 *  screen. A wording drift only downgrades this UX to a generic error, never a
 *  wrong delivery. */
const NO_TRUSTED_SERVER_ERROR =
  "MCP registration failed: no trusted MCP server is enabled for this identity.";

interface McpAuthorizeInput {
  authenticated: Authenticated;
  /** Lifetime in seconds for the session grant (already clamped to
   *  [10 min, 30 days]; the backend clamps again). */
  ttlSeconds: number;
  /** Whether the whole session is read-only: when read-only, every per-app
   *  delegation the server later mints is restricted to query calls. Chosen
   *  once at connect and recorded on the registration entry by `prepare`, so
   *  the server cannot upgrade a read-only session to full access. */
  accessLevel: AccessLevel;
  /** The connect link's server origin (scheme+host[:port] of its callback).
   *  Delivery happens only after `prepare`'s *certified* `trusted_url` confirms
   *  this is the origin the identity trusts — so a forged `mcp_get_config`
   *  query can't redirect delivery to an attacker's origin. */
  serverOrigin: string;
  /** The exact callback the (attacker-craftable) connect link requested. Once
   *  `serverOrigin` is trust-confirmed, it is matched against the allow-list
   *  the server declares at a fixed well-known path on its origin (see
   *  `matchDeclaredCallback`) — the link only ever selects among the declared
   *  entries. This is where II delivers the registration delegation (a
   *  top-level navigation carrying it in the fragment). */
  requestedCallback: string;
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
 * chain* and handing it to the server to redeem, rather than binding a key
 * that II merely fetched from the server. The flow:
 *
 *  1. Generate an ephemeral registration key `Y` for this connect; its private
 *     half never leaves this page.
 *  2. `prepare_mcp_registration_delegation` — authenticated as the user, mint a
 *     short-lived canister-signed delegation `P_reg -> Y`. `P_reg` is derived
 *     from a fresh random nonce, and the whole consent (this anchor, the
 *     read-only choice, the grant TTL, the trusted server URL) is recorded on
 *     the index entry keyed by `P_reg`, so the server recovers it at redemption
 *     rather than being trusted to supply it.
 *  3. `get_mcp_registration_delegation` — fetch the certified `P_reg -> Y`
 *     delegation (handing back the `user_key` prepare returned, which the
 *     canister uses to recover the seed), then extend the chain *locally* with
 *     a second hop `Y -> X` signed by `priv(Y)`, where `X` is the server's
 *     per-connect registration key from the link.
 *  4. Deliver the full chain to the trusted server over a URL fragment (a
 *     top-level navigation to the callback it declared — see
 *     `matchDeclaredCallback`). The server, holding `X`'s private key, redeems
 *     it by calling `mcp_register_v2` with only its long-lived session key `S`;
 *     the backend recovers the entire consent (anchor, read-only choice, grant
 *     lifetime) from the registration entry keyed by `caller()`. II never binds
 *     a key it merely received, the server passes no consented value (so it
 *     can't alter any), and it never learns the anchor number.
 *
 * The two-hop shape is load-bearing: what the canister signs transits the IC
 * (the `get` query response passes the answering replica and API boundary
 * nodes), so it must be inert on its own — it delegates to the browser-held
 * `Y`, never to the link-supplied `X` an attacker could have planted. The only
 * redeemable artifact, the full `P_reg -> Y -> X` chain, is assembled inside
 * this page and leaves it exclusively via the fragment navigation to the
 * declared callback.
 *
 * Delivery is gated here, on values that can't be forged by a single node: the
 * connect link's origin must equal the origin of `prepare`'s `trusted_url`
 * (certified, since `prepare` is an update call — so a forged `mcp_get_config`
 * query can't redirect delivery to an attacker's origin), and only then is the
 * link's callback matched against the allow-list the server declares on that
 * origin. A trust mismatch throws {@link McpUntrustedServerError} before the
 * chain is fetched or assembled, so the minted registration stays inert and
 * expires undelivered. Resolves with the delivery URL the caller should
 * navigate the tab to; the server's callback finishes the flow on its side
 * (e.g. redeeming the chain and handing an OAuth code back to an MCP client).
 */
export class McpUntrustedServerError extends Error {
  constructor() {
    super("The connect link's server is not trusted by this identity.");
    this.name = "McpUntrustedServerError";
  }
}

export const mcpAuthorize = async ({
  authenticated,
  ttlSeconds,
  accessLevel,
  serverOrigin,
  requestedCallback,
  state,
  registrationKey,
}: McpAuthorizeInput): Promise<string> => {
  const { identityNumber, actor } = authenticated;

  // The ephemeral registration key `Y` for this connect. The canister-signed
  // hop targets this browser-held key — never the link-supplied `X` — so the
  // delegation that transits the IC is inert to any transport-level observer:
  // only this page holds `priv(Y)`. Fresh per attempt so each connect signs its
  // own second hop (`P_reg` is seeded canister-side from a fresh random nonce —
  // not from `Y` and not from the consent).
  const browserIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const browserKey = new Uint8Array(browserIdentity.getPublicKey().toDer());

  // The session-grant lifetime and the read-only choice go to `prepare`, which
  // records the whole consent on the index entry keyed by `P_reg` (the server
  // recovers it at redemption). A backend refusal (MCP disabled,
  // unauthenticated, ...) throws here and fails the connect before anything is
  // delivered.
  const grantTtlNanos = BigInt(ttlSeconds) * BigInt(1e9);
  const prepared = await actor.prepare_mcp_registration_delegation(
    identityNumber,
    browserKey,
    toPermissionsArg(accessLevel),
    [grantTtlNanos],
  );
  if ("Err" in prepared) {
    // "No trusted server" means MCP is off or unset for this identity — the
    // untrusted case, routed to the untrusted screen (where the user can set a
    // trusted server) like every other not-trusted outcome, rather than shown
    // as a hard error. Matched exactly (see `NO_TRUSTED_SERVER_ERROR`) so an
    // unrelated refusal (e.g. authentication) propagates as an error instead.
    if (prepared.Err === NO_TRUSTED_SERVER_ERROR) {
      throw new McpUntrustedServerError();
    }
    throw new Error(prepared.Err);
  }
  const { user_key, expiration, trusted_url } = prepared.Ok;

  // Trust gate on a *certified* value. `prepare` is an update call, so
  // `trusted_url` (the identity's resolved trusted server) is certified through
  // consensus and cannot be forged by a single malicious replica or boundary
  // node — unlike an `mcp_get_config` query. Deliver only when the connect
  // link's origin is that trusted origin; on a mismatch we throw before
  // fetching `get` or assembling the chain, so the registration `prepare` just
  // minted is never turned into a redeemable artifact and simply expires.
  if (originOf(trusted_url) !== serverOrigin) {
    throw new McpUntrustedServerError();
  }

  // Trust confirmed: only now contact the server for its declared-callback
  // allow-list, and require the link's callback to exact-match a declared
  // entry (so a crafted link can't point II at an arbitrary path on the trusted
  // origin). Failure here fails the connect closed, before delivery.
  const callback = await matchDeclaredCallback(serverOrigin, requestedCallback);

  // `get` recovers the seed from `user_key` (the value `prepare` returned), so
  // it needs neither the consent params nor a deterministic re-derivation.
  const signed = await actor
    .get_mcp_registration_delegation(
      identityNumber,
      browserKey,
      user_key,
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
    browserIdentity,
    { toDer: () => registrationKey } as unknown as PublicKey,
    new Date(Number(expiration / BigInt(1_000_000))),
    { previous: canisterHop },
  );

  // Deliver over the fragment (never sent to the server in the HTTP request):
  // the trusted server's endpoint reads it client-side, reconstructs the chain,
  // and redeems it with `mcp_register_v2(session_key)` — nothing else. The
  // whole consent (anchor, read-only choice, grant lifetime) is recovered
  // server-side from the registration entry, so the server passes only its own
  // key and never learns any of it; only `state` rides along, so the server can
  // correlate the delivery with the connect it started.
  const fragment = new URLSearchParams();
  fragment.set("delegation", JSON.stringify(chain.toJSON()));
  fragment.set("state", state);
  return `${callback}#${fragment.toString()}`;
};

/**
 * Whether `origin` may be connected. An identity that never configured MCP
 * (`undefined`) may connect the official connector — completing the consent is
 * what enables the feature for them. One that switched the feature off may not:
 * they are sent back to Settings rather than silently re-enabled by a link.
 *
 * Same rule as Settings' `trustedUrl` (both mirror the canister's
 * `connect_trusted_url`): a never-configured identity may connect the official
 * connector, an explicitly disabled one may not. This variant answers it as a
 * boolean against a specific origin, next to the consent flow that needs it.
 *
 * Takes the canister config rather than a bare URL so the official connector
 * can only come from the deployment, never from an arbitrary string at a call
 * site. Matching is by origin, the same boundary the delegation uses (II
 * derives a per-origin principal; the path can't scope it).
 */
export const isOriginTrusted = (
  config: McpConfig | undefined,
  origin: string,
  { mcp_official_url }: Pick<BackendCanisterConfig, "mcp_official_url">,
): boolean => {
  const official = mcp_official_url[0];
  const url =
    config === undefined
      ? official
      : config.enabled
        ? (config.url ?? official)
        : undefined;
  return url !== undefined && originOf(url) === origin;
};
