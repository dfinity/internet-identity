import type { Authenticated } from "$lib/stores/authentication.store";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { remapToLegacyDomain } from "$lib/utils/iiConnection";
import {
  fromBase64URL,
  retryFor,
  throwCanisterError,
  transformSignedDelegation,
} from "$lib/utils/utils";

interface McpAuthorizeInput {
  authenticated: Authenticated;
  /** base64url-encoded DER session pubkey supplied by the MCP server. */
  publicKey: string;
  /** The MCP server origin, taken from the connect request's callback (e.g.
   *  "https://mcp.id.ai"). MCP connections are to remote servers, so this is
   *  always an https origin. Connecting binds the agent to this origin and the
   *  standing delegation acts as the user's identity there. */
  mcpServerOrigin: string;
  /** Lifetime in minutes. */
  ttlMinutes: number;
  /** Callback URL (on the MCP server origin) the delegation chain is
   *  form-POSTed to. */
  callback: string;
  /** Opaque value from the request, echoed back so the MCP server can tie the
   *  delivered delegation to the request it started. */
  state: string;
}

/**
 * Builds a two-hop delegation chain rooted at the user's identity and ending at
 * the MCP server's public key, then delivers it to the MCP server's callback
 * via a top-level form-POST navigation. The MCP server reads the post and
 * redirects the browser back to `/mcp` with a `status` so this page keeps
 * owning the UI.
 *
 * Connecting authorizes the agent for the user's identity, not for a specific
 * account: it binds the principal II derives for the anchor at the MCP server
 * origin (`mcp_set_access`), which is the same principal the standing delegation
 * below carries. No account is chosen here — the MCP server origin is the
 * connector, not an app, and accounts are per-origin; the server selects an app
 * account per call later via `mcp_prepare_account_delegation`.
 *
 * The canister only ever signs a delegation to a freshly-generated,
 * non-extractable browser key — never to the public_key from the URL fragment
 * (which is attacker-controllable). The MCP server's public key only enters the
 * chain via a sub-delegation signed by the ephemeral key.
 */
export const mcpAuthorize = async ({
  authenticated,
  publicKey,
  mcpServerOrigin,
  ttlMinutes,
  callback,
  state,
}: McpAuthorizeInput): Promise<void> => {
  const { identityNumber, actor } = authenticated;

  // Bind and sign at the MCP server origin. Remap a gateway origin (*.icp0.io /
  // *.icp.net) to *.ic0.app so the principal matches the one /authorize derives
  // for that origin.
  const effectiveOrigin = remapToLegacyDomain(mcpServerOrigin);
  const maxTimeToLiveNanos = BigInt(ttlMinutes) * BigInt(60) * BigInt(1e9);

  // Connecting is the opt-in: authorize this agent for the anchor at the MCP
  // server origin so II honors the server's later on-demand per-app delegation
  // calls. The backend binds the principal it derives for the anchor at this
  // origin — exactly the principal the standing delegation below carries — and
  // re-derives the same principal to revoke. Idempotent.
  const accessResult = await actor.mcp_set_access(
    identityNumber,
    effectiveOrigin,
    true,
  );
  if ("Err" in accessResult) {
    throw new Error(accessResult.Err);
  }

  const ephemeralIdentity = await ECDSAKeyIdentity.generate({
    extractable: false,
  });
  const ephemeralPublicKey = new Uint8Array(
    ephemeralIdentity.getPublicKey().toDer(),
  );

  // The standing delegation is for the identity's default account at the MCP
  // server origin (`[]`) — the same principal `mcp_set_access` bound above.
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

  // Sub-delegate from the ephemeral key to the MCP server's public key. The
  // expiration matches the canister-signed delegation so the chain expires as a
  // whole.
  const mcpPubKey = fromBase64URL(publicKey);
  const expirationDate = new Date(Number(expiration / BigInt(1_000_000)));
  const chain = await DelegationChain.create(
    ephemeralIdentity,
    { toDer: () => mcpPubKey },
    expirationDate,
    { previous: canisterChain },
  );

  // Submit as a top-level navigation to the MCP server's callback. The /mcp
  // landing page's `form-action` CSP is `'self' https:`, matching the https
  // callbacks the connect flow accepts. The MCP server redirects back to /mcp
  // with a status param.
  const form = document.createElement("form");
  form.method = "POST";
  form.action = callback;
  const delegationInput = document.createElement("input");
  delegationInput.type = "hidden";
  delegationInput.name = "delegation";
  delegationInput.value = JSON.stringify(chain.toJSON());
  form.appendChild(delegationInput);
  // Echo the state so the MCP server can match this delivery to the request it
  // started.
  const stateInput = document.createElement("input");
  stateInput.type = "hidden";
  stateInput.name = "state";
  stateInput.value = state;
  form.appendChild(stateInput);
  document.body.appendChild(form);
  form.submit();
};
