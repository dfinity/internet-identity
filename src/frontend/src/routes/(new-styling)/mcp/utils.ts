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
  /** The configured MCP server origin (e.g. "https://mcp.id.ai"). The standing
   *  delegation acts as the user's account at this origin — it's II config, not
   *  a request parameter, since the connection is to the MCP server itself. */
  mcpServerOrigin: string;
  /** Lifetime in minutes. */
  ttlMinutes: number;
  /** Callback URL (on the configured MCP server origin) the delegation chain is
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
 * The chain is derived for the user's account at the configured MCP server
 * origin: `get_default_account` resolves that default account, and its account
 * number (the unreserved default or a specific one the user set) is what the
 * delegation is bound to — so the MCP server acts as the same account a normal
 * `/authorize` sign-in to that origin would use, not the legacy anchor-seed
 * principal a blind `null` produces.
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

  // Connecting the MCP server is the opt-in: enable MCP access for this anchor
  // so II authorizes the server's later on-demand per-app delegation calls. The
  // backend recovers the anchor from the caller principal it derives for this
  // anchor at the configured mcp_server_origin (see the `mcp_*` canister
  // methods), which is exactly the principal the standing delegation below
  // carries. Idempotent.
  const accessResult = await actor.mcp_set_access(identityNumber, true);
  if ("Err" in accessResult) {
    throw new Error(accessResult.Err);
  }

  // The delegation acts as the user's account at the configured MCP server
  // origin. Remap a gateway origin (*.icp0.io / *.icp.net) to *.ic0.app so the
  // principal matches the one /authorize derives for that origin.
  const effectiveOrigin = remapToLegacyDomain(mcpServerOrigin);
  const maxTimeToLiveNanos = BigInt(ttlMinutes) * BigInt(60) * BigInt(1e9);

  const { account_number } = await actor
    .get_default_account(identityNumber, effectiveOrigin)
    .then(throwCanisterError);

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
      account_number,
      ephemeralPublicKey,
      [maxTimeToLiveNanos],
    )
    .then(throwCanisterError);

  const canisterChain = await retryFor(5, () =>
    actor
      .get_account_delegation(
        identityNumber,
        effectiveOrigin,
        account_number,
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

  // Submit as a top-level navigation to the configured MCP server origin (the
  // only origin allowed by the `form-action` CSP). The MCP server redirects
  // back to /mcp with a status param.
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
