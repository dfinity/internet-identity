import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { BackendCanisterConfig } from "$lib/globals";
import type { McpConfig } from "$lib/utils/mcpConfig";

/**
 * The URL `config` trusts for display: the identity's own server when they set
 * one, otherwise the deployment's official connector. Nothing while the feature
 * is off, or when the identity has never configured it. Mirrors
 * `session_trusted_url` in the canister.
 *
 * Takes the canister config rather than a bare URL so the official connector
 * can only come from the deployment, never from an arbitrary string at a call
 * site.
 */
export const trustedUrl = (
  config: McpConfig | undefined,
  { mcp_official_url }: Pick<BackendCanisterConfig, "mcp_official_url">,
): string | undefined =>
  config !== undefined && config.enabled
    ? (config.url ?? mcp_official_url[0])
    : undefined;

/**
 * Persist the identity's synced MCP config, replacing whatever is stored.
 *
 * `config` is the user's complete intent — both fields, as the Settings screen
 * shows them. Deliberately no read-modify-write: merging in a field the caller
 * left out would mean reading the stored config first, and the read available
 * here is the uncertified `mcp_get_config` query — which would let a single
 * malicious node pick the trusted server in a write the user then signs. The
 * screen takes its starting point from `IdentityInfo.mcp_config` instead,
 * certified by the `identity_info` update call.
 */
export const writeMcpConfig = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  config: McpConfig,
): Promise<void> => {
  const result = await actor.mcp_set_config(identityNumber, {
    enabled: config.enabled,
    url: config.url === undefined ? [] : [config.url],
  });
  if ("Err" in result) {
    throw new Error(result.Err);
  }
};
