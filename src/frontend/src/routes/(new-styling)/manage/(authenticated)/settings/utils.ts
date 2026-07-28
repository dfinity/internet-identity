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
