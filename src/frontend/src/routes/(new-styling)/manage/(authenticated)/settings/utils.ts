import type { BackendCanisterConfig } from "$lib/globals";
import type { McpConfig } from "$lib/utils/mcpConfig";

/**
 * The URL a connect from this identity would currently target — what Settings
 * shows as the active connector: the identity's own server when they set one,
 * otherwise the deployment's official connector. Mirrors the canister's
 * `connect_trusted_url` (not `session_trusted_url`), so the two cases where they
 * differ are handled the same as the connect flow:
 *
 *  - **never configured** (`config === undefined`) → the official connector.
 *    A brand-new identity can connect the official connector by default, so
 *    Settings shows AI access as on; turning it off writes the disabled config
 *    that actually blocks it (the opt-out).
 *  - **explicitly disabled** (`enabled === false`) → nothing. Switching the
 *    feature off is the opt-out and is not silently undone.
 *
 * Takes the canister config rather than a bare URL so the official connector
 * can only come from the deployment, never from an arbitrary string at a call
 * site.
 */
export const trustedUrl = (
  config: McpConfig | undefined,
  { mcp_official_url }: Pick<BackendCanisterConfig, "mcp_official_url">,
): string | undefined =>
  config === undefined
    ? mcp_official_url[0]
    : config.enabled
      ? (config.url ?? mcp_official_url[0])
      : undefined;
