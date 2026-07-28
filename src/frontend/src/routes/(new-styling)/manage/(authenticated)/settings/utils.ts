import type { McpConfig } from "$lib/utils/mcpConfig";

/**
 * The URL `config` trusts for display: the identity's own server when they set
 * one, otherwise `officialUrl`. Nothing while the feature is off, or when the
 * identity has never configured it. Mirrors `session_trusted_url` in the
 * canister.
 */
export const trustedUrl = (
  config: McpConfig | undefined,
  officialUrl: string | undefined,
): string | undefined =>
  config !== undefined && config.enabled
    ? (config.url ?? officialUrl)
    : undefined;
