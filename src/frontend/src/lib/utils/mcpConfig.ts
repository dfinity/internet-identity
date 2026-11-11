/**
 * The user's trusted-MCP-server configuration, persisted on-chain (keyed by
 * anchor) so it syncs across all of the identity's devices — unlike the
 * CLI-access toggle, which is device-local. It is written via `mcp_set_config`,
 * authenticated as the identity, so only the user (never a page that initiates
 * a connect request) can change it. The `/mcp` connect flow reads it as the
 * source of truth at connect time.
 *
 * The config has two parts:
 *  - `enabled`: the feature's master toggle for this identity.
 *  - `url`: the trusted server URL, kept verbatim (so the Settings UI can probe
 *    a path-based endpoint like `https://host/mcp`); trust matching is by origin.
 *
 * It reaches the frontend two ways: certified, as `IdentityInfo.mcp_config` on
 * the `identity_info` update call, and uncertified, from the `mcp_get_config`
 * query below.
 */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  McpConfig as CanisterMcpConfig,
} from "$lib/generated/internet_identity_types";

export interface McpConfig {
  /** Master toggle for the feature on this identity. */
  enabled: boolean;
  /** The trusted server URL, or undefined when none is set. */
  url: string | undefined;
}

// Candid `opt text` -> `string | undefined`.
const fromOpt = (opt: [] | [string]): string | undefined =>
  opt.length === 0 ? undefined : opt[0];

/**
 * Decode a candid `opt McpConfig` — `IdentityInfo.mcp_config` or a
 * `mcp_get_config` reply. `undefined` when the identity never wrote a config.
 */
export const fromCanisterMcpConfig = (
  config: [] | [CanisterMcpConfig],
): McpConfig | undefined =>
  config.length === 0
    ? undefined
    : { enabled: config[0].enabled, url: fromOpt(config[0].url) };

/** Origin (scheme + host[:port], no path) of a URL, or undefined if unparsable. */
export const originOf = (url: string): string | undefined => {
  try {
    return new URL(url).origin;
  } catch {
    return undefined;
  }
};

/**
 * Read the identity's synced MCP config with the `mcp_get_config` query.
 *
 * The reply is **uncertified** — a single malicious node can forge it — so it
 * may only steer UX that a certified value decides for real: the `/mcp`
 * pre-check and the untrusted screen's auto-advance, both of which end in
 * `mcpAuthorize` gating delivery on `prepare`'s certified `trusted_url`. Never
 * render trust from it, and never feed it into an `mcp_set_config` write: use
 * `IdentityInfo.mcp_config`, certified by the `identity_info` update call.
 */
export const readMcpConfig = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<McpConfig | undefined> =>
  fromCanisterMcpConfig(await actor.mcp_get_config(identityNumber));
