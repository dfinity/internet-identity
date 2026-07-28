/**
 * The user's trusted-MCP-server configuration, persisted on-chain (keyed by
 * anchor) so it syncs across all of the identity's devices — unlike the
 * CLI-access toggle, which is device-local. It is read via `mcp_get_config` and
 * written via `mcp_set_config`, both authenticated as the identity, so only the
 * user (never a page that initiates a connect request) can change it. The `/mcp`
 * connect flow reads it as the source of truth at connect time.
 *
 * The config has two parts:
 *  - `enabled`: the feature's master toggle for this identity.
 *  - `url`: the trusted server URL, kept verbatim (so the Settings UI can probe
 *    a path-based endpoint like `https://host/mcp`); trust matching is by origin.
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
  /**
   * Whether this identity has ever written a config. `enabled: false` alone is
   * ambiguous — it reads the same for someone who never touched the feature and
   * for someone who switched it off in Settings — and those must differ at
   * `/mcp`: the first may connect the official connector, the second is sent
   * back to Settings. `undefined` means an older backend that doesn't report
   * it, in which case only a stored URL is ever trusted.
   */
  configured: boolean | undefined;
}

// Candid `opt text` <-> `string | undefined`.
const fromOpt = (opt: [] | [string]): string | undefined =>
  opt.length === 0 ? undefined : opt[0];
const toOpt = (value: string | undefined): [] | [string] =>
  value === undefined ? [] : [value];

const fromCanister = (config: CanisterMcpConfig): McpConfig => ({
  enabled: config.enabled,
  url: fromOpt(config.url),
  configured: config.configured.length === 0 ? undefined : config.configured[0],
});

// `configured` is read-only — the canister ignores it here — but it round-trips
// so a read-modify-write doesn't have to strip it.
const toCanister = (config: McpConfig): CanisterMcpConfig => ({
  enabled: config.enabled,
  url: toOpt(config.url),
  configured: config.configured === undefined ? [] : [config.configured],
});

/** Origin (scheme + host[:port], no path) of a URL, or undefined if unparsable. */
export const originOf = (url: string): string | undefined => {
  try {
    return new URL(url).origin;
  } catch {
    return undefined;
  }
};

/**
 * The URL `config` trusts: the identity's own when they set one, otherwise
 * `officialUrl`. Exactly one is trusted at a time, and nothing is while the
 * feature is off. Mirrors `trusted_url_of` in the canister, which is what
 * actually enforces it.
 */
export const trustedUrl = (
  config: McpConfig,
  officialUrl: string | undefined,
): string | undefined =>
  config.enabled ? (config.url ?? officialUrl) : undefined;

/**
 * The URL a *new* connect may target. An identity that never configured MCP may
 * connect the official connector — completing the consent is what enables the
 * feature for them. One that switched the feature off may not: they are sent
 * back to Settings rather than silently re-enabled by a link. Mirrors
 * `connect_trusted_url` in the canister, which is what actually enforces it.
 */
export const connectTrustedUrl = (
  config: McpConfig,
  officialUrl: string | undefined,
): string | undefined => {
  if (config.configured === false) {
    return officialUrl;
  }
  if (!config.enabled) {
    return undefined;
  }
  return config.url ?? officialUrl;
};

/**
 * Whether `origin` may be connected. Matching is by origin — the same security
 * boundary the delegation uses (II derives a per-origin principal; the path
 * can't scope it).
 */
export const isOriginTrusted = (
  config: McpConfig,
  origin: string,
  officialUrl?: string,
): boolean => {
  const url = connectTrustedUrl(config, officialUrl);
  return url !== undefined && originOf(url) === origin;
};

/** Read the identity's synced MCP config from the canister. */
export const readMcpConfig = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<McpConfig> =>
  fromCanister(await actor.mcp_get_config(identityNumber));

// Read-modify-write a single field of the synced config and persist it. Reads
// the current config first so an unrelated field (toggle vs URL) isn't clobbered
// by the wholesale `mcp_set_config`.
const updateConfig = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  patch: Partial<McpConfig>,
): Promise<McpConfig> => {
  const current = await readMcpConfig(actor, identityNumber);
  const next: McpConfig = { ...current, ...patch };
  const result = await actor.mcp_set_config(identityNumber, toCanister(next));
  if ("Err" in result) {
    throw new Error(result.Err);
  }
  return next;
};

/** Turn the master toggle on/off (synced). */
export const setMcpEnabled = (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  enabled: boolean,
): Promise<McpConfig> =>
  updateConfig(
    actor,
    identityNumber,
    enabled ? { enabled: true } : { enabled: false, url: undefined },
  );

/** Set the trusted server URL (synced). */
export const setMcpTrustedServer = (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  url: string,
): Promise<McpConfig> => updateConfig(actor, identityNumber, { url });

export const trustAndEnableMcp = (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  url: string,
): Promise<McpConfig> =>
  updateConfig(actor, identityNumber, { url, enabled: true });

/** Forget the trusted server URL (synced). */
export const clearMcpTrustedServer = (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<McpConfig> =>
  updateConfig(actor, identityNumber, { url: undefined });
