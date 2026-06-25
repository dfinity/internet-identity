/**
 * The user's trusted-MCP-server configuration, stored as on-chain identity
 * metadata so it syncs across all of the identity's devices — unlike the
 * CLI-access toggle, which is device-local. It is read via `identity_info` and
 * written via `identity_metadata_replace`, both authenticated as the identity,
 * so only the user (never a page that initiates a connect request) can change
 * it. The `/mcp` connect flow reads it as the source of truth at connect time.
 *
 * Two identity-metadata keys back it:
 *  - `mcp_enabled`: "true" when the feature's master toggle is on.
 *  - `mcp_trusted_server`: the trusted server URL, kept verbatim (so the
 *    Settings UI can probe a path-based endpoint like `https://host/mcp`);
 *    trust matching is by origin.
 */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  MetadataMapV2,
} from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";

const KEY_ENABLED = "mcp_enabled";
const KEY_URL = "mcp_trusted_server";

export interface McpConfig {
  /** Master toggle for the feature on this identity. */
  enabled: boolean;
  /** The trusted server URL, or undefined when none is set. */
  url: string | undefined;
}

const stringEntry = (
  metadata: MetadataMapV2,
  key: string,
): string | undefined => {
  const entry = metadata.find(([entryKey]) => entryKey === key);
  if (entry === undefined) {
    return undefined;
  }
  const value = entry[1];
  return "String" in value ? value.String : undefined;
};

const parseConfig = (metadata: MetadataMapV2): McpConfig => ({
  enabled: stringEntry(metadata, KEY_ENABLED) === "true",
  url: stringEntry(metadata, KEY_URL),
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
 * Whether `config` trusts `origin`: the feature is enabled and a trusted server
 * of that origin is set. Matching is by origin — the same security boundary the
 * delegation uses (II derives a per-origin principal; the path can't scope it).
 */
export const isOriginTrusted = (config: McpConfig, origin: string): boolean =>
  config.enabled && config.url !== undefined && originOf(config.url) === origin;

/** Read the identity's synced MCP config from its on-chain metadata. */
export const readMcpConfig = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<McpConfig> => {
  const info = await actor
    .identity_info(identityNumber)
    .then(throwCanisterError);
  return parseConfig(info.metadata);
};

// Replace a single identity-metadata key (preserving all others) and persist.
// Reads the current metadata first so concurrent/unrelated keys aren't dropped
// by the wholesale `identity_metadata_replace`.
const replaceKey = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  key: string,
  value: string | undefined,
): Promise<McpConfig> => {
  const info = await actor
    .identity_info(identityNumber)
    .then(throwCanisterError);
  const others = info.metadata.filter(([entryKey]) => entryKey !== key);
  const entry: MetadataMapV2[number] = [key, { String: value ?? "" }];
  const next: MetadataMapV2 = value === undefined ? others : [...others, entry];
  await actor
    .identity_metadata_replace(identityNumber, next)
    .then(throwCanisterError);
  return parseConfig(next);
};

/** Turn the master toggle on/off (synced). */
export const setMcpEnabled = (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  enabled: boolean,
): Promise<McpConfig> =>
  replaceKey(actor, identityNumber, KEY_ENABLED, enabled ? "true" : undefined);

/** Set the trusted server URL (synced). */
export const setMcpTrustedServer = (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  url: string,
): Promise<McpConfig> => replaceKey(actor, identityNumber, KEY_URL, url);

/** Forget the trusted server URL (synced). */
export const clearMcpTrustedServer = (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<McpConfig> => replaceKey(actor, identityNumber, KEY_URL, undefined);
