/**
 * The user's trusted-MCP-server configuration, persisted on-chain (keyed by
 * anchor) so it syncs across all of the identity's devices — unlike the
 * CLI-access toggle, which is device-local. It is read via `mcp_get_config` and
 * written via `mcp_set_config`, both authenticated as the identity and both
 * update calls, so only the user (never a page that initiates a connect request)
 * can change it and no single node can misreport it. The `/mcp` connect flow
 * reads it as the source of truth at connect time (see `readMcpConfig` for why
 * the read must be certified).
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
}

// Candid `opt text` <-> `string | undefined`.
const fromOpt = (opt: [] | [string]): string | undefined =>
  opt.length === 0 ? undefined : opt[0];
const toOpt = (value: string | undefined): [] | [string] =>
  value === undefined ? [] : [value];

const fromCanister = (config: CanisterMcpConfig): McpConfig => ({
  enabled: config.enabled,
  url: fromOpt(config.url),
});

const toCanister = (config: McpConfig): CanisterMcpConfig => ({
  enabled: config.enabled,
  url: toOpt(config.url),
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
 * Read the identity's synced MCP config from the canister.
 *
 * `mcp_get_config` is declared as an **update** in the canister interface, so
 * this goes through consensus and the reply is certified — never a query. That
 * is load-bearing rather than incidental: this read is what tells the `/mcp`
 * connect flow which origin the identity trusts, and a query reply is signed by
 * the single node that served it. A malicious replica or boundary node could
 * answer with a trusted-server URL the user never set, and the connect flow
 * would then mint and deliver a registration delegation to it — a server the
 * user never approved acting as them at any app. The backend can't catch that
 * either: `prepare_mcp_registration_delegation` records the anchor's *real*
 * trusted URL, so the check at redemption still passes. Certification is the
 * only thing that makes the answer trustworthy, which is why the endpoint is an
 * update (see the `mcp_get_config` comment in `internet_identity.did`) and why
 * `readMcpConfig` must stay the single way the frontend obtains this config.
 */
export const readMcpConfig = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
): Promise<McpConfig | undefined> => {
  const config = await actor.mcp_get_config(identityNumber);
  return config.length === 0 ? undefined : fromCanister(config[0]);
};

// Read-modify-write a single field of the synced config and persist it. Reads
// the current config first so an unrelated field (toggle vs URL) isn't clobbered
// by the wholesale `mcp_set_config`.
const updateConfig = async (
  actor: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  patch: Partial<McpConfig>,
): Promise<McpConfig> => {
  const current = (await readMcpConfig(actor, identityNumber)) ?? {
    enabled: false,
    url: undefined,
  };
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
