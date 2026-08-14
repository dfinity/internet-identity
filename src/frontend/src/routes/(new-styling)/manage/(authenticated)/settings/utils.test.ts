import { describe, it, expect, vi } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { BackendCanisterConfig } from "$lib/globals";
import type { McpConfig } from "$lib/utils/mcpConfig";
import { trustedUrl, writeMcpConfig } from "./utils";

const OFFICIAL: Pick<BackendCanisterConfig, "mcp_official_url"> = {
  mcp_official_url: ["https://official-mcp.id.ai/mcp"],
};
const NO_OFFICIAL: Pick<BackendCanisterConfig, "mcp_official_url"> = {
  mcp_official_url: [],
};
const CUSTOM = "https://mcp.acme.com/mcp";

/** A stored config. `undefined` stands for an identity that never wrote one. */
const cfg = (enabled: boolean, url: string | undefined): McpConfig => ({
  enabled,
  url,
});

describe("trustedUrl", () => {
  it("falls back to the official connector when no custom URL is set", () => {
    expect(trustedUrl(cfg(true, undefined), OFFICIAL)).toBe(
      OFFICIAL.mcp_official_url[0],
    );
  });

  it("prefers the custom connector over the official one", () => {
    expect(trustedUrl(cfg(true, CUSTOM), OFFICIAL)).toBe(CUSTOM);
  });

  it("trusts nothing while the feature is disabled", () => {
    expect(trustedUrl(cfg(false, undefined), OFFICIAL)).toBeUndefined();
    expect(trustedUrl(cfg(false, CUSTOM), OFFICIAL)).toBeUndefined();
  });

  it("trusts nothing when enabled with no custom and no official connector", () => {
    expect(trustedUrl(cfg(true, undefined), NO_OFFICIAL)).toBeUndefined();
  });
});

const ANCHOR = BigInt(10_000);

const makeActor = () => ({
  // Present so a read-modify-write would have something to read: the
  // "never reads" test below asserts nothing calls it.
  mcp_get_config: vi
    .fn()
    .mockResolvedValue([{ enabled: false, url: ["https://mcp.attacker.tld"] }]),
  mcp_set_config: vi.fn().mockResolvedValue({ Ok: null }),
});

const asActor = (actor: ReturnType<typeof makeActor>) =>
  actor as unknown as ActorSubclass<_SERVICE>;

describe("writeMcpConfig", () => {
  it("writes the config it is given, verbatim", async () => {
    const actor = makeActor();

    await writeMcpConfig(asActor(actor), ANCHOR, cfg(true, CUSTOM));

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: true,
      url: [CUSTOM],
    });
  });

  it("writes an absent URL as an empty candid opt", async () => {
    const actor = makeActor();

    await writeMcpConfig(asActor(actor), ANCHOR, cfg(false, undefined));

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: false,
      url: [],
    });
  });

  // The uncertified `mcp_get_config` query is forgeable by a single malicious
  // node. Reading it here would copy the forged URL into a write the user
  // signs, so the write path must never consult it.
  it("never reads the config it is about to overwrite", async () => {
    const actor = makeActor();

    await writeMcpConfig(asActor(actor), ANCHOR, cfg(true, undefined));

    expect(actor.mcp_get_config).not.toHaveBeenCalled();
    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: true,
      url: [],
    });
  });

  it("rejects with the canister's error so callers can roll back", async () => {
    const actor = makeActor();
    actor.mcp_set_config.mockResolvedValue({ Err: "config is too large" });

    await expect(
      writeMcpConfig(asActor(actor), ANCHOR, cfg(true, CUSTOM)),
    ).rejects.toThrow("config is too large");
  });
});
