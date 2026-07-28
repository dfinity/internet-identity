import { describe, it, expect, vi } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  originOf,
  setMcpEnabled,
  trustAndEnableMcp,
  clearMcpTrustedServer,
  type McpConfig,
} from "./mcpConfig";

describe("originOf", () => {
  it("returns the origin (scheme + host + port), dropping path/query/hash", () => {
    expect(originOf("https://mcp.id.ai/mcp?x=1#frag")).toBe(
      "https://mcp.id.ai",
    );
  });

  it("keeps a non-default port in the origin", () => {
    expect(originOf("https://mcp.id.ai:8443/mcp")).toBe(
      "https://mcp.id.ai:8443",
    );
  });

  it("returns undefined for an unparsable URL", () => {
    expect(originOf("not a url")).toBeUndefined();
    expect(originOf("")).toBeUndefined();
    expect(originOf("/mcp")).toBeUndefined();
  });
});

const ANCHOR = BigInt(10_000);

const makeActor = (current: McpConfig) => ({
  mcp_get_config: vi.fn().mockResolvedValue([
    {
      enabled: current.enabled,
      url: current.url === undefined ? [] : [current.url],
    },
  ]),
  mcp_set_config: vi.fn().mockResolvedValue({ Ok: null }),
});

const asActor = (actor: ReturnType<typeof makeActor>) =>
  actor as unknown as ActorSubclass<_SERVICE>;

const CUSTOM = "https://mcp.acme.com/mcp";

/** A stored config. `undefined` stands for an identity that never wrote one. */
const cfg = (enabled: boolean, url: string | undefined): McpConfig => ({
  enabled,
  url,
});

describe("setMcpEnabled", () => {
  it("forgets the custom URL when turning the feature off", async () => {
    const actor = makeActor(cfg(true, CUSTOM));

    await setMcpEnabled(asActor(actor), ANCHOR, false);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: false,
      url: [],
    });
  });

  it("leaves the custom URL alone when turning the feature on", async () => {
    const actor = makeActor(cfg(false, CUSTOM));

    await setMcpEnabled(asActor(actor), ANCHOR, true);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: true,
      url: [CUSTOM],
    });
  });
});

describe("trustAndEnableMcp", () => {
  it("enables the feature and stores the URL in a single write", async () => {
    const actor = makeActor(cfg(false, undefined));

    await trustAndEnableMcp(asActor(actor), ANCHOR, CUSTOM);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: true,
      url: [CUSTOM],
    });
  });
});

describe("clearMcpTrustedServer", () => {
  it("forgets the custom URL without turning the feature off", async () => {
    const actor = makeActor(cfg(true, CUSTOM));

    await clearMcpTrustedServer(asActor(actor), ANCHOR);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: true,
      url: [],
    });
  });

  it("keeps the feature off when it was already off", async () => {
    const actor = makeActor(cfg(false, CUSTOM));

    await clearMcpTrustedServer(asActor(actor), ANCHOR);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: false,
      url: [],
    });
  });
});

describe("write failures", () => {
  it("rejects with the canister's error so callers can roll back", async () => {
    const actor = makeActor(cfg(true, CUSTOM));
    actor.mcp_set_config.mockResolvedValue({ Err: "config is too large" });

    await expect(setMcpEnabled(asActor(actor), ANCHOR, false)).rejects.toThrow(
      "config is too large",
    );
  });
});
