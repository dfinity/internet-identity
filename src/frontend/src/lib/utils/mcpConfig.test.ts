import { describe, it, expect, vi } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { originOf, fromCanisterMcpConfig, readMcpConfig } from "./mcpConfig";

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

const CUSTOM = "https://mcp.acme.com/mcp";

describe("fromCanisterMcpConfig", () => {
  it("decodes a config with a trusted URL", () => {
    expect(fromCanisterMcpConfig([{ enabled: true, url: [CUSTOM] }])).toEqual({
      enabled: true,
      url: CUSTOM,
    });
  });

  it("decodes an absent URL as undefined", () => {
    expect(fromCanisterMcpConfig([{ enabled: true, url: [] }])).toEqual({
      enabled: true,
      url: undefined,
    });
  });

  it("returns undefined for an identity that never wrote a config", () => {
    expect(fromCanisterMcpConfig([])).toBeUndefined();
  });
});

const ANCHOR = BigInt(10_000);

describe("readMcpConfig", () => {
  it("decodes what the query returns", async () => {
    const actor = {
      mcp_get_config: vi
        .fn()
        .mockResolvedValue([{ enabled: true, url: [CUSTOM] }]),
    } as unknown as ActorSubclass<_SERVICE>;

    expect(await readMcpConfig(actor, ANCHOR)).toEqual({
      enabled: true,
      url: CUSTOM,
    });
  });

  it("reports an identity that never wrote a config", async () => {
    const actor = {
      mcp_get_config: vi.fn().mockResolvedValue([]),
    } as unknown as ActorSubclass<_SERVICE>;

    expect(await readMcpConfig(actor, ANCHOR)).toBeUndefined();
  });
});
