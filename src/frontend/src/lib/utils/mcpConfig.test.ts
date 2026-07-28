import { describe, it, expect, vi } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import { IDL } from "@icp-sdk/core/candid";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { idlFactory } from "$lib/generated/internet_identity_idl";
import {
  originOf,
  readMcpConfig,
  setMcpEnabled,
  trustAndEnableMcp,
  clearMcpTrustedServer,
  type McpConfig,
} from "./mcpConfig";

describe("mcp_get_config in the canister interface", () => {
  // The whole trust decision at /mcp rests on this read, so the actor must send
  // it as an update (certified by consensus) and never as a query (signed by the
  // single node that answered, hence forgeable by a malicious replica or
  // boundary node — see `readMcpConfig`). agent-js picks the call type straight
  // from the IDL annotation, so asserting on the generated interface is what
  // actually pins the behaviour: a regenerated binding from a `.did` that put
  // `query` back would silently downgrade every call site.
  const service = idlFactory({ IDL }) as IDL.ServiceClass;
  const method = service._fields.find(([name]) => name === "mcp_get_config");

  it("is declared", () => {
    expect(method).toBeDefined();
  });

  it("is not annotated as a query, so the actor makes an update call", () => {
    expect(method?.[1].annotations).not.toContain("query");
    expect(method?.[1].annotations).toEqual([]);
  });
});

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

describe("readMcpConfig", () => {
  it("maps the canister's optionals onto the frontend shape", async () => {
    const actor = makeActor(cfg(true, CUSTOM));

    await expect(readMcpConfig(asActor(actor), ANCHOR)).resolves.toEqual(
      cfg(true, CUSTOM),
    );
  });

  it("reports an identity that never wrote a config as undefined", async () => {
    const actor = makeActor(cfg(false, undefined));
    // `opt McpConfig` = `null`: never configured, which /mcp treats differently
    // from a stored config that is switched off.
    actor.mcp_get_config.mockResolvedValue([]);

    await expect(
      readMcpConfig(asActor(actor), ANCHOR),
    ).resolves.toBeUndefined();
  });
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
