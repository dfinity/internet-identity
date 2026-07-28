import { describe, it, expect, vi } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  originOf,
  isOriginTrusted,
  trustedUrl,
  connectTrustedUrl,
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

describe("isOriginTrusted", () => {
  const trust = (url: string | undefined, enabled = true): McpConfig =>
    cfg(enabled, url);

  it("trusts an origin that matches the configured server's origin", () => {
    expect(
      isOriginTrusted(trust("https://mcp.id.ai/mcp"), "https://mcp.id.ai"),
    ).toBe(true);
  });

  it("matches by origin only, ignoring the trusted URL's path", () => {
    // The URL is kept verbatim (e.g. a path-based endpoint), but trust is an
    // origin decision — the path must not narrow or widen the match.
    expect(
      isOriginTrusted(
        trust("https://mcp.id.ai/some/deep/path"),
        "https://mcp.id.ai",
      ),
    ).toBe(true);
  });

  it("does not trust when the feature is disabled, even if the origin matches", () => {
    expect(
      isOriginTrusted(
        trust("https://mcp.id.ai/mcp", false),
        "https://mcp.id.ai",
      ),
    ).toBe(false);
  });

  it("does not trust when no server URL is configured", () => {
    expect(isOriginTrusted(trust(undefined), "https://mcp.id.ai")).toBe(false);
  });

  it("rejects a different host", () => {
    expect(
      isOriginTrusted(
        trust("https://mcp.id.ai/mcp"),
        "https://evil.example.com",
      ),
    ).toBe(false);
  });

  it("rejects a scheme mismatch (http vs https is a different origin)", () => {
    expect(
      isOriginTrusted(trust("https://mcp.id.ai/mcp"), "http://mcp.id.ai"),
    ).toBe(false);
  });

  it("rejects a port mismatch", () => {
    expect(
      isOriginTrusted(trust("https://mcp.id.ai:8443/mcp"), "https://mcp.id.ai"),
    ).toBe(false);
  });

  it("rejects a subdomain that is not the exact origin", () => {
    expect(
      isOriginTrusted(trust("https://mcp.id.ai/mcp"), "https://sub.mcp.id.ai"),
    ).toBe(false);
  });

  it("does not trust when the configured URL is unparsable", () => {
    expect(isOriginTrusted(trust("not a url"), "https://mcp.id.ai")).toBe(
      false,
    );
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

const OFFICIAL = "https://official-mcp.id.ai/mcp";
const CUSTOM = "https://mcp.acme.com/mcp";

/** A stored config. `undefined` stands for an identity that never wrote one. */
const cfg = (enabled: boolean, url: string | undefined): McpConfig => ({
  enabled,
  url,
});

describe("trustedUrl", () => {
  it("falls back to the official connector when no custom URL is set", () => {
    expect(trustedUrl(cfg(true, undefined), OFFICIAL)).toBe(OFFICIAL);
  });

  it("prefers the custom connector over the official one", () => {
    expect(trustedUrl(cfg(true, CUSTOM), OFFICIAL)).toBe(CUSTOM);
  });

  it("trusts nothing while the feature is disabled", () => {
    expect(trustedUrl(cfg(false, undefined), OFFICIAL)).toBeUndefined();
    expect(trustedUrl(cfg(false, CUSTOM), OFFICIAL)).toBeUndefined();
  });

  it("trusts nothing when enabled with no custom and no official connector", () => {
    expect(trustedUrl(cfg(true, undefined), undefined)).toBeUndefined();
  });
});

describe("connectTrustedUrl", () => {
  it("offers the official connector to an identity that never configured MCP (no stored config)", () => {
    expect(connectTrustedUrl(undefined, OFFICIAL)).toBe(OFFICIAL);
  });

  it("blocks once the feature has been switched off", () => {
    // The reason `configured` exists: this reads identically to a fresh
    // identity, but must be sent back to Settings rather than silently
    // re-enabled by a connect link.
    expect(connectTrustedUrl(cfg(false, undefined), OFFICIAL)).toBeUndefined();
  });

  it("offers the official connector to an enabled config with no custom server", () => {
    expect(connectTrustedUrl(cfg(true, undefined), OFFICIAL)).toBe(OFFICIAL);
  });

  it("lets a custom connector displace the official one", () => {
    expect(connectTrustedUrl(cfg(true, CUSTOM), OFFICIAL)).toBe(CUSTOM);
  });

  it("requires the feature enabled for a custom connector", () => {
    expect(connectTrustedUrl(cfg(false, CUSTOM), OFFICIAL)).toBeUndefined();
  });

  it("offers nothing without an official connector", () => {
    expect(connectTrustedUrl(undefined, undefined)).toBeUndefined();
  });
});

describe("isOriginTrusted with an official connector", () => {
  it("accepts the official origin when no custom URL is set", () => {
    expect(
      isOriginTrusted(
        cfg(true, undefined),
        "https://official-mcp.id.ai",
        OFFICIAL,
      ),
    ).toBe(true);
  });

  it("stops accepting the official origin once a custom URL is set", () => {
    expect(
      isOriginTrusted(
        cfg(true, CUSTOM),
        "https://official-mcp.id.ai",
        OFFICIAL,
      ),
    ).toBe(false);
  });

  it("rejects the official origin after the feature was switched off", () => {
    expect(
      isOriginTrusted(
        cfg(false, undefined),
        "https://official-mcp.id.ai",
        OFFICIAL,
      ),
    ).toBe(false);
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
