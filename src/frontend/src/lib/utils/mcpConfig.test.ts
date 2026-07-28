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
  const trust = (url: string | undefined, enabled = true): McpConfig => ({
    enabled,
    url,
  });

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
const CUSTOM_URL = "https://mcp.acme.com/mcp";

const makeActor = (current: McpConfig) => ({
  mcp_get_config: vi.fn().mockResolvedValue({
    enabled: current.enabled,
    url: current.url === undefined ? [] : [current.url],
  }),
  mcp_set_config: vi.fn().mockResolvedValue({ Ok: null }),
});

const asActor = (actor: ReturnType<typeof makeActor>) =>
  actor as unknown as ActorSubclass<_SERVICE>;

describe("trustedUrl", () => {
  const OFFICIAL = "https://official-mcp.id.ai/mcp";

  it("falls back to the official connector when no custom URL is set", () => {
    expect(trustedUrl({ enabled: true, url: undefined }, OFFICIAL)).toBe(
      OFFICIAL,
    );
  });

  it("prefers the custom connector over the official one", () => {
    expect(
      trustedUrl({ enabled: true, url: "https://mcp.acme.com/mcp" }, OFFICIAL),
    ).toBe("https://mcp.acme.com/mcp");
  });

  it("trusts nothing while the feature is disabled", () => {
    expect(
      trustedUrl({ enabled: false, url: undefined }, OFFICIAL),
    ).toBeUndefined();
    expect(
      trustedUrl({ enabled: false, url: "https://mcp.acme.com/mcp" }, OFFICIAL),
    ).toBeUndefined();
  });

  it("trusts nothing when enabled with no custom and no official connector", () => {
    expect(
      trustedUrl({ enabled: true, url: undefined }, undefined),
    ).toBeUndefined();
  });
});

describe("connectTrustedUrl", () => {
  const OFFICIAL = "https://official-mcp.id.ai/mcp";
  const CUSTOM = "https://mcp.acme.com/mcp";

  it("offers the official connector to an identity that never enabled the feature", () => {
    expect(
      connectTrustedUrl({ enabled: false, url: undefined }, OFFICIAL),
    ).toBe(OFFICIAL);
  });

  it("still offers it after the feature was switched off", () => {
    // Completing the consent at /mcp is what turns it back on, so a disabled
    // config is not a permanent block on connecting.
    expect(
      connectTrustedUrl({ enabled: false, url: undefined }, OFFICIAL),
    ).toBe(OFFICIAL);
  });

  it("lets a custom connector displace the official one", () => {
    expect(connectTrustedUrl({ enabled: true, url: CUSTOM }, OFFICIAL)).toBe(
      CUSTOM,
    );
  });

  it("requires the feature enabled for a custom connector", () => {
    expect(
      connectTrustedUrl({ enabled: false, url: CUSTOM }, OFFICIAL),
    ).toBeUndefined();
  });

  it("offers nothing without an official connector", () => {
    expect(
      connectTrustedUrl({ enabled: false, url: undefined }, undefined),
    ).toBeUndefined();
  });
});

describe("isOriginTrusted with an official connector", () => {
  const OFFICIAL = "https://official-mcp.id.ai/mcp";

  it("accepts the official origin when no custom URL is set", () => {
    expect(
      isOriginTrusted(
        { enabled: true, url: undefined },
        "https://official-mcp.id.ai",
        OFFICIAL,
      ),
    ).toBe(true);
  });

  it("stops accepting the official origin once a custom URL is set", () => {
    expect(
      isOriginTrusted(
        { enabled: true, url: "https://mcp.acme.com/mcp" },
        "https://official-mcp.id.ai",
        OFFICIAL,
      ),
    ).toBe(false);
  });
});

describe("setMcpEnabled", () => {
  it("forgets the custom URL when turning the feature off", async () => {
    const actor = makeActor({ enabled: true, url: CUSTOM_URL });

    await setMcpEnabled(asActor(actor), ANCHOR, false);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: false,
      url: [],
    });
  });

  it("leaves the custom URL alone when turning the feature on", async () => {
    const actor = makeActor({ enabled: false, url: CUSTOM_URL });

    await setMcpEnabled(asActor(actor), ANCHOR, true);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: true,
      url: [CUSTOM_URL],
    });
  });
});

describe("trustAndEnableMcp", () => {
  it("enables the feature and stores the URL in a single write", async () => {
    const actor = makeActor({ enabled: false, url: undefined });

    await trustAndEnableMcp(asActor(actor), ANCHOR, CUSTOM_URL);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: true,
      url: [CUSTOM_URL],
    });
  });
});

describe("clearMcpTrustedServer", () => {
  it("forgets the custom URL without turning the feature off", async () => {
    const actor = makeActor({ enabled: true, url: CUSTOM_URL });

    await clearMcpTrustedServer(asActor(actor), ANCHOR);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: true,
      url: [],
    });
  });

  it("keeps the feature off when it was already off", async () => {
    const actor = makeActor({ enabled: false, url: CUSTOM_URL });

    await clearMcpTrustedServer(asActor(actor), ANCHOR);

    expect(actor.mcp_set_config).toHaveBeenCalledWith(ANCHOR, {
      enabled: false,
      url: [],
    });
  });
});

describe("write failures", () => {
  it("rejects with the canister's error so callers can roll back", async () => {
    const actor = makeActor({ enabled: true, url: CUSTOM_URL });
    actor.mcp_set_config.mockResolvedValue({ Err: "config is too large" });

    await expect(setMcpEnabled(asActor(actor), ANCHOR, false)).rejects.toThrow(
      "config is too large",
    );
  });
});
