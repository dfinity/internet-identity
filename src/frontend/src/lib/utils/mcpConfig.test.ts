import { describe, it, expect, vi } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  originOf,
  isOriginTrusted,
  setMcpEnabled,
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

describe("setMcpEnabled", () => {
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
