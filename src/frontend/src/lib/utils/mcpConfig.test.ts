import { describe, it, expect } from "vitest";
import {
  originOf,
  isOriginTrusted,
  connectCallbackUrl,
  MCP_CONNECT_PATH,
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

describe("connectCallbackUrl", () => {
  const config = (url: string | undefined, enabled = true): McpConfig => ({
    enabled,
    url,
  });

  it("pins the connect endpoint to the configured origin + fixed path", () => {
    expect(connectCallbackUrl(config("https://mcp.id.ai/mcp"))).toBe(
      `https://mcp.id.ai${MCP_CONNECT_PATH}`,
    );
  });

  it("ignores the configured URL's path (derives from the origin only)", () => {
    // The trusted URL is the MCP resource endpoint; the connect endpoint is a
    // fixed path on the same origin, never a path taken from config or the link.
    expect(connectCallbackUrl(config("https://mcp.id.ai/some/deep/path"))).toBe(
      `https://mcp.id.ai${MCP_CONNECT_PATH}`,
    );
  });

  it("keeps a non-default port", () => {
    expect(connectCallbackUrl(config("https://mcp.id.ai:8443/mcp"))).toBe(
      `https://mcp.id.ai:8443${MCP_CONNECT_PATH}`,
    );
  });

  it("is undefined when the feature is disabled", () => {
    expect(
      connectCallbackUrl(config("https://mcp.id.ai/mcp", false)),
    ).toBeUndefined();
  });

  it("is undefined when no server URL is configured", () => {
    expect(connectCallbackUrl(config(undefined))).toBeUndefined();
  });

  it("is undefined when the configured URL is unparsable", () => {
    expect(connectCallbackUrl(config("not a url"))).toBeUndefined();
  });
});
