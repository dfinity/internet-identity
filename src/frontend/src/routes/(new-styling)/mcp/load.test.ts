import { describe, expect, it } from "vitest";
import { load, type McpParams } from "./+page";

const HOUR = 60 * 60;
const MIN_TTL = 10 * 60;
const MAX_TTL = 30 * 24 * 60 * 60;

// A complete, valid fragment with an optional `ttl`. `public_key` is any
// base64url-decodable string; the callback is an accepted https origin.
const fragment = (ttl?: string): string => {
  const params = new URLSearchParams();
  params.set("public_key", "AAAA");
  params.set("callback", "https://mcp.example.com/cb");
  params.set("state", "opaque-state");
  if (ttl !== undefined) {
    params.set("ttl", ttl);
  }
  return params.toString();
};

// `load` is synchronous here; the `PageLoad` signature widens the return to
// MaybePromise<void | ...>, so narrow it back for the assertions.
const loadTtl = (ttl?: string): McpParams =>
  (
    load({
      url: new URL(`https://id.ai/mcp#${fragment(ttl)}`),
    } as Parameters<typeof load>[0]) as { params: McpParams }
  ).params;

describe("/mcp load: ttl parsing", () => {
  it("defaults to 1 hour when ttl is omitted", () => {
    const params = loadTtl();
    expect(params.kind).toBe("valid");
    if (params.kind === "valid") {
      expect(params.ttlSeconds).toBe(HOUR);
    }
  });

  it("honours an in-range value exactly", () => {
    const params = loadTtl(String(2 * HOUR));
    expect(params.kind).toBe("valid");
    if (params.kind === "valid") {
      expect(params.ttlSeconds).toBe(2 * HOUR);
    }
  });

  it("clamps a below-floor value up to 10 minutes", () => {
    // 60s — what a server still sending minutes-as-seconds would emit.
    const params = loadTtl("60");
    expect(params.kind).toBe("valid");
    if (params.kind === "valid") {
      expect(params.ttlSeconds).toBe(MIN_TTL);
    }
  });

  it("clamps an above-cap value down to 30 days", () => {
    const params = loadTtl(String(MAX_TTL + 1));
    expect(params.kind).toBe("valid");
    if (params.kind === "valid") {
      expect(params.ttlSeconds).toBe(MAX_TTL);
    }
  });

  it("rejects a non-numeric ttl", () => {
    expect(loadTtl("abc").kind).toBe("invalid");
  });

  it("rejects a non-positive ttl", () => {
    expect(loadTtl("0").kind).toBe("invalid");
    expect(loadTtl("-5").kind).toBe("invalid");
  });
});
