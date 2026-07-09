import { describe, expect, it } from "vitest";
import { load, type McpParams } from "./+page";

const HOUR = 60 * 60;
const MIN_TTL = 10 * 60;
const MAX_TTL = 30 * 24 * 60 * 60;

// A valid base64url registration key (the server's public per-connect key `X`).
const REGISTRATION_KEY = "cmVnaXN0cmF0aW9uLWtleQ";

// A complete, valid fragment with an optional `ttl`. The callback is an
// accepted https origin; the only key material is the server's *public*
// registration key (II mints a `P_reg -> X` delegation for it — nothing secret
// travels in the fragment).
const fragment = (ttl?: string): string => {
  const params = new URLSearchParams();
  params.set("registration_key", REGISTRATION_KEY);
  params.set("callback", "https://mcp.example.com/cb");
  params.set("state", "opaque-state");
  if (ttl !== undefined) {
    params.set("ttl", ttl);
  }
  return params.toString();
};

// `load` is synchronous here; the `PageLoad` signature widens the return to
// MaybePromise<void | ...>, so narrow it back for the assertions.
const loadParams = (fragmentString: string): McpParams =>
  (
    load({
      url: new URL(`https://id.ai/mcp#${fragmentString}`),
    } as Parameters<typeof load>[0]) as { params: McpParams }
  ).params;

const loadTtl = (ttl?: string): McpParams => loadParams(fragment(ttl));

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

describe("/mcp load: request validation", () => {
  it("surfaces the registration key verbatim on a valid request", () => {
    const parsed = loadTtl();
    expect(parsed.kind).toBe("valid");
    if (parsed.kind === "valid") {
      expect(parsed.registrationKey).toBe(REGISTRATION_KEY);
    }
  });

  it("rejects a fragment without a registration_key", () => {
    const params = new URLSearchParams(fragment());
    params.delete("registration_key");
    expect(loadParams(params.toString()).kind).toBe("invalid");
  });

  it("rejects a present-but-empty or undecodable registration_key", () => {
    const empty = new URLSearchParams(fragment());
    empty.set("registration_key", "");
    expect(loadParams(empty.toString()).kind).toBe("invalid");

    const garbage = new URLSearchParams(fragment());
    // `*` is outside the base64url alphabet, so decoding throws → invalid.
    garbage.set("registration_key", "***not-base64***");
    expect(loadParams(garbage.toString()).kind).toBe("invalid");
  });

  it("rejects a fragment without a callback", () => {
    const params = new URLSearchParams(fragment());
    params.delete("callback");
    expect(loadParams(params.toString()).kind).toBe("invalid");
  });

  it("rejects a fragment without a state", () => {
    const params = new URLSearchParams(fragment());
    params.delete("state");
    expect(loadParams(params.toString()).kind).toBe("invalid");
  });

  it("tolerates (and ignores) an unknown extra param", () => {
    const params = new URLSearchParams(fragment());
    params.set("public_key", "AAAA");
    const parsed = loadParams(params.toString());
    expect(parsed.kind).toBe("valid");
    if (parsed.kind === "valid") {
      expect("publicKey" in parsed).toBe(false);
    }
  });

  it("rejects a present-but-empty callback or state", () => {
    // `callback=` / `state=` parse as "" (not null) from URLSearchParams; the
    // guards must treat empty the same as absent.
    const emptyCallback = new URLSearchParams(fragment());
    emptyCallback.set("callback", "");
    expect(loadParams(emptyCallback.toString()).kind).toBe("invalid");

    const emptyState = new URLSearchParams(fragment());
    emptyState.set("state", "");
    expect(loadParams(emptyState.toString()).kind).toBe("invalid");
  });
});

describe("/mcp load: callback validation", () => {
  const withCallback = (callback: string): McpParams => {
    const params = new URLSearchParams(fragment());
    params.set("callback", callback);
    return loadParams(params.toString());
  };

  it("rejects a malformed (non-URL) callback", () => {
    expect(withCallback("not a url").kind).toBe("invalid");
    expect(withCallback("mcp.example.com/cb").kind).toBe("invalid");
    expect(withCallback("/cb").kind).toBe("invalid");
  });

  it("rejects a non-http(s) callback scheme", () => {
    for (const callback of [
      "javascript:alert(1)",
      "data:text/html,hi",
      "ws://mcp.example.com/cb",
      "ftp://mcp.example.com/cb",
      "file:///etc/passwd",
    ]) {
      expect(withCallback(callback).kind).toBe("invalid");
    }
  });

  it("accepts an http callback structurally (the https-only gate is the component's)", () => {
    // The load/component split: `load` is a structural check only (absolute
    // http(s) URL) because it also runs at prerender; the stricter https-only
    // remote gate (`parseMcpServerUrl`) runs in the component, which shows the
    // invalid screen. This pins the split so a refactor moving the https check
    // out of the component doesn't silently drop it from both layers.
    const parsed = withCallback("http://mcp.example.com/cb");
    expect(parsed.kind).toBe("valid");
    if (parsed.kind === "valid") {
      expect(parsed.callback).toBe("http://mcp.example.com/cb");
    }
  });
});
