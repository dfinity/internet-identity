import { describe, it, expect, vi, afterEach } from "vitest";
import { parseMcpServerUrl, probeMcpServer } from "./mcpServer";

describe("parseMcpServerUrl", () => {
  it("accepts a bare https origin and normalizes its parts", () => {
    expect(parseMcpServerUrl("https://mcp.id.ai")).toEqual({
      url: "https://mcp.id.ai",
      origin: "https://mcp.id.ai",
      host: "mcp.id.ai",
    });
  });

  it("keeps the URL verbatim (path + query) but strips them from the origin", () => {
    // Trust matching is by origin; the verbatim URL lets the Settings probe hit
    // a path-based endpoint like https://host/mcp.
    expect(parseMcpServerUrl("https://mcp.id.ai/mcp?v=1")).toEqual({
      url: "https://mcp.id.ai/mcp?v=1",
      origin: "https://mcp.id.ai",
      host: "mcp.id.ai",
    });
  });

  it("carries a non-default port into origin and host", () => {
    expect(parseMcpServerUrl("https://mcp.id.ai:8443/mcp")).toEqual({
      url: "https://mcp.id.ai:8443/mcp",
      origin: "https://mcp.id.ai:8443",
      host: "mcp.id.ai:8443",
    });
  });

  it("trims surrounding whitespace before parsing", () => {
    expect(parseMcpServerUrl("  https://mcp.id.ai/mcp  ")?.url).toBe(
      "https://mcp.id.ai/mcp",
    );
  });

  it("preserves a trailing slash in the verbatim URL while the origin has none", () => {
    const parsed = parseMcpServerUrl("https://mcp.id.ai/");
    expect(parsed?.url).toBe("https://mcp.id.ai/");
    expect(parsed?.origin).toBe("https://mcp.id.ai");
  });

  it("rejects a plain-http URL (MCP connections are remote https only)", () => {
    expect(parseMcpServerUrl("http://mcp.id.ai/mcp")).toBeUndefined();
  });

  it("rejects a plain-http loopback URL", () => {
    // The gate is https-only, so an http loopback callback never parses.
    expect(parseMcpServerUrl("http://127.0.0.1:8080/callback")).toBeUndefined();
  });

  it("rejects non-http(s) schemes", () => {
    for (const raw of [
      "ws://mcp.id.ai",
      "ftp://mcp.id.ai",
      "data:text/html,hi",
      "javascript:alert(1)",
      "file:///etc/passwd",
    ]) {
      expect(parseMcpServerUrl(raw)).toBeUndefined();
    }
  });

  it("rejects malformed, relative, or empty input", () => {
    for (const raw of [
      "",
      "   ",
      "not a url",
      "//mcp.id.ai",
      "/mcp",
      "mcp.id.ai",
    ]) {
      expect(parseMcpServerUrl(raw)).toBeUndefined();
    }
  });
});

describe("probeMcpServer", () => {
  afterEach(() => {
    vi.unstubAllGlobals();
  });

  const RESOURCE = "https://mcp.id.ai/mcp";
  const WELL_KNOWN_BARE =
    "https://mcp.id.ai/.well-known/oauth-protected-resource";
  const WELL_KNOWN_PATH =
    "https://mcp.id.ai/.well-known/oauth-protected-resource/mcp";

  /** Route fetch by (url, method) → a Response, defaulting to a network error. */
  const route = (
    handler: (url: string, method: string) => Response | undefined,
  ): void => {
    vi.stubGlobal(
      "fetch",
      vi.fn((input: string, init?: RequestInit) => {
        const res = handler(
          String(input),
          (init?.method ?? "GET").toUpperCase(),
        );
        return res === undefined
          ? Promise.reject(new Error("network error"))
          : Promise.resolve(res);
      }),
    );
  };

  const metadataDoc = (resource: string): Response =>
    new Response(
      JSON.stringify({
        authorization_servers: ["https://mcp.id.ai"],
        resource,
      }),
      { status: 200, headers: { "content-type": "application/json" } },
    );

  it("confirms MCP via RFC 9728 metadata at the bare well-known path", async () => {
    route((url, method) =>
      method === "GET" && url === WELL_KNOWN_BARE
        ? metadataDoc(RESOURCE)
        : undefined,
    );
    await expect(probeMcpServer(RESOURCE)).resolves.toBe(true);
  });

  it("falls back to the resource-path-suffixed well-known document", async () => {
    route((url) => {
      if (url === WELL_KNOWN_BARE) return new Response("", { status: 404 });
      if (url === WELL_KNOWN_PATH) return metadataDoc(RESOURCE);
      return undefined;
    });
    await expect(probeMcpServer(RESOURCE)).resolves.toBe(true);
  });

  it("tolerates a trailing slash when matching the resource", async () => {
    route((url) =>
      url === WELL_KNOWN_BARE ? metadataDoc(`${RESOURCE}/`) : undefined,
    );
    await expect(probeMcpServer(RESOURCE)).resolves.toBe(true);
  });

  it("rejects metadata whose resource names a different URL", async () => {
    // Wrong resource in metadata, and the initialize fallback also fails.
    route((url, method) => {
      if (method === "GET" && url.includes("/.well-known/"))
        return metadataDoc("https://mcp.id.ai/other");
      if (method === "POST") return new Response("nope", { status: 200 });
      return undefined;
    });
    await expect(probeMcpServer(RESOURCE)).resolves.toBe(false);
  });

  it("falls back to a successful initialize handshake (plain JSON)", async () => {
    route((url, method) => {
      if (method === "GET") return new Response("", { status: 404 });
      if (method === "POST" && url === RESOURCE)
        return new Response(
          JSON.stringify({ jsonrpc: "2.0", id: 1, result: { ok: true } }),
          { status: 200 },
        );
      return undefined;
    });
    await expect(probeMcpServer(RESOURCE)).resolves.toBe(true);
  });

  it("recognizes an initialize response delivered as an SSE data frame", async () => {
    route((url, method) => {
      if (method === "GET") return new Response("", { status: 404 });
      if (method === "POST")
        return new Response(
          'event: message\ndata: {"jsonrpc":"2.0","id":1,"result":{}}\n\n',
          { status: 200 },
        );
      return undefined;
    });
    await expect(probeMcpServer(RESOURCE)).resolves.toBe(true);
  });

  it("returns false when neither metadata nor initialize confirms MCP", async () => {
    route((_url, method) => {
      if (method === "GET") return new Response("", { status: 404 });
      if (method === "POST")
        return new Response("<html>not mcp</html>", { status: 200 });
      return undefined;
    });
    await expect(probeMcpServer(RESOURCE)).resolves.toBe(false);
  });

  it("returns false when every request errors (network/CORS)", async () => {
    route(() => undefined); // all reject
    await expect(probeMcpServer(RESOURCE)).resolves.toBe(false);
  });
});
