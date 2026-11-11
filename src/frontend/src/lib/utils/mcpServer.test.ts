import { describe, it, expect, vi, afterEach } from "vitest";
import {
  isLoopbackUrl,
  LOCAL_MCP_SERVER_URL,
  parseMcpServerUrl,
  probeMcpServer,
  trustsOrigin,
} from "./mcpServer";

describe("parseMcpServerUrl", () => {
  it("accepts a bare https origin and normalizes its parts", () => {
    expect(parseMcpServerUrl("https://mcp.id.ai")).toEqual({
      url: "https://mcp.id.ai",
      origin: "https://mcp.id.ai",
      host: "mcp.id.ai",
      isLoopback: false,
    });
  });

  it("keeps the URL verbatim (path + query) but strips them from the origin", () => {
    // Trust matching is by origin; the verbatim URL lets the Settings probe hit
    // a path-based endpoint like https://host/mcp.
    expect(parseMcpServerUrl("https://mcp.id.ai/mcp?v=1")).toEqual({
      url: "https://mcp.id.ai/mcp?v=1",
      origin: "https://mcp.id.ai",
      host: "mcp.id.ai",
      isLoopback: false,
    });
  });

  it("carries a non-default port into origin and host", () => {
    expect(parseMcpServerUrl("https://mcp.id.ai:8443/mcp")).toEqual({
      url: "https://mcp.id.ai:8443/mcp",
      origin: "https://mcp.id.ai:8443",
      host: "mcp.id.ai:8443",
      isLoopback: false,
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

  it("accepts a plain-http loopback URL, port and path included", () => {
    // A local server binds a fresh port per sign-in, so its callback carries
    // one; the port-less stored form is what trust matching compares against.
    expect(parseMcpServerUrl("http://127.0.0.1:8080/callback")).toEqual({
      url: "http://127.0.0.1:8080/callback",
      origin: "http://127.0.0.1:8080",
      host: "127.0.0.1:8080",
      isLoopback: true,
    });
  });

  it("accepts the port-less local form that Settings stores", () => {
    expect(parseMcpServerUrl(LOCAL_MCP_SERVER_URL)).toEqual({
      url: "http://127.0.0.1",
      origin: "http://127.0.0.1",
      host: "127.0.0.1",
      isLoopback: true,
    });
  });

  it("rejects loopback lookalikes that aren't the IPv4 literal", () => {
    // `localhost` is a name, `::1` can't be expressed in CSP's host-source
    // grammar (so it would die at delivery), `0.0.0.0` isn't loopback, and a
    // name that merely resolves to loopback is rebinding surface.
    for (const raw of [
      "http://localhost:8080/callback",
      "http://[::1]:8080/callback",
      "http://0.0.0.0:8080/callback",
      "http://127.0.0.1.nip.io:8080/callback",
      "http://127.0.0.1.evil.example.com/callback",
    ]) {
      expect(parseMcpServerUrl(raw)).toBeUndefined();
    }
  });

  it("rejects an https loopback URL", () => {
    // A local listener can't present a CA-trusted cert; http is the loopback
    // shape, and accepting https here would only widen what matches.
    expect(
      parseMcpServerUrl("https://127.0.0.1:8080/callback")?.isLoopback,
    ).toBe(false);
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

  it("derives origin/host from the real host, not embedded userinfo", () => {
    // https://mcp.id.ai@evil.example.com/ actually points at evil.example.com;
    // the origin (what trust matching uses) and host (what the consent screen
    // shows) must both name the real host, so the userinfo can't spoof either.
    const parsed = parseMcpServerUrl("https://mcp.id.ai@evil.example.com/mcp");
    expect(parsed?.origin).toBe("https://evil.example.com");
    expect(parsed?.host).toBe("evil.example.com");
    expect(parsed?.origin).not.toContain("mcp.id.ai");
  });
});

describe("isLoopbackUrl", () => {
  it("is true only for the http IPv4 loopback literal", () => {
    expect(isLoopbackUrl("http://127.0.0.1")).toBe(true);
    expect(isLoopbackUrl("http://127.0.0.1:52341/callback")).toBe(true);
    for (const raw of [
      "https://127.0.0.1",
      "http://localhost",
      "http://[::1]",
      "http://0.0.0.0",
      "https://mcp.id.ai",
      "not a url",
    ]) {
      expect(isLoopbackUrl(raw)).toBe(false);
    }
  });
});

describe("trustsOrigin", () => {
  it("matches a remote server by exact origin", () => {
    expect(trustsOrigin("https://mcp.id.ai/mcp", "https://mcp.id.ai")).toBe(
      true,
    );
    expect(
      trustsOrigin("https://mcp.id.ai/mcp", "https://mcp.id.ai:8443"),
    ).toBe(false);
    expect(trustsOrigin("https://mcp.id.ai/mcp", "https://evil.example")).toBe(
      false,
    );
  });

  it("matches a local server on any port, since it binds a fresh one", () => {
    expect(trustsOrigin(LOCAL_MCP_SERVER_URL, "http://127.0.0.1:52341")).toBe(
      true,
    );
    expect(trustsOrigin(LOCAL_MCP_SERVER_URL, "http://127.0.0.1:9")).toBe(true);
  });

  it("does not let a local entry trust anything off loopback", () => {
    for (const origin of [
      "https://evil.example",
      "http://localhost:52341",
      "http://[::1]:52341",
      "https://127.0.0.1:52341",
    ]) {
      expect(trustsOrigin(LOCAL_MCP_SERVER_URL, origin)).toBe(false);
    }
  });

  it("does not let a remote entry trust loopback", () => {
    expect(trustsOrigin("https://mcp.id.ai", "http://127.0.0.1:52341")).toBe(
      false,
    );
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

  it("rejects metadata without an authorization_servers array (RFC 9728 requires both fields)", async () => {
    // A resource match alone is not enough: an II-usable MCP server is
    // OAuth-protected, so the metadata must also name authorization servers.
    route((url, method) => {
      if (method === "GET" && url.includes("/.well-known/"))
        return new Response(JSON.stringify({ resource: RESOURCE }), {
          status: 200,
          headers: { "content-type": "application/json" },
        });
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
