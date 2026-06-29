/**
 * Helpers for the MCP server URL a user trusts: parsing/normalizing it and a
 * best-effort probe that it actually speaks MCP. Shared by the Settings UI
 * (where the user enters the URL) and the `/mcp` connect flow (which matches the
 * request's callback origin against the trusted server).
 */

export interface McpServer {
  /** The URL the user entered, kept verbatim (not slash-normalized) so it
   *  displays as typed and we can probe a path-based endpoint like
   *  `https://host/mcp`, not just the origin root. */
  url: string;
  /** Normalized origin: scheme + host[:port], no path. Used for trust matching
   *  and as the `form-action` target. */
  origin: string;
  /** host[:port], for display. */
  host: string;
}

/**
 * Parses an MCP server URL into its normalized parts, or `undefined` when it
 * isn't an acceptable target. MCP connections are to remote servers only, so
 * the URL must be https (a plain-http or loopback URL is rejected).
 */
export const parseMcpServerUrl = (raw: string): McpServer | undefined => {
  const trimmed = raw.trim();
  let url: URL;
  try {
    url = new URL(trimmed);
  } catch {
    return undefined;
  }
  if (url.protocol !== "https:") {
    return undefined;
  }
  // Keep the URL as entered rather than `url.href`, which appends a trailing
  // slash to a bare origin. Trust matching is by origin; verification matches
  // the canonical resource URL.
  return { url: trimmed, origin: url.origin, host: url.host };
};

/** MCP protocol revision we advertise in the `initialize` probe. */
const MCP_PROTOCOL_VERSION = "2025-06-18";
const PROBE_TIMEOUT_MS = 5_000;

/**
 * Probes whether `url` actually speaks MCP — not merely that "something loads".
 *
 * An MCP server usable by II is OAuth-protected (it consumes the II-issued
 * delegation as a bearer token) and, per the MCP authorization spec (RFC 9728),
 * advertises an OAuth Protected Resource Metadata document under a
 * `/.well-known/oauth-protected-resource` path. That endpoint is a simple,
 * CORS-enabled GET, so the browser can read it even though the MCP endpoint
 * itself answers `401` and typically blocks cross-origin reads. That metadata
 * is the reliable signal.
 *
 * The `initialize` JSON-RPC handshake is kept as a fallback for the rarer
 * CORS-permissive / unprotected MCP server. Resolves `false` only when neither
 * confirms MCP (a network error, a timeout, a CORS-blocked response, or a
 * non-MCP answer) — callers treat that as "couldn't verify" (warn + offer a
 * re-check), never a hard block.
 */
export const probeMcpServer = async (url: string): Promise<boolean> => {
  if (await hasProtectedResourceMetadata(url)) {
    return true;
  }
  return initializeHandshakeSucceeds(url);
};

/**
 * Whether the server advertises an RFC 9728 Protected Resource Metadata
 * document naming exactly this URL as its `resource`. The well-known segment is
 * inserted between host and path; deployments vary between the bare path and
 * the resource-path-suffixed form, so try both.
 */
const hasProtectedResourceMetadata = async (url: string): Promise<boolean> => {
  let parsed: URL;
  try {
    parsed = new URL(url);
  } catch {
    return false;
  }
  const candidates = [
    `${parsed.origin}/.well-known/oauth-protected-resource`,
    `${parsed.origin}/.well-known/oauth-protected-resource${parsed.pathname}`,
  ];
  for (const candidate of candidates) {
    if (await describesResource(candidate, parsed.href)) {
      return true;
    }
  }
  return false;
};

/**
 * Whether the metadata at `metadataUrl` is a Protected Resource Metadata
 * document whose `resource` is exactly `resourceHref`. Matching the canonical
 * `resource` — not merely its origin — means the real MCP endpoint
 * (`https://host/mcp`) verifies while the bare origin or a wrong path does not.
 */
const describesResource = async (
  metadataUrl: string,
  resourceHref: string,
): Promise<boolean> => {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), PROBE_TIMEOUT_MS);
  try {
    const response = await fetch(metadataUrl, { signal: controller.signal });
    if (!response.ok) {
      return false;
    }
    const doc: unknown = await response.json();
    if (typeof doc !== "object" || doc === null) {
      return false;
    }
    const record = doc as Record<string, unknown>;
    // RFC 9728 requires both fields; require `resource` to name this exact URL.
    return (
      Array.isArray(record.authorization_servers) &&
      typeof record.resource === "string" &&
      sameResource(record.resource, resourceHref)
    );
  } catch {
    return false;
  } finally {
    clearTimeout(timer);
  }
};

/** Compares two resource URLs, tolerating a trailing slash. */
const sameResource = (a: string, b: string): boolean => {
  const normalize = (value: string): string | undefined => {
    try {
      return new URL(value).href.replace(/\/+$/, "");
    } catch {
      return undefined;
    }
  };
  const normalized = normalize(a);
  return normalized !== undefined && normalized === normalize(b);
};

/** Sends the MCP `initialize` request and checks for a JSON-RPC response. */
const initializeHandshakeSucceeds = async (url: string): Promise<boolean> => {
  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), PROBE_TIMEOUT_MS);
  try {
    const response = await fetch(url, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        accept: "application/json, text/event-stream",
      },
      body: JSON.stringify({
        jsonrpc: "2.0",
        id: 1,
        method: "initialize",
        params: {
          protocolVersion: MCP_PROTOCOL_VERSION,
          capabilities: {},
          clientInfo: { name: "internet-identity", version: "1" },
        },
      }),
      signal: controller.signal,
    });
    return isJsonRpcResponse(await response.text());
  } catch {
    return false;
  } finally {
    clearTimeout(timer);
  }
};

/** Lines of an SSE body that carry a payload (`data: <json>`). */
const sseDataFrames = (body: string): string[] =>
  body
    .split(/\r?\n/)
    .filter((line) => line.startsWith("data:"))
    .map((line) => line.slice("data:".length).trim());

/**
 * Whether `body` contains a JSON-RPC response (a `result` or an `error`).
 * Tolerates both a plain JSON response and an SSE stream (the two
 * Streamable-HTTP shapes).
 */
const isJsonRpcResponse = (body: string): boolean => {
  const candidates = [body.trim(), ...sseDataFrames(body)];
  return candidates.some((candidate) => {
    let message: unknown;
    try {
      message = JSON.parse(candidate);
    } catch {
      return false;
    }
    if (typeof message !== "object" || message === null) {
      return false;
    }
    const { jsonrpc, result, error } = message as Record<string, unknown>;
    return (
      jsonrpc === "2.0" &&
      ((typeof result === "object" && result !== null) ||
        (typeof error === "object" && error !== null))
    );
  });
};
