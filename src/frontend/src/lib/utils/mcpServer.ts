/**
 * Helpers for the MCP server URL a user trusts: parsing/normalizing it and a
 * best-effort probe that it actually speaks MCP. Shared by the Settings UI
 * (where the user enters the URL) and the `/mcp` connect flow (which matches the
 * request's callback origin against the trusted server).
 *
 * Two kinds of server can be trusted:
 *  - a **remote** one, at an https URL the user enters;
 *  - a **local** one, a program listening on loopback. It is stored as the
 *    port-less {@link LOCAL_MCP_SERVER_URL}, because a local server binds a
 *    fresh port per sign-in (it cannot promise one ahead of time), so trust
 *    matching for it is by host, not by origin — see {@link trustsOrigin}.
 */

/** The stored trusted URL that stands for "a local server on this computer".
 *  Port-less on purpose: the port is whatever the local program binds for the
 *  sign-in it is starting, so the stored value is the stable part. */
export const LOCAL_MCP_SERVER_URL = "http://127.0.0.1";

/** The one loopback host a local server may listen on.
 *
 *  Only the IPv4 literal. Not `localhost` (a name, and so subject to whatever
 *  resolves it), not `0.0.0.0`, and not a name that merely resolves to
 *  loopback — those are rebinding surface, and the browser cannot be asked
 *  where a name will point. Not `::1` either: CSP's host-source grammar can't
 *  express IPv6 literals, so `http://[::1]:*` would be an invalid `form-action`
 *  source that browsers ignore, and a `::1` callback would pass here only to
 *  die at delivery — the same trap the /cli flow documents. */
const LOOPBACK_HOSTNAME = "127.0.0.1";

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
  /** Whether this is a local server on the loopback interface, which trust
   *  matching treats by host rather than by origin. */
  isLoopback: boolean;
}

/**
 * Whether `url` names a local server on loopback — the port-less stored form
 * as well as the ported callbacks a local server actually listens on.
 */
export const isLoopbackUrl = (url: string): boolean => {
  try {
    const parsed = new URL(url);
    return parsed.protocol === "http:" && parsed.hostname === LOOPBACK_HOSTNAME;
  } catch {
    return false;
  }
};

/**
 * Whether a server at `origin` is the one `trustedUrl` names.
 *
 * The single trust comparison, used both by the connect flow's pre-filter and
 * by its authoritative gate against the *certified* `trusted_url` — one
 * function so the two can't drift into disagreeing about what is trusted.
 *
 * Remote servers match by exact origin. A local server matches by host only:
 * the stored URL carries no port because the program binds a fresh one per
 * sign-in, so any port on the loopback host is the trusted server. That is a
 * real widening, and it is the point — trusting a local server means trusting
 * the programs on this computer, which is why enabling one is its own
 * deliberate choice in Settings and why the connect flow says so before the
 * first local sign-in on a machine.
 */
export const trustsOrigin = (trustedUrl: string, origin: string): boolean => {
  if (isLoopbackUrl(trustedUrl)) {
    return isLoopbackUrl(origin);
  }
  try {
    return new URL(trustedUrl).origin === origin;
  } catch {
    return false;
  }
};

/**
 * Parses an MCP server URL into its normalized parts, or `undefined` when it
 * isn't an acceptable target: an https URL for a remote server, or an
 * `http://127.0.0.1[:port]` URL for a local one.
 */
export const parseMcpServerUrl = (raw: string): McpServer | undefined => {
  const trimmed = raw.trim();
  let url: URL;
  try {
    url = new URL(trimmed);
  } catch {
    return undefined;
  }
  const isLoopback = isLoopbackUrl(trimmed);
  if (url.protocol !== "https:" && !isLoopback) {
    return undefined;
  }
  // Keep the URL as entered rather than `url.href`, which appends a trailing
  // slash to a bare origin. Trust matching is by origin; verification matches
  // the canonical resource URL.
  return { url: trimmed, origin: url.origin, host: url.host, isLoopback };
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
