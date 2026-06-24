/**
 * Helpers for the MCP server URL a user trusts: parsing/normalizing it and a
 * best-effort reachability probe. Shared by the Settings allowlist UI (where
 * the user enters the URL) and the `/mcp` connect flow (which reads the origin
 * from the request's callback).
 */

export interface McpServer {
  /** Normalized origin: scheme + host[:port], no path. Used for trust matching
   *  and as the `form-action` target. */
  origin: string;
  /** host[:port], for display. */
  host: string;
}

/**
 * Parses an MCP server URL into its normalized origin, or `undefined` when it
 * isn't an acceptable target. Accepts any https origin, or http on the
 * 127.0.0.1 loopback (a server the user runs themselves) — mirroring the `/mcp`
 * `form-action` CSP. `localhost` is excluded: it can resolve off-loopback, and
 * CSP's host-source grammar can't pin it to the loopback.
 */
export const parseMcpServerUrl = (raw: string): McpServer | undefined => {
  let url: URL;
  try {
    url = new URL(raw.trim());
  } catch {
    return undefined;
  }
  const isHttps = url.protocol === "https:";
  const isHttpLoopback =
    url.protocol === "http:" && url.hostname === "127.0.0.1";
  if (!isHttps && !isHttpLoopback) {
    return undefined;
  }
  return { origin: url.origin, host: url.host };
};

/**
 * Best-effort reachability probe. A `no-cors` fetch can't read the response
 * (opaque), but resolving without throwing means the origin resolved and
 * answered at the network level. Returns `false` on any network error. This is
 * advisory only — callers surface a soft warning, never a hard block, so a
 * server that blocks cross-origin probes (or a momentary blip) doesn't prevent
 * the user from adding a server they know is correct.
 */
export const checkMcpServerReachable = async (
  url: string,
): Promise<boolean> => {
  try {
    await fetch(url, { method: "GET", mode: "no-cors", redirect: "follow" });
    return true;
  } catch {
    return false;
  }
};
