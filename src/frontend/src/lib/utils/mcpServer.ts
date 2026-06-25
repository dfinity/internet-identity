/**
 * Helpers for the MCP server URL a user trusts: parsing/normalizing it and a
 * best-effort probe that it actually speaks MCP. Shared by the Settings UI
 * (where the user enters the URL) and the `/mcp` connect flow (which matches the
 * request's callback origin against the trusted server).
 */

export interface McpServer {
  /** The full normalized endpoint URL the user entered (kept so we can probe a
   *  path-based endpoint like `https://host/mcp`, not just the origin root). */
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
  let url: URL;
  try {
    url = new URL(raw.trim());
  } catch {
    return undefined;
  }
  if (url.protocol !== "https:") {
    return undefined;
  }
  return { url: url.href, origin: url.origin, host: url.host };
};

/** MCP protocol revision we advertise in the probe's `initialize` request. */
const MCP_PROTOCOL_VERSION = "2025-06-18";
const PROBE_TIMEOUT_MS = 5_000;

/**
 * Probes whether `url` actually speaks MCP — not merely that "something loads".
 *
 * Sends the MCP `initialize` JSON-RPC request over the Streamable HTTP
 * transport and resolves `true` only when the endpoint answers with a JSON-RPC
 * `initialize` *result* (carrying `protocolVersion` / `serverInfo` /
 * `capabilities`), read from either a JSON body or the first Server-Sent-Events
 * `data:` frame. Resolves `false` on a network error, a timeout, a response we
 * can't read (CORS), or any non-MCP answer.
 *
 * Inherently best-effort: a server that doesn't allow our origin via CORS can't
 * be verified from the browser, so callers treat `false` as "couldn't verify"
 * (surface a warning + offer a re-check), never as a hard block.
 */
export const probeMcpServer = async (url: string): Promise<boolean> => {
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
    return isInitializeResult(await response.text());
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
 * Whether `body` contains a JSON-RPC `initialize` result. Tolerates both a
 * plain JSON response and an SSE stream (the two Streamable-HTTP shapes).
 */
const isInitializeResult = (body: string): boolean => {
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
    const { jsonrpc, result } = message as Record<string, unknown>;
    return (
      jsonrpc === "2.0" &&
      typeof result === "object" &&
      result !== null &&
      ("protocolVersion" in result ||
        "serverInfo" in result ||
        "capabilities" in result)
    );
  });
};
