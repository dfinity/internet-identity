import { Funnel } from "./Funnel";

/**
 * /mcp authorize flow events (the funnel prefixes each with `mcp-authorize--`):
 *
 * start-mcp-authorize (INIT)
 *   mcp-authorize--request-invalid   (fragment missing/malformed, or callback
 *                                      not an acceptable MCP server URL)
 *   mcp-authorize--request-received  (valid request, connect screen shown)
 *     mcp-authorize--server-untrusted (callback origin not on the chosen
 *                                      identity's trusted-server list)
 *     mcp-authorize--confirmed       (user clicked Allow access)
 *       mcp-authorize--success       (session registered; terminal screen shown)
 *       mcp-authorize--error         (key fetch or registration failed)
 * end-mcp-authorize
 *
 * The whole flow runs on this page (fetch the server's key, register it,
 * notify the server), so success and error fire in the same JS lifetime as
 * the start — no cross-load correlation needed.
 */
export const McpAuthorizeEvents = {
  RequestInvalid: "request-invalid",
  RequestReceived: "request-received",
  ServerUntrusted: "server-untrusted",
  Confirmed: "confirmed",
  Success: "success",
  Error: "error",
} as const;

export const mcpAuthorizeFunnel = new Funnel<typeof McpAuthorizeEvents>(
  "mcp-authorize",
  true,
);
