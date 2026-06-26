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
 *       mcp-authorize--success       (MCP server redirected back status=success)
 *       mcp-authorize--error         (MCP server redirected back status=error)
 * end-mcp-authorize
 *
 * The flow navigates away to the MCP server and back, so the success and error
 * events fire on a fresh page load — Plausible correlates them with the start
 * by visitor session, not by JS lifetime.
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
