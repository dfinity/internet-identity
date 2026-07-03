import type { Authenticated } from "$lib/stores/authentication.store";
import { fromBase64URL } from "$lib/utils/utils";

interface McpAuthorizeInput {
  authenticated: Authenticated;
  /** Lifetime in seconds for the session grant (already clamped to
   *  [10 min, 30 days]; the backend clamps again). */
  ttlSeconds: number;
  /** Callback URL on the MCP server origin (e.g. "https://mcp.id.ai/callback").
   *  MCP connections are to remote servers, so this is always https, and its
   *  origin has been verified against the identity's synced trusted-server
   *  config before this runs. The connect flow talks to it twice: once to
   *  fetch the server's session public key, once to report completion. */
  callback: string;
  /** Opaque value the server issued for this connect, included in both
   *  callback requests so the server can tie them to the request it started. */
  state: string;
}

/** The key-request response: the server's session public key for this
 *  connection, DER-encoded and base64url. */
const parsePublicKey = (body: unknown): Uint8Array => {
  if (
    typeof body !== "object" ||
    body === null ||
    !("public_key" in body) ||
    typeof body.public_key !== "string"
  ) {
    throw new Error("The MCP server's key response is missing `public_key`.");
  }
  return fromBase64URL(body.public_key);
};

/**
 * Connects the MCP server by registering its session key with the backend:
 * fetch the server's session public key from its callback, register it via
 * `mcp_register` (binding the key's principal to the identity with the
 * user-chosen expiry), and report completion back to the callback. No
 * delegation is minted for the server itself — its capability is the grant,
 * which the backend checks on every `mcp_*` call and which Settings revokes
 * via `mcp_set_config`.
 *
 * The key is fetched over an origin-attested channel: this code only ever
 * contacts the *trusted* origin's callback (verified against the synced
 * config by the caller), and the server only answers for a `state` it issued.
 * Nothing from the unauthenticated connect link is ever registered — an
 * attacker-crafted link yields no key (unknown state) and so registers
 * nothing, preserving the property that a session only materializes with the
 * trusted server's cooperation.
 *
 * A resolved promise means the session is registered and the server was (best
 * effort) notified; unlike the old delegation form-POST flow, the page stays
 * put and shows the terminal screen itself.
 */
export const mcpAuthorize = async ({
  authenticated,
  ttlSeconds,
  callback,
  state,
}: McpAuthorizeInput): Promise<void> => {
  const { identityNumber, actor } = authenticated;

  // Ask the trusted server for its session public key for this connect. A
  // non-2xx answer (e.g. a state it never issued) aborts before anything is
  // registered.
  const keyResponse = await fetch(callback, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ state }),
  });
  if (!keyResponse.ok) {
    throw new Error(
      `The MCP server rejected the connect request (HTTP ${keyResponse.status}).`,
    );
  }
  const sessionKey = parsePublicKey(await keyResponse.json());

  // One authenticated call registers the session: the backend binds the key's
  // self-authenticating principal to the identity (replacing any previous
  // session — at most one at a time) until the chosen expiry.
  const grantTtlNanos = BigInt(ttlSeconds) * BigInt(1e9);
  const result = await actor.mcp_register(
    identityNumber,
    sessionKey,
    grantTtlNanos,
  );
  if ("Err" in result) {
    throw new Error(result.Err);
  }
  const { expiration } = result.Ok;

  // Tell the server its session is live and when it expires (ns since epoch,
  // as a string — the value overflows JSON numbers). Best effort: on failure
  // the server discovers success on its first signed call.
  try {
    await fetch(callback, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ state, expiration: expiration.toString() }),
    });
  } catch {
    // Deliberately ignored; see above.
  }
};
