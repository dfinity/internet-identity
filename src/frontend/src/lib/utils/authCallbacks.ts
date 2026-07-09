/**
 * Server-declared auth callbacks: the `/.well-known/ii-auth-callbacks`
 * allow-list.
 *
 * II is the sign-in provider and the server is the party being returned to —
 * so the *server* declares where it can be returned to, by hosting a JSON
 * document at a fixed well-known path on its origin:
 *
 *     { "callbacks": ["https://server.example/mcp/connect", ...] }
 *
 * A connect link's callback is honoured only when it exact-matches one of the
 * declared entries: the (attacker-craftable) link only ever *selects* among
 * the server-declared set, it never chooses the destination on its own. The
 * name is deliberately not MCP-specific — this is a general auth-callback
 * allow-list other II flows can reuse.
 *
 * The indirection is only as safe as its validation, so the fetch and the
 * match are deliberately strict, and every failure fails the flow (closed):
 *  - the fetch refuses redirects — an open redirect at the well-known path
 *    must not let a third party serve the list — carries no ambient
 *    credentials, and is never cached (`no-store`: the match is always
 *    against the server's current declaration);
 *  - the response must be `application/json` and under a size cap;
 *  - the matched entry must be same-origin with the (already trusted) origin
 *    — a declared cross-origin callback is rejected rather than honoured, so
 *    nothing ever ships off-origin — and must not carry a fragment (the
 *    connect flow appends its own).
 *
 * The server must serve the document with CORS headers that let II read it
 * (`Access-Control-Allow-Origin`), like any cross-origin JSON resource.
 */

/** The fixed well-known path where a server declares the auth callbacks it
 *  accepts. Same origin only — different origins are separate trusted-server
 *  entries, each with its own file. */
export const AUTH_CALLBACKS_PATH = "/.well-known/ii-auth-callbacks";

/** Upper bound on the allow-list document size, in UTF-16 code units
 *  (~bytes for the ASCII documents these are). Generous for a list of URLs;
 *  anything larger is a misbehaving server, not a bigger list. */
export const AUTH_CALLBACKS_MAX_SIZE = 8 * 1024;

/**
 * Fetches `origin`'s declared auth-callback allow-list and exact-matches
 * `requestedCallback` against it. Resolves with the matched callback
 * (identical to `requestedCallback`) or throws — any failure (unreachable or
 * redirecting list, wrong content-type, oversized or malformed document, no
 * match, cross-origin or fragment-carrying entry) fails the caller's flow
 * before anything is minted or delivered.
 *
 * `origin` must already be trusted by the caller (for the MCP connect flow:
 * verified against the identity's synced trusted-server config); this
 * function only answers "does that origin accept this callback?".
 */
export const matchDeclaredCallback = async (
  origin: string,
  requestedCallback: string,
): Promise<string> => {
  const response = await fetch(`${origin}${AUTH_CALLBACKS_PATH}`, {
    // An open redirect at the well-known path must not let a third party
    // serve the allow-list: any redirect is a hard failure, never followed.
    redirect: "error",
    // No ambient credentials on this cross-origin GET, and no cache — the
    // match is always against the server's current declaration.
    credentials: "omit",
    cache: "no-store",
  });
  if (!response.ok) {
    throw new Error(
      `The MCP server's callback allow-list could not be fetched (HTTP ${response.status}).`,
    );
  }
  const contentType = response.headers.get("content-type") ?? "";
  if (!contentType.toLowerCase().startsWith("application/json")) {
    throw new Error("The MCP server's callback allow-list is not JSON.");
  }
  const text = await response.text();
  if (text.length > AUTH_CALLBACKS_MAX_SIZE) {
    throw new Error("The MCP server's callback allow-list is too large.");
  }
  let body: unknown;
  try {
    body = JSON.parse(text);
  } catch {
    throw new Error("The MCP server's callback allow-list is not valid JSON.");
  }
  if (
    typeof body !== "object" ||
    body === null ||
    !("callbacks" in body) ||
    !Array.isArray(body.callbacks)
  ) {
    throw new Error(
      "The MCP server's callback allow-list is missing `callbacks`.",
    );
  }
  // Exact match (string equality — no normalization, so there is nothing to
  // confuse): the link's callback only selects among the declared entries.
  if (!body.callbacks.includes(requestedCallback)) {
    throw new Error(
      "The MCP server does not declare this callback in its allow-list.",
    );
  }
  // Validate the matched entry itself — never just the fact of a match. The
  // caller only asks about callbacks on the trusted origin, but re-verifying
  // here keeps the invariant local: a declared cross-origin callback is
  // rejected rather than honoured, whatever the caller did.
  let matched: URL;
  try {
    matched = new URL(requestedCallback);
  } catch {
    throw new Error("The declared callback is not a valid URL.");
  }
  if (matched.origin !== origin) {
    throw new Error("The declared callback is not on the server's origin.");
  }
  if (matched.hash !== "") {
    // The connect flow appends its own fragment; a declared fragment could
    // clobber or be clobbered by it. Nothing legitimate needs one.
    throw new Error("The declared callback must not carry a fragment.");
  }
  return requestedCallback;
};
