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
 *
 * Responsibility of the declaring server: a declared callback must not itself
 * be an open redirect. A response is delivered to the callback in the URL
 * fragment, and a `3xx` whose `Location` carries no fragment re-attaches the
 * original fragment to the redirect target — so a callback that forwards onward
 * would leak the delivered delegation (scoped to the callback's own origin, and
 * therefore usable to impersonate the user *at that origin*) to wherever it
 * forwards. II validates only that the callback is declared, never where it
 * ultimately lands, so the declaring origin must ensure its callbacks terminate
 * locally — the same caveat OAuth's `redirect_uri` has always carried.
 */

/** The fixed well-known path where a server declares the auth callbacks it
 *  accepts. Same origin only — different origins are separate trusted-server
 *  entries, each with its own file. */
export const AUTH_CALLBACKS_PATH = "/.well-known/ii-auth-callbacks";

/** Upper bound on the allow-list document size, in bytes. Generous for a
 *  list of URLs; anything larger is a misbehaving server, not a bigger list. */
export const AUTH_CALLBACKS_MAX_SIZE = 8 * 1024;

/** Reads `response`'s body as text with {@link AUTH_CALLBACKS_MAX_SIZE} as a
 *  hard cap: rejected up front when the server declares an oversize
 *  `Content-Length`, and enforced chunk-by-chunk while streaming otherwise —
 *  the read stops (and the connection is cancelled) at the cap, rather than
 *  buffering an arbitrarily large body first and measuring it afterwards. */
const readCapped = async (response: Response): Promise<string> => {
  const tooLarge = (): Error =>
    new Error("The MCP server's callback allow-list is too large.");

  const declaredLength = response.headers.get("content-length");
  if (
    declaredLength !== null &&
    Number(declaredLength) > AUTH_CALLBACKS_MAX_SIZE
  ) {
    throw tooLarge();
  }

  if (response.body === null) {
    // No streamable body (older environments): read, then enforce the cap.
    const text = await response.text();
    if (text.length > AUTH_CALLBACKS_MAX_SIZE) {
      throw tooLarge();
    }
    return text;
  }

  const reader = response.body.getReader();
  const decoder = new TextDecoder();
  let received = 0;
  let text = "";
  for (;;) {
    const { done, value } = await reader.read();
    if (done) {
      break;
    }
    received += value.byteLength;
    if (received > AUTH_CALLBACKS_MAX_SIZE) {
      // Stop pulling from the network instead of buffering the rest.
      await reader.cancel().catch(() => undefined);
      throw tooLarge();
    }
    text += decoder.decode(value, { stream: true });
  }
  return text + decoder.decode();
};

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
  const text = await readCapped(response);
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
  // Validate the matched entry itself — never just the fact of a match.
  return assertDeliverableCallback(origin, requestedCallback);
};

/**
 * The checks every callback must pass before anything is delivered to it,
 * whatever established that it is the right one: it must parse, sit on the
 * trust-confirmed `origin`, and carry no fragment of its own.
 *
 * Separate from the allow-list so both connect paths share it — a remote
 * server's callback reaches this after matching its declared entry, a local
 * server's (which has no allow-list to match) reaches it directly. Callers
 * generally derive `origin` from the callback itself, so the same-origin check
 * is usually a tautology; keeping it here means the invariant holds locally
 * even if a caller ever stops doing that. The fragment check is not a
 * tautology: the connect flow appends its own fragment, and a callback that
 * already carried one would produce `...#theirs#delegation=...`, mangling the
 * delivery. Nothing legitimate needs one.
 *
 * Returns the callback unchanged, so it reads as the value that passed.
 */
export const assertDeliverableCallback = (
  origin: string,
  callback: string,
): string => {
  let parsed: URL;
  try {
    parsed = new URL(callback);
  } catch {
    throw new Error("The callback is not a valid URL.");
  }
  if (parsed.origin !== origin) {
    throw new Error("The callback is not on the trusted server's origin.");
  }
  if (parsed.hash !== "") {
    throw new Error("The callback must not carry a fragment.");
  }
  return callback;
};
