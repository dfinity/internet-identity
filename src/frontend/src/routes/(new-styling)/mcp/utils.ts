import {
  toPermissionsArg,
  toPermissionsString,
  type AccessLevel,
} from "$lib/utils/accessLevel";
import type { Authenticated } from "$lib/stores/authentication.store";
import { fromBase64URL } from "$lib/utils/utils";

interface McpAuthorizeInput {
  authenticated: Authenticated;
  /** Lifetime in seconds for the session grant (already clamped to
   *  [10 min, 30 days]; the backend clamps again). */
  ttlSeconds: number;
  /** Whether the whole session is read-only: when read-only, every per-app
   *  delegation the server later mints is restricted to query calls. Chosen
   *  once at connect and passed to `mcp_register`, which persists it on the
   *  grant — read-only is a property of the session, not a per-call flag. */
  accessLevel: AccessLevel;
  /** The trusted server's pinned connect endpoint: its origin plus a fixed
   *  well-known path, derived by the caller from the identity's synced
   *  trusted-server config (see `connectCallbackUrl`) — never taken from the
   *  (attacker-craftable) connect link, whose callback path is dropped. Always
   *  https (the configured URL is). The connect flow talks to it twice: once to
   *  fetch the server's session public key, once to report completion. */
  callback: string;
  /** Opaque value the server issued for this connect, included in both
   *  callback requests so the server can tie them to the request it started. */
  state: string;
}

/** The key-request response: the server's session public key for this
 *  connection (DER-encoded, base64url), plus an optional `finish_url` the
 *  browser should be sent to once the session is registered — this is how a
 *  server completes a flow of its own around the connect (e.g. minting the
 *  OAuth authorization code for an MCP client and redirecting back to it).
 *
 *  `finish_url` is only honoured on the *callback's* origin — the trusted
 *  server the user consented to, per their synced config. It arrives over the
 *  same origin-attested channel as the key (the callback response, never the
 *  connect link), and the same-origin requirement keeps the invariant that II
 *  only ever navigates to the trusted origin; where the server redirects from
 *  there is its own (standard OAuth) responsibility. Origin equality also
 *  rules out non-https schemes (`javascript:` etc. have no matching origin).
 *  A `finish_url` that fails these checks fails the connect — before anything
 *  is registered — rather than silently completing without the redirect. */
const parseKeyResponse = (
  body: unknown,
  callback: string,
): { sessionKey: Uint8Array; finishUrl: string | undefined } => {
  if (
    typeof body !== "object" ||
    body === null ||
    !("public_key" in body) ||
    typeof body.public_key !== "string"
  ) {
    throw new Error("The MCP server's key response is missing `public_key`.");
  }
  const sessionKey = fromBase64URL(body.public_key);
  // `null` and `""` count as absent, not as errors: they're how common server
  // stacks serialize an omitted optional (serde `Option::None`, a Go string
  // zero-value, Python `None`), and "no redirect" is the only thing an absent
  // value can mean — there's nothing to navigate to, so nothing to fail.
  if (
    !("finish_url" in body) ||
    body.finish_url === undefined ||
    body.finish_url === null ||
    body.finish_url === ""
  ) {
    return { sessionKey, finishUrl: undefined };
  }
  if (typeof body.finish_url !== "string") {
    throw new Error("The MCP server's `finish_url` is not a string.");
  }
  let finishUrl;
  try {
    finishUrl = new URL(body.finish_url);
  } catch {
    throw new Error("The MCP server's `finish_url` is not a valid URL.");
  }
  if (finishUrl.origin !== new URL(callback).origin) {
    throw new Error(
      "The MCP server's `finish_url` is not on the server's own origin.",
    );
  }
  return { sessionKey, finishUrl: finishUrl.href };
};

/**
 * Connects the MCP server by registering its session key with the backend:
 * fetch the server's session public key from its callback, register it via
 * `mcp_register` (binding the key's principal to the identity with the
 * user-chosen expiry and access level), and report completion back to the
 * callback. No delegation is minted for the server itself — its capability is
 * the grant, which the backend checks on every `mcp_*` call and which Settings
 * revokes via `mcp_set_config`. A read-only `accessLevel` makes every per-app
 * delegation the session later mints query-only.
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
 * effort) notified. Resolves with the server's validated `finish_url` when it
 * supplied one in the key response (the caller then navigates the tab there,
 * letting the server finish a flow of its own — e.g. hand an OAuth code back
 * to an MCP client), and with `undefined` otherwise (the page stays put and
 * shows the terminal screen itself).
 */
export const mcpAuthorize = async ({
  authenticated,
  ttlSeconds,
  accessLevel,
  callback,
  state,
}: McpAuthorizeInput): Promise<string | undefined> => {
  const { identityNumber, actor } = authenticated;

  // Ask the trusted server for its session public key for this connect. A
  // non-2xx answer (e.g. a state it never issued) aborts before anything is
  // registered. `redirect: "error"` makes a 307/308 a hard failure rather than
  // silently re-POSTing the single-use `state` to a redirected origin; `omit`
  // keeps ambient credentials off this cross-origin call (mirrors the
  // fail-closed fetches elsewhere, e.g. validateDerivationOrigin).
  const keyResponse = await fetch(callback, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ state }),
    redirect: "error",
    credentials: "omit",
  });
  if (!keyResponse.ok) {
    throw new Error(
      `The MCP server rejected the connect request (HTTP ${keyResponse.status}).`,
    );
  }
  // Parsing also validates any `finish_url` (same-origin with the trusted
  // callback), so a misbehaving response aborts here — nothing registered.
  const { sessionKey, finishUrl } = parseKeyResponse(
    await keyResponse.json(),
    callback,
  );

  // One authenticated call registers the session: the backend binds the key's
  // self-authenticating principal to the identity (replacing any previous
  // session — at most one at a time) until the chosen expiry, and persists the
  // access level so every per-app delegation this session mints inherits it.
  const grantTtlNanos = BigInt(ttlSeconds) * BigInt(1e9);
  const result = await actor.mcp_register(
    identityNumber,
    sessionKey,
    grantTtlNanos,
    toPermissionsArg(accessLevel),
  );
  if ("Err" in result) {
    throw new Error(result.Err);
  }
  const { expiration } = result.Ok;

  // Tell the server its session is live: when it expires (ns since epoch, as a
  // string — the value overflows JSON numbers) and the access level the user
  // chose (`permissions`, "queries" | "all"), so it learns read-only up front
  // instead of inferring it from a minted delegation. Best effort: on failure
  // the server discovers success on its first signed call, and can still read
  // the access level off any delegation's `permissions`. Sent (and awaited)
  // before any `finish_url` navigation, so the server normally hears about the
  // registration before the browser arrives — but a server must not rely on
  // that ordering, since this notification can fail while the navigation still
  // happens.
  try {
    await fetch(callback, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({
        state,
        expiration: expiration.toString(),
        permissions: toPermissionsString(accessLevel),
      }),
      // Same fail-closed posture as the key request: never let a redirect
      // forward the completion payload to another origin.
      redirect: "error",
      credentials: "omit",
    });
  } catch {
    // Deliberately ignored; see above.
  }

  return finishUrl;
};
