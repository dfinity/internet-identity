import type { PageLoad } from "./$types";
import { fromBase64URL } from "$lib/utils/utils";

/** Default delegation lifetime in minutes. */
const DEFAULT_TTL_MINUTES = 480;

/**
 * The `/cli` request, parsed from the URL fragment the CLI opens the page with.
 * `valid` carries the validated request — the session public key to delegate
 * to, the loopback callback to post the delegation back to, the single-use
 * nonce, the delegation TTL, and the optional app origin. `invalid`
 * means the fragment was missing or malformed, and the page shows the
 * invalid-request screen.
 */
export type CliParams =
  | {
      kind: "valid";
      /** base64url-encoded DER session pubkey supplied by the CLI. */
      publicKey: string;
      callback: string;
      /** Single-use secret echoed back to the loopback server so it can tell
       *  this page's POST from a stray or forged local request. */
      nonce: string;
      ttlMinutes: number;
      /** Origin of the app to get an identity for, or undefined for generic
       *  mode (the auth page's own default, e.g. cli.id.ai). */
      appOrigin: string | undefined;
    }
  | { kind: "invalid" };

/**
 * Outcome the loopback server redirects back with after receiving the
 * delegation. `success` and `error` arrive on their own; `identity-mismatch`
 * arrives alongside `public_key`/`callback` so the authorize screen can be
 * re-shown for an in-place retry.
 *
 * `identity-mismatch` is the `icp identity login` re-auth case: that command
 * re-authenticates an already-linked identity, which has a fixed principal. If
 * the user signs in with a different II identity than the one originally
 * linked, the resulting delegation resolves to a different principal, so the
 * CLI rejects it and redirects back here — letting the user switch to the
 * correct identity and try again without restarting the flow.
 */
export type CliStatus = "success" | "identity-mismatch" | "error";

const parseStatus = (raw: string | null): CliStatus | undefined => {
  if (raw === "success" || raw === "identity-mismatch" || raw === "error") {
    return raw;
  }
  return undefined;
};

const parseBase64Url = (raw: string | null): string | undefined => {
  if (raw === null || raw === "") {
    return undefined;
  }
  try {
    fromBase64URL(raw);
    return raw;
  } catch {
    return undefined;
  }
};

/**
 * RFC 8252 — the callback must point at a loopback IP literal so an attacker
 * can't intercept the delegation by registering a hostile DNS record.
 */
const parseLoopbackCallback = (raw: string | null): string | undefined => {
  if (raw === null || raw === "") {
    return undefined;
  }
  let url: URL;
  try {
    url = new URL(raw);
  } catch {
    return undefined;
  }
  // Loopback callbacks are http only: RFC 8252 expects http for loopback (the
  // local listener can't realistically present a CA-trusted cert), and the
  // frontend canister's `form-action` CSP only allows http loopback — so an
  // https callback would pass here but silently die on the CSP at submit time.
  if (url.protocol !== "http:") {
    return undefined;
  }
  // Only the IPv4 loopback literal. CSP's host-source grammar can't express
  // IPv6 literals, so `http://[::1]:*` is an invalid `form-action` source the
  // browser ignores — a `::1` callback would pass here but die on the CSP at
  // submit time, the same trap as https above. The CLI must bind 127.0.0.1.
  if (url.hostname !== "127.0.0.1") {
    return undefined;
  }
  return raw;
};

/**
 * Whether `hostname` names this machine. IPv4 loopback only: the whole
 * 127.0.0.0/8, matched as an IP literal rather than by prefix, because
 * `127.example.com` is a registrable name that resolves anywhere. `::1` is
 * deliberately absent — the CLI flow is IPv4-only.
 */
const isLoopbackHostname = (hostname: string): boolean =>
  hostname === "localhost" ||
  hostname.endsWith(".localhost") ||
  /^127\.\d{1,3}\.\d{1,3}\.\d{1,3}$/.test(hostname);

/**
 * Returns the normalised origin of the app named by `--app`, or undefined if
 * `raw` isn't one. A bare hostname is read as `https://<hostname>`; a scheme
 * makes `raw` a full origin, so a local app served over http on a non-default
 * port derives the principal that /authorize derives for the same origin.
 * Anything carrying more than an origin — a path, query, fragment or
 * userinfo — is rejected rather than silently trimmed.
 */
const parseAppOrigin = (raw: string): string | undefined => {
  // Parsing `raw` first and falling back on failure would not work: a bare
  // `oisy.com:443` parses, taking `oisy.com` for the scheme and `443` for the
  // path. So a scheme has to be spotted before parsing, not after.
  let url: URL;
  try {
    url = new URL(raw.includes("://") ? raw : `https://${raw}`);
  } catch {
    return undefined;
  }
  // http would otherwise hand out a principal for an origin any network
  // attacker can impersonate; loopback is the local-development case, where
  // there is no CA-trusted cert to serve the app under.
  const schemeAllowed =
    url.protocol === "https:" ||
    (url.protocol === "http:" && isLoopbackHostname(url.hostname));
  if (!schemeAllowed) {
    return undefined;
  }
  // Checked on the parsed URL rather than against the input string: an
  // explicit default port is a valid part of an origin that `url.origin`
  // canonicalises away, so comparing the two spellings would reject it.
  // A bare origin leaves "/" behind as the path.
  if (
    url.username !== "" ||
    url.password !== "" ||
    url.pathname !== "/" ||
    url.search !== "" ||
    url.hash !== ""
  ) {
    return undefined;
  }
  return url.origin;
};

const parseTtl = (raw: string | null): number | undefined => {
  if (raw === null) {
    return DEFAULT_TTL_MINUTES;
  }
  const parsed = Number(raw);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    return undefined;
  }
  return Math.floor(parsed);
};

export const load: PageLoad = ({
  url,
}): { params: CliParams; status: CliStatus | undefined } => {
  // CLI binaries put `public_key`, `callback`, etc. in the URL fragment
  // because the fragment is never sent to the server — it stays out of server
  // logs (the address-bar copy is then cleared in `+page.svelte`'s onMount via
  // `replaceState`). The fragment is only readable in the browser, so reading
  // `url.hash` here relies on this universal `load` re-running client-side on
  // hydration/navigation; with `adapter-static` it's empty during prerender.
  // Requires SvelteKit 2+ — kit 1 made `url.hash` inaccessible from `load`.
  // `url.hash` is "" or "#…", so slicing one char yields the raw query string.
  const params = new URLSearchParams(url.hash.slice(1));

  const status = parseStatus(params.get("status"));
  const publicKey = parseBase64Url(params.get("public_key"));
  const callback = parseLoopbackCallback(params.get("callback"));
  const nonce = parseBase64Url(params.get("nonce"));
  const ttlMinutes = parseTtl(params.get("ttl"));

  // `domain` is optional. Absent or empty → generic mode. Present → must parse.
  const domainRaw = params.get("domain");
  let appOrigin: string | undefined;
  if (domainRaw !== null && domainRaw !== "") {
    appOrigin = parseAppOrigin(domainRaw);
    if (appOrigin === undefined) {
      return { params: { kind: "invalid" }, status };
    }
  }

  if (
    publicKey === undefined ||
    callback === undefined ||
    nonce === undefined ||
    ttlMinutes === undefined
  ) {
    return { params: { kind: "invalid" }, status };
  }
  return {
    params: {
      kind: "valid",
      publicKey,
      callback,
      nonce,
      ttlMinutes,
      appOrigin,
    },
    status,
  };
};
