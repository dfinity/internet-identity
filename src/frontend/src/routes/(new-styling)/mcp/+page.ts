import type { PageLoad } from "./$types";
import { fromBase64URL } from "$lib/utils/utils";

/** Default delegation lifetime in seconds when the request omits `ttl`. */
const DEFAULT_TTL_SECONDS = 60 * 60;
/** Largest TTL the request can ask for: the backend caps a delegation at 30 days
 *  (`MAX_EXPIRATION_PERIOD_NS`), so anything larger is clamped to this. */
const MAX_TTL_SECONDS = 30 * 24 * 60 * 60;

/**
 * The `/mcp` request, parsed from the URL fragment the MCP server redirects the
 * browser to. `valid` carries the validated request — the session public key to
 * delegate to, the callback to post the delegation back to, the single-use
 * `state` echoed back to the MCP server, and the delegation TTL (`ttl`, in
 * seconds). The MCP server the user connects is identified by the callback's
 * origin (each user trusts whichever server they connect); no account is chosen
 * here (it's per-app, picked server-side at delegation time). `invalid` means
 * the fragment was missing or malformed.
 *
 * Whether the callback origin is one the connect flow accepts (https only — MCP
 * connections are to remote servers) is checked in the page component, which
 * shows a clean invalid screen and mirrors the `form-action` CSP.
 */
export type McpParams =
  | {
      kind: "valid";
      /** base64url-encoded DER session pubkey supplied by the MCP server. */
      publicKey: string;
      callback: string;
      /** Opaque value echoed back to the MCP server so it can tie the delivered
       *  delegation to the request it started (CSRF protection). */
      state: string;
      /** Requested delegation lifetime in seconds (clamped to the 30-day cap). */
      ttlSeconds: number;
    }
  | { kind: "invalid" };

/**
 * Outcome the MCP server redirects back with after receiving the delegation.
 * `success` and `error` arrive on their own on a fresh page load.
 */
export type McpStatus = "success" | "error";

const parseStatus = (raw: string | null): McpStatus | undefined => {
  if (raw === "success" || raw === "error") {
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
 * Structural callback check: must be an absolute http(s) URL. The stricter
 * "is this an origin the connect flow accepts" check (https only) happens in
 * the component, alongside the consent UI — keeping this `load` minimal since
 * it also runs at prerender.
 */
const parseCallback = (raw: string | null): string | undefined => {
  if (raw === null || raw === "") {
    return undefined;
  }
  let url: URL;
  try {
    url = new URL(raw);
  } catch {
    return undefined;
  }
  if (url.protocol !== "https:" && url.protocol !== "http:") {
    return undefined;
  }
  return raw;
};

const parseState = (raw: string | null): string | undefined => {
  if (raw === null || raw === "") {
    return undefined;
  }
  return raw;
};

// `ttl` is a lifetime in seconds. Any positive value is accepted and clamped to
// the allowed range (the backend caps at 30 days regardless); an omitted `ttl`
// uses the default, and a malformed one (non-numeric or <= 0) invalidates the
// request.
const parseTtl = (raw: string | null): number | undefined => {
  if (raw === null) {
    return DEFAULT_TTL_SECONDS;
  }
  const parsed = Number(raw);
  if (!Number.isFinite(parsed) || parsed <= 0) {
    return undefined;
  }
  return Math.min(Math.floor(parsed), MAX_TTL_SECONDS);
};

export const load: PageLoad = ({
  url,
}): { params: McpParams; status: McpStatus | undefined } => {
  // The MCP server redirects the browser here with the request in the URL
  // fragment (never sent to the server, so the session key and callback stay
  // out of II's request logs; the address-bar copy is cleared in `+page.svelte`
  // via `replaceState`). Reading `url.hash` relies on this universal `load`
  // re-running client-side; with `adapter-static` it's empty during prerender.
  const params = new URLSearchParams(url.hash.slice(1));

  const status = parseStatus(params.get("status"));
  const publicKey = parseBase64Url(params.get("public_key"));
  const callback = parseCallback(params.get("callback"));
  const state = parseState(params.get("state"));
  const ttlSeconds = parseTtl(params.get("ttl"));

  if (
    publicKey === undefined ||
    callback === undefined ||
    state === undefined ||
    ttlSeconds === undefined
  ) {
    return { params: { kind: "invalid" }, status };
  }
  return {
    params: { kind: "valid", publicKey, callback, state, ttlSeconds },
    status,
  };
};
