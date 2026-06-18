import type { PageLoad } from "./$types";
import { fromBase64URL } from "$lib/utils/utils";

/** Default delegation lifetime in minutes when the request omits `ttl`. */
const DEFAULT_TTL_MINUTES = 60;

/**
 * The `/mcp` request, parsed from the URL fragment the MCP server redirects the
 * browser to. `valid` carries the validated request — the session public key to
 * delegate to, the callback to post the delegation back to, the single-use
 * `state` echoed back to the MCP server, and the delegation TTL. The account the
 * delegation acts as is the user's default account at the configured MCP server
 * origin (II config), not a request parameter. `invalid` means the fragment was
 * missing or malformed.
 *
 * The callback's origin is checked against the configured MCP server origin in
 * the page component (where the canister config is available), not here.
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
      ttlMinutes: number;
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
 * Structural callback check: must be an absolute http(s) URL. The exact origin
 * match against the configured MCP server origin happens in the component — the
 * canister config isn't available here (this `load` also runs at prerender).
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
  const ttlMinutes = parseTtl(params.get("ttl"));

  if (
    publicKey === undefined ||
    callback === undefined ||
    state === undefined ||
    ttlMinutes === undefined
  ) {
    return { params: { kind: "invalid" }, status };
  }
  return {
    params: { kind: "valid", publicKey, callback, state, ttlMinutes },
    status,
  };
};
