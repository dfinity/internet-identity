import type { PageLoad } from "./$types";
import { fromBase64URL } from "$lib/utils/utils";

/** Default delegation lifetime in minutes. */
const DEFAULT_TTL_MINUTES = 480;

export type CliParams =
  | {
      kind: "valid";
      /** base64url-encoded DER session pubkey supplied by the CLI. */
      publicKey: string;
      callback: string;
      ttlMinutes: number;
      /** Hostname of the app the CLI is being authorized for, or undefined
       *  for generic mode. */
      appHost: string | undefined;
    }
  | { kind: "invalid" };

/**
 * Outcome the loopback server redirects back with after receiving the
 * delegation. `success` and `error` arrive on their own; `identity-mismatch`
 * arrives alongside `public_key`/`callback` so the authorize screen can be
 * re-shown for an in-place retry.
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
  if (url.protocol !== "http:" && url.protocol !== "https:") {
    return undefined;
  }
  if (url.hostname !== "127.0.0.1" && url.hostname !== "::1") {
    return undefined;
  }
  return raw;
};

/**
 * Returns the normalised hostname if `raw` is a bare hostname (optionally
 * with mixed case), or undefined if it's not. Rejects port, path, query,
 * fragment, scheme prefix, and userinfo by requiring the round-trip
 * through `new URL` to leave only the hostname behind.
 */
const parseAppHost = (raw: string): string | undefined => {
  let url: URL;
  try {
    url = new URL(`https://${raw}`);
  } catch {
    return undefined;
  }
  if (url.hostname.toLowerCase() !== raw.toLowerCase()) {
    return undefined;
  }
  return url.hostname;
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
  const ttlMinutes = parseTtl(params.get("ttl"));

  // `app` is optional. Absent or empty → generic mode. Present → must parse.
  const appRaw = params.get("app");
  let appHost: string | undefined;
  if (appRaw !== null && appRaw !== "") {
    appHost = parseAppHost(appRaw);
    if (appHost === undefined) {
      return { params: { kind: "invalid" }, status };
    }
  }

  if (
    publicKey === undefined ||
    callback === undefined ||
    ttlMinutes === undefined
  ) {
    return { params: { kind: "invalid" }, status };
  }
  return {
    params: { kind: "valid", publicKey, callback, ttlMinutes, appHost },
    status,
  };
};
