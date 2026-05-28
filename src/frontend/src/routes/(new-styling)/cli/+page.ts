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
  // CLI binaries put `public_key`, `callback`, etc. in the URL fragment so
  // they don't appear in browser history or server logs. The fragment is
  // only available client-side, so this `load` only resolves meaningfully
  // in the browser (`adapter-static` is fine — `load` runs on the client at
  // navigation time).
  const fragment = url.hash.startsWith("#") ? url.hash.slice(1) : url.hash;
  const params = new URLSearchParams(fragment);

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
