import type { PageLoad } from "./$types";
import { fromHex } from "$lib/utils/utils";

/** Default delegation lifetime in minutes. */
const DEFAULT_TTL_MINUTES = 480;

export type CliParams =
  | {
      kind: "valid";
      publicKey: string;
      callback: string;
      ttlMinutes: number;
      /** Hostname of the app the CLI is being authorized for, or undefined
       *  for generic mode. */
      appHost: string | undefined;
    }
  | { kind: "invalid" };

/** Returns the input unchanged if it hex-decodes, undefined otherwise. */
const parseHex = (raw: string | null): string | undefined => {
  if (raw === null || raw === "") {
    return undefined;
  }
  try {
    fromHex(raw);
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

export const load: PageLoad = ({ url }): { params: CliParams } => {
  const publicKey = parseHex(url.searchParams.get("public_key"));
  const callback = parseLoopbackCallback(url.searchParams.get("callback"));
  const ttlMinutes = parseTtl(url.searchParams.get("ttl"));

  // `app` is optional. Absent or empty → generic mode. Present → must parse.
  const appRaw = url.searchParams.get("app");
  let appHost: string | undefined;
  if (appRaw !== null && appRaw !== "") {
    appHost = parseAppHost(appRaw);
    if (appHost === undefined) {
      return { params: { kind: "invalid" } };
    }
  }

  if (
    publicKey === undefined ||
    callback === undefined ||
    ttlMinutes === undefined
  ) {
    return { params: { kind: "invalid" } };
  }
  return {
    params: { kind: "valid", publicKey, callback, ttlMinutes, appHost },
  };
};
