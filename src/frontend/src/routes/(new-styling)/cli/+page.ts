import type { PageLoad } from "./$types";

/**
 * Derivation origin to use for generic (non-dapp) CLI sign-in. Kept stable
 * across the migration from the standalone `cli.id.ai` site to the built-in
 * `id.ai/cli` route so existing CLI principals stay valid.
 */
export const CLI_GENERIC_DERIVATION_ORIGIN = "https://cli.id.ai";

/** Default delegation lifetime in minutes — matches the previous cli.id.ai. */
const DEFAULT_TTL_MINUTES = 480;

export type CliParams =
  | {
      kind: "valid";
      publicKey: string;
      callback: string;
      ttlMinutes: number;
      /** Hostname of the dapp the CLI is being authorized for, or undefined
       *  for generic mode. */
      appHost?: string;
    }
  | { kind: "invalid"; reason: string };

const isLoopbackCallback = (raw: string): boolean => {
  let url: URL;
  try {
    url = new URL(raw);
  } catch {
    return false;
  }
  if (url.protocol !== "http:" && url.protocol !== "https:") {
    return false;
  }
  if (url.username !== "" || url.password !== "") {
    return false;
  }
  return url.hostname === "127.0.0.1" || url.hostname === "::1";
};

const isValidPublicKey = (raw: string): boolean => /^[0-9a-fA-F]+$/.test(raw);

const isValidAppHost = (raw: string): boolean => {
  // Allow standard hostnames; reject anything with whitespace, slashes,
  // or characters that wouldn't be a legal URL host.
  return /^[a-zA-Z0-9.-]+$/.test(raw) && raw.length > 0 && raw.length <= 253;
};

export const load: PageLoad = ({ url }): { params: CliParams } => {
  const publicKey = url.searchParams.get("public_key");
  const callback = url.searchParams.get("callback");
  const app = url.searchParams.get("app");
  const ttlRaw = url.searchParams.get("ttl");

  if (publicKey === null || publicKey === "") {
    return { params: { kind: "invalid", reason: "missing-public-key" } };
  }
  if (!isValidPublicKey(publicKey)) {
    return { params: { kind: "invalid", reason: "invalid-public-key" } };
  }
  if (callback === null || callback === "") {
    return { params: { kind: "invalid", reason: "missing-callback" } };
  }
  if (!isLoopbackCallback(callback)) {
    return { params: { kind: "invalid", reason: "invalid-callback" } };
  }

  let ttlMinutes = DEFAULT_TTL_MINUTES;
  if (ttlRaw !== null) {
    const parsed = Number(ttlRaw);
    if (!Number.isFinite(parsed) || parsed <= 0 || parsed > 7 * 24 * 60) {
      return { params: { kind: "invalid", reason: "invalid-ttl" } };
    }
    ttlMinutes = Math.floor(parsed);
  }

  let appHost: string | undefined;
  if (app !== null && app !== "") {
    if (!isValidAppHost(app)) {
      return { params: { kind: "invalid", reason: "invalid-app" } };
    }
    appHost = app.toLowerCase();
  }

  return {
    params: { kind: "valid", publicKey, callback, ttlMinutes, appHost },
  };
};
