/**
 * Permissionless app metadata.
 *
 * Apps that integrate Internet Identity sign-in can provide their own display
 * name, description and logo for the authorize flow by serving a JSON file at
 * `/.well-known/ii-app-metadata` on their origin:
 *
 * ```json
 * {
 *   "name": "Example App",
 *   "description": "A short tagline shown on the sign-in screen",
 *   "logo": "/logo.png"
 * }
 * ```
 *
 * This replaces the curated dapps list previously shipped with Internet
 * Identity as the source of display metadata: any app can publish the file
 * without asking anyone, so the metadata is exactly as trustworthy as the
 * origin serving it. The origin (hostname) therefore remains the trust
 * anchor and is always displayed alongside this metadata.
 *
 * Hardening, since the file is attacker-controlled by construction:
 * - The file is fetched without credentials and redirects are rejected,
 *   mirroring the `/.well-known/ii-alternative-origins` fetch.
 * - Text fields are stripped of control and bidi-override characters and
 *   dropped when they exceed conservative length limits.
 * - The logo must live on the app's own origin. It is downloaded via `fetch`
 *   (II's CSP `connect-src` allows any https origin, unlike `img-src`),
 *   checked against an image content-type allowlist and a size cap, and only
 *   then rendered — as a `data:` URL, which `img-src` already permits. The
 *   logo asset itself is thus never hotlinked.
 * - Every failure mode (missing file, CORS, timeout, invalid JSON, …) yields
 *   `undefined` rather than an error: metadata is a display nicety and must
 *   never break the sign-in flow.
 */

export interface AppMetadata {
  /** Display name of the app. */
  name?: string;
  /** Short description of the app. */
  description?: string;
  /** Ready-to-render `<img src>` value (a `data:` URL for fetched logos). */
  logo?: string;
}

/** Well-known path (relative to the app's origin) the metadata is served on. */
export const APP_METADATA_PATH = "/.well-known/ii-app-metadata";

/** Maximum size of the metadata file (in UTF-16 code units of the body). */
export const MAX_APP_METADATA_SIZE = 8_192;

/** Maximum length of the app name, after whitespace normalization. */
export const MAX_APP_NAME_LENGTH = 40;

/** Maximum length of the app description, after whitespace normalization. */
export const MAX_APP_DESCRIPTION_LENGTH = 120;

/** Maximum size of the logo asset in bytes (256 KiB). */
export const MAX_APP_LOGO_SIZE = 262_144;

/** Content types the logo asset may be served with. */
export const APP_LOGO_CONTENT_TYPES = [
  "image/png",
  "image/jpeg",
  "image/webp",
  "image/gif",
  "image/avif",
  "image/svg+xml",
];

/** Time budget for each fetch; the UI renders a fallback in the meantime, so
 *  a slow origin only delays its own polish, never the sign-in flow. */
const FETCH_TIMEOUT_MILLIS = 10_000;

const fetchOptions = (accept: string): RequestInit => ({
  // fail on redirects
  redirect: "error",
  headers: {
    Accept: accept,
  },
  // do not send cookies or other credentials
  credentials: "omit",
  signal:
    typeof AbortSignal.timeout === "function"
      ? AbortSignal.timeout(FETCH_TIMEOUT_MILLIS)
      : undefined,
});

/**
 * Clean up an app-provided text field: normalize whitespace and strip
 * control characters as well as bidi-override/invisible formatting
 * characters, which could otherwise be used to visually spoof the sign-in
 * screen. Returns `undefined` when the value is not a string, is empty after
 * cleanup, or exceeds `maxLength` (no silent truncation: apps should notice
 * and fix their metadata instead of shipping a clipped name).
 */
const cleanTextField = (
  value: unknown,
  maxLength: number,
): string | undefined => {
  if (typeof value !== "string") {
    return undefined;
  }
  const cleaned = value
    // Strip control and invisible formatting characters, except the ASCII
    // whitespace ones (\t \n \v \f \r), which the next step collapses into
    // regular spaces (stripping them instead would join adjacent words).
    .replace(
      // eslint-disable-next-line no-control-regex
      /[\u0000-\u0008\u000e-\u001f\u007f-\u009f\u200b-\u200f\u202a-\u202e\u2066-\u2069\ufeff]/g,
      "",
    )
    .replace(/\s+/g, " ")
    .trim();
  if (cleaned.length === 0 || cleaned.length > maxLength) {
    return undefined;
  }
  return cleaned;
};

/**
 * Resolve the `logo` field against the app origin and require the result to
 * live on that same origin. Same-origin keeps the sign-in private (no third
 * party learns about it through an image load) and rules out `data:`,
 * `javascript:` and other non-http(s) schemes, whose origin never matches.
 */
const resolveLogoUrl = (value: unknown, origin: string): URL | undefined => {
  if (typeof value !== "string" || value.length === 0) {
    return undefined;
  }
  try {
    const url = new URL(value, origin);
    return url.origin === new URL(origin).origin ? url : undefined;
  } catch {
    return undefined;
  }
};

const toBase64 = (buffer: ArrayBuffer): string => {
  const bytes = new Uint8Array(buffer);
  // Convert in chunks: spreading the whole buffer into one
  // `String.fromCharCode` call can overflow the argument limit.
  const chunkSize = 0x8000;
  let binary = "";
  for (let i = 0; i < bytes.length; i += chunkSize) {
    binary += String.fromCharCode(...bytes.subarray(i, i + chunkSize));
  }
  return btoa(binary);
};

/**
 * Download the logo asset and re-encode it as a `data:` URL so it can be
 * rendered under II's strict `img-src` CSP (which does not allow arbitrary
 * https origins) and so the size and content type can be enforced up front.
 */
const fetchLogoAsDataUrl = async (url: URL): Promise<string | undefined> => {
  const response = await fetch(url.href, fetchOptions("image/*"));
  if (response.status !== 200) {
    return undefined;
  }
  const contentType = (response.headers.get("content-type") ?? "")
    .split(";")[0]
    .trim()
    .toLowerCase();
  if (!APP_LOGO_CONTENT_TYPES.includes(contentType)) {
    return undefined;
  }
  const buffer = await response.arrayBuffer();
  if (buffer.byteLength === 0 || buffer.byteLength > MAX_APP_LOGO_SIZE) {
    return undefined;
  }
  return `data:${contentType};base64,${toBase64(buffer)}`;
};

/**
 * Fetch and validate the app metadata served by `origin` on
 * {@link APP_METADATA_PATH}.
 *
 * Individual fields that fail validation are dropped; the result is
 * `undefined` when the file is absent, cannot be fetched (e.g. missing CORS
 * headers), is malformed, or contains no usable field — callers should then
 * fall back to other sources (e.g. the hostname).
 *
 * @param origin Origin of the app requesting authorization, i.e. the origin
 *   shown to the user (not the derivation origin, which may be remapped).
 */
export const fetchAppMetadata = async (
  origin: string,
): Promise<AppMetadata | undefined> => {
  try {
    const url = new URL(APP_METADATA_PATH, origin);
    const response = await fetch(url.href, fetchOptions("application/json"));
    if (response.status !== 200) {
      return undefined;
    }
    const contentLength = response.headers.get("content-length");
    if (
      contentLength !== null &&
      Number(contentLength) > MAX_APP_METADATA_SIZE
    ) {
      return undefined;
    }
    const body = await response.text();
    if (body.length > MAX_APP_METADATA_SIZE) {
      return undefined;
    }
    const parsed: unknown = JSON.parse(body);
    if (
      parsed === null ||
      typeof parsed !== "object" ||
      Array.isArray(parsed)
    ) {
      return undefined;
    }
    const { name, description, logo } = parsed as Record<string, unknown>;
    const metadata: AppMetadata = {
      name: cleanTextField(name, MAX_APP_NAME_LENGTH),
      description: cleanTextField(description, MAX_APP_DESCRIPTION_LENGTH),
    };
    const logoUrl = resolveLogoUrl(logo, origin);
    if (logoUrl !== undefined) {
      // A broken logo (e.g. missing CORS headers on the asset) only loses the
      // logo, not the name and description.
      metadata.logo = await fetchLogoAsDataUrl(logoUrl).catch(() => undefined);
    }
    if (
      metadata.name === undefined &&
      metadata.description === undefined &&
      metadata.logo === undefined
    ) {
      return undefined;
    }
    return metadata;
  } catch {
    // Missing file, missing CORS headers, redirect, timeout, invalid JSON, …:
    // the app simply gets the default (hostname-based) presentation.
    return undefined;
  }
};
