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
 *   mirroring the `/.well-known/ii-alternative-origins` fetch, with a
 *   timeout spanning both the connection and the body read.
 * - Bodies are read with hard byte caps, enforced up front via
 *   `Content-Length` and chunk-by-chunk while streaming, so an origin can't
 *   make the authorization page buffer an arbitrarily large response.
 * - A document is applied only if every field it carries meets the
 *   requirements: one bad field rejects the whole document, with a console
 *   warning naming it, so the app's developers can see and fix the mistake
 *   instead of shipping a half-applied file that looks subtly wrong.
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

/** Maximum size of the metadata file in bytes (8 KiB). */
export const MAX_APP_METADATA_SIZE = 8_192;

/** Maximum length of the app name in Unicode code points, after whitespace
 *  normalization. */
export const MAX_APP_NAME_LENGTH = 40;

/** Maximum length of the app description in Unicode code points, after
 *  whitespace normalization. */
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

/** Time budget per resource, spanning the connection and the body read; the
 *  UI renders a fallback in the meantime, so a slow origin only delays its
 *  own polish, never the sign-in flow. */
export const APP_METADATA_FETCH_TIMEOUT_MILLIS = 10_000;

/**
 * Read the body with a hard byte cap, cancelling the stream as soon as the
 * cap is crossed rather than buffering an arbitrarily large body first and
 * measuring it afterwards (mirrors the capped reader in `authCallbacks.ts`).
 * Returns `undefined` when the cap is exceeded.
 */
const readBodyCapped = async (
  response: Response,
  maxBytes: number,
): Promise<Uint8Array | undefined> => {
  if (response.body === null) {
    // No streamable body (older environments): read, then enforce the cap.
    const buffer = new Uint8Array(await response.arrayBuffer());
    return buffer.byteLength > maxBytes ? undefined : buffer;
  }
  const reader = response.body.getReader();
  const chunks: Uint8Array[] = [];
  let received = 0;
  for (;;) {
    const { done, value } = await reader.read();
    if (done) {
      break;
    }
    received += value.byteLength;
    if (received > maxBytes) {
      // Stop pulling from the network instead of buffering the rest.
      await reader.cancel().catch(() => undefined);
      return undefined;
    }
    chunks.push(value);
  }
  const body = new Uint8Array(received);
  let offset = 0;
  for (const chunk of chunks) {
    body.set(chunk, offset);
    offset += chunk.byteLength;
  }
  return body;
};

/**
 * Fetch `url` with the hardened options shared by both resources and read the
 * body under `maxBytes` (rejected up front when `Content-Length` declares an
 * oversize response, enforced while streaming otherwise). Returns `undefined`
 * on a non-200 status or when the cap is exceeded; network errors propagate
 * to the caller.
 */
const fetchCapped = async (
  url: URL,
  accept: string,
  maxBytes: number,
): Promise<{ response: Response; body: Uint8Array } | undefined> => {
  // AbortController + setTimeout matches the rest of the FE (e.g.
  // `lib/utils/dnssec/doh.ts`, `lib/utils/ssoDiscovery.ts`); we avoid
  // `AbortSignal.timeout` because the project still supports browsers
  // without it. The timer stays armed until the body has been read, so a
  // slow origin can't keep the request pending indefinitely.
  const controller = new AbortController();
  const timeoutId = setTimeout(
    () => controller.abort(),
    APP_METADATA_FETCH_TIMEOUT_MILLIS,
  );
  try {
    const response = await fetch(url.href, {
      // fail on redirects
      redirect: "error",
      headers: {
        Accept: accept,
      },
      // do not send cookies or other credentials
      credentials: "omit",
      signal: controller.signal,
    });
    if (response.status !== 200) {
      // Cancel the stream so the download stops now, not at GC time.
      await response.body?.cancel().catch(() => undefined);
      return undefined;
    }
    const declaredLength = response.headers.get("content-length");
    if (declaredLength !== null && Number(declaredLength) > maxBytes) {
      await response.body?.cancel().catch(() => undefined);
      return undefined;
    }
    const body = await readBodyCapped(response, maxBytes);
    return body === undefined ? undefined : { response, body };
  } finally {
    clearTimeout(timeoutId);
  }
};

/** Marker for a field that is present but violates the requirements. It
 *  rejects the whole document rather than just itself — see
 *  {@link fetchAppMetadata}. */
const INVALID = Symbol("invalid");

/** A validated field: the value to use, `undefined` when the field is absent,
 *  or {@link INVALID} when it is present and unusable. */
type Validated<T> = T | undefined | typeof INVALID;

/**
 * Reject the document, logging which field is at fault and why. Without this,
 * a mistake in the file would be invisible to the app's developers: II simply
 * falls back to the previous presentation, on a screen they don't control.
 */
const reject = (field: string, problem: string): typeof INVALID => {
  console.warn(`Ignoring ${APP_METADATA_PATH}: \`${field}\` ${problem}.`);
  return INVALID;
};

/** Characters an app-provided text field must not contain: control characters
 *  (except the ASCII whitespace ones \t \n \v \f \r, which are normalized to
 *  spaces below) together with zero-width and bidi formatting characters,
 *  which could otherwise be used to visually spoof the sign-in screen.
 *
 *  The zero-width joiners U+200C/U+200D are deliberately allowed: ZWNJ drives
 *  correct shaping in scripts such as Persian and ZWJ holds emoji sequences
 *  together (dropping it decays one joined emoji into two), and neither is a
 *  control or bidi character. */
const FORBIDDEN_CHARACTERS =
  // eslint-disable-next-line no-control-regex
  /[\u0000-\u0008\u000e-\u001f\u007f-\u009f\u061c\u200b\u200e\u200f\u202a-\u202e\u2066-\u2069\ufeff]/;

/**
 * Validate an app-provided text field, returning the value to display (with
 * whitespace collapsed and trimmed), `undefined` when the field is absent, or
 * {@link INVALID} when it is present but does not meet the requirements.
 *
 * The length limit is applied to the value as served, counted in Unicode code
 * points, so it is exactly what the published JSON Schema expresses — a
 * document that validates against the schema is one II accepts. Whitespace
 * normalization is presentation only and never rescues an over-long value.
 */
const validateTextField = (
  value: unknown,
  field: string,
  maxLength: number,
): Validated<string> => {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== "string") {
    return reject(field, "must be a string");
  }
  if ([...value].length > maxLength) {
    return reject(field, `must not exceed ${maxLength} characters`);
  }
  if (FORBIDDEN_CHARACTERS.test(value)) {
    return reject(
      field,
      "must not contain control or bidirectional formatting characters",
    );
  }
  const normalized = value.replace(/\s+/g, " ").trim();
  if (normalized.length === 0) {
    return reject(field, "must not be blank");
  }
  return normalized;
};

/**
 * Validate the `logo` field: it must resolve (relative to the app origin) to a
 * URL on that same origin. Same-origin keeps the sign-in private (no third
 * party learns about it through an image load) and rules out `data:`,
 * `javascript:` and other non-http(s) schemes, whose origin never matches.
 */
const validateLogoUrl = (value: unknown, origin: string): Validated<URL> => {
  if (value === undefined) {
    return undefined;
  }
  if (typeof value !== "string" || value.length === 0) {
    return reject("logo", "must be a non-empty string");
  }
  let url: URL;
  try {
    url = new URL(value, origin);
  } catch {
    return reject("logo", "must be a valid URL");
  }
  if (url.origin !== new URL(origin).origin) {
    return reject("logo", "must be on the same origin as the document");
  }
  return url;
};

const toBase64 = (bytes: Uint8Array): string => {
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
  const result = await fetchCapped(url, "image/*", MAX_APP_LOGO_SIZE);
  if (result === undefined) {
    return undefined;
  }
  const contentType = (result.response.headers.get("content-type") ?? "")
    .split(";")[0]
    .trim()
    .toLowerCase();
  if (!APP_LOGO_CONTENT_TYPES.includes(contentType)) {
    return undefined;
  }
  if (result.body.byteLength === 0) {
    return undefined;
  }
  return `data:${contentType};base64,${toBase64(result.body)}`;
};

/**
 * Fetch and validate the app metadata served by `origin` on
 * {@link APP_METADATA_PATH}.
 *
 * Validation is all-or-nothing: a field that does not meet the requirements
 * rejects the whole document, so an app never ends up displayed with half of
 * its metadata applied and nothing to indicate why. The result is `undefined`
 * when the file is absent, cannot be fetched (e.g. missing CORS headers), is
 * malformed, fails validation, or carries no usable field — callers should
 * then fall back to other sources (e.g. the hostname).
 *
 * @param origin Origin the app's identity is derived for: the validated
 *   derivation origin when the authorization request carries one, and the
 *   requesting origin otherwise. That origin is the single source of truth for
 *   an app's presentation, so sibling origins sharing it (its alternative
 *   origins, which it has certified as its own) present identically without
 *   having to duplicate the document.
 */
export const fetchAppMetadata = async (
  origin: string,
): Promise<AppMetadata | undefined> => {
  try {
    const url = new URL(APP_METADATA_PATH, origin);
    const result = await fetchCapped(
      url,
      "application/json",
      MAX_APP_METADATA_SIZE,
    );
    if (result === undefined) {
      return undefined;
    }
    // Fatal decoding: malformed UTF-8 rejects the file (via the catch below)
    // instead of silently turning into U+FFFD on the sign-in screen.
    const parsed: unknown = JSON.parse(
      new TextDecoder("utf-8", { fatal: true }).decode(result.body),
    );
    if (
      parsed === null ||
      typeof parsed !== "object" ||
      Array.isArray(parsed)
    ) {
      return undefined;
    }
    const { name, description, logo } = parsed as Record<string, unknown>;
    // Validate every field before bailing out, so a document with more than
    // one problem reports all of them in one go.
    const validName = validateTextField(name, "name", MAX_APP_NAME_LENGTH);
    const validDescription = validateTextField(
      description,
      "description",
      MAX_APP_DESCRIPTION_LENGTH,
    );
    const logoUrl = validateLogoUrl(logo, origin);
    if (
      validName === INVALID ||
      validDescription === INVALID ||
      logoUrl === INVALID
    ) {
      return undefined;
    }
    const metadata: AppMetadata = {
      name: validName,
      description: validDescription,
    };
    if (logoUrl !== undefined) {
      // The asset is a second network resource, so unlike the fields of the
      // document its failures aren't necessarily authoring mistakes (a 500 or
      // a dropped connection is transient). Losing just the logo is the better
      // outcome here: the name and description still render.
      metadata.logo = await fetchLogoAsDataUrl(logoUrl).catch(() => undefined);
      if (metadata.logo === undefined) {
        console.warn(
          `Ignoring the \`logo\` in ${APP_METADATA_PATH}: it could not be loaded as an image of an allowed type within ${MAX_APP_LOGO_SIZE} bytes.`,
        );
      }
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
