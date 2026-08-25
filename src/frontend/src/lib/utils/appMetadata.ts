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
 *   make the authorization page buffer an arbitrarily large response. They
 *   stream into a `Blob`, so the payload lives in the browser's blob store
 *   with only one chunk at a time in the JS heap.
 * - A document is applied only if every field it carries meets the
 *   requirements: one bad field rejects the whole document, with a console
 *   warning naming it, so the app's developers can see and fix the mistake
 *   instead of shipping a half-applied file that looks subtly wrong.
 * - The logo must live on the app's own origin. It is downloaded via `fetch`
 *   (II's CSP `connect-src` allows any https origin, unlike `img-src`),
 *   checked against an image content-type allowlist and a size cap, and then
 *   decoded and re-encoded by II before it is rendered from a `blob:` URL. So
 *   the bytes reaching the `<img>` are a still raster II produced itself: the
 *   app's file is never hotlinked, nothing about it has to be trusted to be an
 *   image, and no attacker-controlled payload ends up in the DOM or the JS
 *   heap (which a `data:` URL would put in both).
 * - Every failure mode (missing file, CORS, timeout, invalid JSON, …) yields
 *   `undefined` rather than an error: metadata is a display nicety and must
 *   never break the sign-in flow.
 */

import { gatewayOriginTwins } from "$lib/utils/urlUtils";

export interface AppMetadata {
  /** Display name of the app. */
  name?: string;
  /** Short description of the app. */
  description?: string;
  /** Ready-to-render `<img src>` value (a `blob:` URL for fetched logos). */
  logo?: string;
}

/**
 * The origins to ask for an app's metadata, in order.
 *
 * Callers pass the origin the app's identity is derived for, which has already
 * been remapped onto `ic0.app` when it is a canister gateway origin (see
 * `remapToLegacyDomain`), because principals must not depend on which gateway
 * domain the user arrived through. The metadata document carries no such
 * requirement, and a canister is not necessarily served on all three domains:
 * a canister reachable at `<id>.icp0.io` can answer `400
 * client_domain_canister_mismatch` at `<id>.ic0.app`. Asking only the remapped
 * origin would lose the metadata of every app in that position.
 *
 * So the remap is inverted here (via {@link gatewayOriginTwins}) and the twins
 * are used as fallbacks. All three resolve to the same canister, so this
 * widens where the document may be served, never whose document is used.
 */
export const appMetadataOrigins = (origin: string): string[] => [
  origin,
  ...gatewayOriginTwins(origin),
];

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

/** Maximum size of the logo asset in bytes (1 MiB). Deliberately generous:
 *  since the asset is re-encoded before it is rendered (see
 *  {@link transcodeToObjectUrl}), this bounds only the download, not what ends
 *  up held or displayed — so the cap should not be the reason an app that
 *  simply exported its logo without optimizing it loses it. */
export const MAX_APP_LOGO_SIZE = 1_048_576;

/** Content types the logo asset may be served with. `image/svg+xml` is
 *  deliberately absent: every logo is re-encoded from its decoded pixels (see
 *  {@link transcodeToObjectUrl}), which is what lets II guarantee that what it
 *  renders is a still image of bounded size, and SVG cannot be decoded that
 *  way across the browsers II supports. Apps serve a raster logo instead. */
export const APP_LOGO_CONTENT_TYPES = [
  "image/png",
  "image/jpeg",
  "image/webp",
  "image/gif",
  "image/avif",
];

/** Largest source image II will re-encode, per axis. The size cap alone does
 *  not bound this: a small file can declare enormous dimensions, and the
 *  decoded bitmap costs about four bytes per pixel. */
export const MAX_APP_LOGO_DIMENSION = 4_096;

/** Longest side of the re-encoded logo. The screens render it at roughly
 *  80–200 CSS pixels, so this is headroom for high-density displays and
 *  nothing more. */
export const APP_LOGO_RENDER_SIZE = 512;

/** Time budget per resource, spanning the connection and the body read; the
 *  UI renders a fallback in the meantime, so a slow origin only delays its
 *  own polish, never the sign-in flow. */
export const APP_METADATA_FETCH_TIMEOUT_MILLIS = 10_000;

/**
 * Read the body into a `Blob` under a hard byte cap, stopping the download as
 * soon as the cap is crossed rather than buffering an arbitrarily large body
 * first and measuring it afterwards.
 *
 * Piping through a counting transform is what keeps this off the JS heap: the
 * bytes accumulate in the browser's blob store and only one chunk at a time is
 * a JS buffer, so an attacker-controlled payload is never materialized as one.
 * Erroring the transform aborts the source stream, which is what makes the cap
 * bound the transfer and not just the result.
 */
const readBodyCapped = async (
  response: Response,
  maxBytes: number,
): Promise<Blob | undefined> => {
  if (response.body === null) {
    // No streamable body (older environments): read, then enforce the cap.
    const blob = await response.blob();
    return blob.size > maxBytes ? undefined : blob;
  }
  let received = 0;
  const capped = response.body.pipeThrough(
    new TransformStream<Uint8Array, Uint8Array>({
      transform: (chunk, controller) => {
        received += chunk.byteLength;
        if (received > maxBytes) {
          controller.error(new Error(`Response exceeds ${maxBytes} bytes`));
          return;
        }
        controller.enqueue(chunk);
      },
    }),
  );
  try {
    // Only the content type is carried over, so the blob keeps it; copying the
    // rest would attach a `Content-Length` and `Content-Encoding` that no
    // longer describe this stream.
    return await new Response(capped, {
      headers: { "content-type": response.headers.get("content-type") ?? "" },
    }).blob();
  } catch {
    return undefined;
  }
};

/** Outcome of a capped fetch. On failure the origin's HTTP status is carried
 *  when there was a response at all, so callers can tell an origin that
 *  answered (`404`: nothing served here) from one that never did (a network
 *  error, a CORS rejection, a redirect or a timeout leave `status` unset). */
type CappedFetch =
  | { ok: true; response: Response; blob: Blob }
  | { ok: false; status: number | undefined };

/**
 * Fetch `url` with the hardened options shared by both resources and read the
 * body under `maxBytes` (rejected up front when `Content-Length` declares an
 * oversize response, enforced while streaming otherwise). Fails on a non-200
 * status or when the cap is exceeded; network errors propagate to the caller.
 */
const fetchCapped = async (
  url: URL,
  accept: string,
  maxBytes: number,
): Promise<CappedFetch> => {
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
      return { ok: false, status: response.status };
    }
    const declaredLength = response.headers.get("content-length");
    if (declaredLength !== null && Number(declaredLength) > maxBytes) {
      await response.body?.cancel().catch(() => undefined);
      return { ok: false, status: response.status };
    }
    const blob = await readBodyCapped(response, maxBytes);
    return blob === undefined
      ? { ok: false, status: response.status }
      : { ok: true, response, blob };
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
 *  spaces below), the bidi embeddings and overrides U+202A-U+202E, and the
 *  deprecated U+FEFF.
 *
 *  Only the reordering controls are refused. An override makes text render in
 *  an order other than the one it is written in, which is what would let a name
 *  read as something it doesn't contain; the embeddings are deprecated in
 *  favour of the isolates for the same reason.
 *
 *  Deliberately allowed, because mixed-direction and non-Latin names need them:
 *  the bidi marks U+200E/U+200F/U+061C, which are zero-width hints that only
 *  affect where neutral characters (punctuation, digits) land at a direction
 *  boundary -- exactly what an Arabic name ending in "!" or a Hebrew name with
 *  an embedded Latin word requires; the bidi isolates U+2066-U+2069, the
 *  mechanism Unicode recommends for embedding a run of unknown direction (kept
 *  contained by {@link hasBalancedIsolates}); and the zero-width characters
 *  U+200B-U+200D, which carry meaning in several scripts -- line-break
 *  opportunities in Thai and Khmer, shaping in Persian, and holding emoji
 *  sequences together. */
const FORBIDDEN_CHARACTERS =
  // eslint-disable-next-line no-control-regex
  /[\u0000-\u0008\u000e-\u001f\u007f-\u009f\u202a-\u202e\ufeff]/;

/** Bidi isolate initiators (LRI, RLI, FSI) and the terminator (PDI). */
const ISOLATE_INITIATORS = "\u2066\u2067\u2068";
const ISOLATE_TERMINATOR = "\u2069";

/**
 * Whether the value closes every bidi isolate it opens, and closes none it
 * didn't open.
 *
 * An isolate contains its contents, so it cannot reorder the text around it --
 * but only while it is balanced. An unbalanced one runs to the end of the
 * paragraph, which for a name interpolated into one of II's own sentences means
 * past the app's string and into II's text. Requiring balance is what makes
 * "an app can style its own name, never the screen around it" true.
 */
const hasBalancedIsolates = (value: string): boolean => {
  let depth = 0;
  for (const char of value) {
    if (ISOLATE_INITIATORS.includes(char)) {
      depth += 1;
    } else if (char === ISOLATE_TERMINATOR) {
      depth -= 1;
      if (depth < 0) {
        return false;
      }
    }
  }
  return depth === 0;
};

/** Characters that render as nothing: whitespace, the bidi marks and isolate
 *  controls, and the zero-width characters. A field made only of these reads as
 *  absent, so it is refused rather than displayed as a blank name. */
const INVISIBLE_CHARACTERS = /[\s\u061c\u200b-\u200f\u2066-\u2069]/g;

/**
 * Validate an app-provided text field, returning the value to display (with
 * whitespace collapsed and trimmed), `undefined` when the field is absent, or
 * {@link INVALID} when it is present but does not meet the requirements.
 *
 * The length limit is applied to the value as served, counted in Unicode code
 * points, so it is what the published JSON Schema expresses (which can capture
 * every rule here except the isolate balance). Whitespace normalization is
 * presentation only and never rescues a value that breaks a requirement.
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
      "must not contain control characters, or bidirectional embeddings and overrides",
    );
  }
  if (!hasBalancedIsolates(value)) {
    return reject(field, "must close every bidirectional isolate it opens");
  }
  const normalized = value.replace(/\s+/g, " ").trim();
  if (normalized.replace(INVISIBLE_CHARACTERS, "").length === 0) {
    return reject(field, "must contain at least one visible character");
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

/**
 * Re-encode the downloaded bytes as an image II has produced itself, and hand
 * back a `blob:` URL for it.
 *
 * `createImageBitmap` rejects anything that isn't an image the browser can
 * decode, so the content-type header is never taken on trust. The bitmap is
 * then drawn once into a canvas and encoded again, which flattens an animation
 * to its first frame, drops whatever else rode along in the original container,
 * and scales the result down to what the screens actually render.
 *
 * The result is handed over as a `blob:` URL rather than a `data:` URL so the
 * bytes stay in the browser's blob store instead of being copied into the DOM
 * and the JS heap as a base64 string. The URL lives as long as the page: the
 * metadata store fetches once per origin and nothing supersedes the value, so
 * there is no point at which it could be revoked while still in use.
 */
const transcodeToObjectUrl = async (
  blob: Blob,
): Promise<string | undefined> => {
  // The blob goes to the decoder as it came off the network, so the encoded
  // image is never a JS buffer either.
  const bitmap = await createImageBitmap(blob);
  try {
    if (
      bitmap.width === 0 ||
      bitmap.height === 0 ||
      bitmap.width > MAX_APP_LOGO_DIMENSION ||
      bitmap.height > MAX_APP_LOGO_DIMENSION
    ) {
      return undefined;
    }
    const scale = Math.min(
      1,
      APP_LOGO_RENDER_SIZE / Math.max(bitmap.width, bitmap.height),
    );
    const canvas = document.createElement("canvas");
    canvas.width = Math.max(1, Math.round(bitmap.width * scale));
    canvas.height = Math.max(1, Math.round(bitmap.height * scale));
    const context = canvas.getContext("2d");
    if (context === null) {
      return undefined;
    }
    context.drawImage(bitmap, 0, 0, canvas.width, canvas.height);
    const blob = await new Promise<Blob | null>((resolve) =>
      // WebP keeps logos small and preserves transparency; browsers that
      // can't encode it fall back to PNG on their own, which does too.
      canvas.toBlob(resolve, "image/webp"),
    );
    return blob === null ? undefined : URL.createObjectURL(blob);
  } finally {
    bitmap.close();
  }
};

/**
 * Download the logo asset, check it against the content-type allowlist and the
 * size cap, and return a `blob:` URL for II's own re-encoding of it (see
 * {@link transcodeToObjectUrl}).
 */
const fetchLogoObjectUrl = async (url: URL): Promise<string | undefined> => {
  const result = await fetchCapped(url, "image/*", MAX_APP_LOGO_SIZE);
  if (!result.ok) {
    return undefined;
  }
  const contentType = (result.response.headers.get("content-type") ?? "")
    .split(";")[0]
    .trim()
    .toLowerCase();
  if (!APP_LOGO_CONTENT_TYPES.includes(contentType)) {
    return undefined;
  }
  if (result.blob.size === 0) {
    return undefined;
  }
  return await transcodeToObjectUrl(result.blob);
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
 * When that origin is a canister gateway origin it has already been remapped
 * onto `ic0.app`, and the canister may not be served there; its `icp0.io` and
 * `icp.net` twins are then tried in turn (see {@link appMetadataOrigins}). An
 * origin that answers `404`, or answers at all with the document, settles the
 * question and no twin is tried, so the ordinary case of an app that publishes
 * nothing still costs a single request.
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
  for (const candidate of appMetadataOrigins(origin)) {
    const attempt = await fetchAppMetadataFrom(candidate);
    if (attempt.served) {
      return attempt.metadata;
    }
  }
  return undefined;
};

/** Whether an origin that answered with `status` gave a definitive answer
 *  about the document, rather than one that says this domain isn't where the
 *  app is served. `404` is the app's canister saying it has no such file, and
 *  `200` means the document was received (it may still fail validation, which
 *  is equally definitive). Anything else — `400
 *  client_domain_canister_mismatch` from a gateway domain the canister isn't
 *  served on, a `5xx`, or no response at all — leaves the question open. */
const isDefinitiveStatus = (status: number | undefined): boolean =>
  status === 404 || status === 200;

/** The outcome of asking one origin for the document. `served: false` means
 *  the origin never answered for the app, so a sibling origin may still. */
type MetadataAttempt =
  { served: true; metadata: AppMetadata | undefined } | { served: false };

const fetchAppMetadataFrom = async (
  origin: string,
): Promise<MetadataAttempt> => {
  let result: CappedFetch;
  try {
    const url = new URL(APP_METADATA_PATH, origin);
    result = await fetchCapped(url, "application/json", MAX_APP_METADATA_SIZE);
  } catch {
    // No response at all: a network error, a CORS rejection, a redirect or a
    // timeout. Another of the app's gateway origins may still serve it.
    return { served: false };
  }
  if (!result.ok) {
    return isDefinitiveStatus(result.status)
      ? { served: true, metadata: undefined }
      : { served: false };
  }
  try {
    // Fatal decoding: malformed UTF-8 rejects the file (via the catch below)
    // instead of silently turning into U+FFFD on the sign-in screen.
    // The document is at most 8 KiB and has to be parsed, so this one does
    // become a JS string — unlike the logo, which never leaves the blob store.
    const parsed: unknown = JSON.parse(
      new TextDecoder("utf-8", { fatal: true }).decode(
        await result.blob.arrayBuffer(),
      ),
    );
    if (
      parsed === null ||
      typeof parsed !== "object" ||
      Array.isArray(parsed)
    ) {
      return { served: true, metadata: undefined };
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
      return { served: true, metadata: undefined };
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
      metadata.logo = await fetchLogoObjectUrl(logoUrl).catch(() => undefined);
      if (metadata.logo === undefined) {
        console.warn(
          `Ignoring the \`logo\` in ${APP_METADATA_PATH}: it could not be decoded as a still image of an allowed type, within ${MAX_APP_LOGO_SIZE} bytes and ${MAX_APP_LOGO_DIMENSION} pixels per axis.`,
        );
      }
    }
    if (
      metadata.name === undefined &&
      metadata.description === undefined &&
      metadata.logo === undefined
    ) {
      return { served: true, metadata: undefined };
    }
    return { served: true, metadata };
  } catch {
    // The document was received but could not be decoded or parsed. That is
    // this origin's answer, so no sibling origin is tried: the app simply gets
    // the default (hostname-based) presentation.
    return { served: true, metadata: undefined };
  }
};
