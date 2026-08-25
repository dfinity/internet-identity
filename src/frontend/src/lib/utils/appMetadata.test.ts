import {
  APP_LOGO_RENDER_SIZE,
  APP_METADATA_FETCH_TIMEOUT_MILLIS,
  APP_METADATA_PATH,
  MAX_APP_DESCRIPTION_LENGTH,
  MAX_APP_LOGO_DIMENSION,
  MAX_APP_LOGO_SIZE,
  MAX_APP_METADATA_SIZE,
  MAX_APP_NAME_LENGTH,
  appMetadataOrigins,
  fetchAppMetadata,
} from "$lib/utils/appMetadata";
import { beforeEach, expect, test, vi } from "vitest";

beforeEach(() => {
  // Drop the previous test's canvas/decoder stubs, so a test that needs them
  // has to install them itself rather than inheriting them by accident.
  vi.restoreAllMocks();
  vi.unstubAllGlobals();
  // A rejected document logs a warning naming the offending field, which is
  // how an app's developers find out. Silence it by default; the test below
  // asserts on it explicitly.
  vi.spyOn(console, "warn").mockImplementation(() => undefined);
});

const ORIGIN = "https://app.example.com";
const METADATA_URL = `${ORIGIN}${APP_METADATA_PATH}`;

const JSON_FETCH_OPTS = expect.objectContaining({
  redirect: "error",
  headers: {
    Accept: "application/json",
  },
  credentials: "omit",
});
const IMAGE_FETCH_OPTS = expect.objectContaining({
  redirect: "error",
  headers: {
    Accept: "image/*",
  },
  credentials: "omit",
});

const PNG_BYTES = new Uint8Array([0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a]);
const LOGO_OBJECT_URL =
  "blob:https://id.ai/00000000-0000-0000-0000-000000000000";

/**
 * jsdom implements neither image decoding nor canvas, so the re-encoding step
 * is stubbed: `createImageBitmap` reports whatever dimensions (or failure) the
 * test wants, and the canvas hands back a fixed blob. Returns the stubs so a
 * test can assert on what II drew and how large it drew it.
 */
const setupImageMock = ({
  width = 64,
  height = 64,
  decodable = true,
}: { width?: number; height?: number; decodable?: boolean } = {}) => {
  const close = vi.fn();
  const createImageBitmap = vi.fn(() =>
    decodable
      ? Promise.resolve({ width, height, close } as unknown as ImageBitmap)
      : Promise.reject(new Error("The source image could not be decoded.")),
  );
  vi.stubGlobal("createImageBitmap", createImageBitmap);
  const drawImage = vi.fn();
  vi.spyOn(HTMLCanvasElement.prototype, "getContext").mockReturnValue({
    drawImage,
  } as unknown as CanvasRenderingContext2D);
  const toBlob = vi
    .spyOn(HTMLCanvasElement.prototype, "toBlob")
    .mockImplementation((callback, type) =>
      callback(new Blob(["re-encoded"], { type: type ?? "image/png" })),
    );
  const createObjectURL = vi.fn(() => LOGO_OBJECT_URL);
  URL.createObjectURL = createObjectURL;
  return { createImageBitmap, drawImage, toBlob, createObjectURL, close };
};

const imageResponse = (
  bytes: Uint8Array<ArrayBuffer> = PNG_BYTES,
  contentType = "image/png",
): Response =>
  new Response(bytes, {
    status: 200,
    headers: { "Content-Type": contentType },
  });

const setupFetchMock = (...responses: (Response | Error)[]) => {
  const fetchMock = vi.fn();
  global.fetch = fetchMock;
  responses.forEach((response) => {
    if (response instanceof Error) {
      fetchMock.mockRejectedValueOnce(response);
    } else {
      fetchMock.mockResolvedValueOnce(response);
    }
  });
  return fetchMock;
};

test("should fetch metadata from the well-known path with hardened options", async () => {
  const fetchMock = setupFetchMock(
    Response.json({ name: "Example App", description: "An example app" }),
  );

  const result = await fetchAppMetadata(ORIGIN);

  expect(result).toEqual({
    name: "Example App",
    description: "An example app",
  });
  expect(fetchMock).toHaveBeenCalledExactlyOnceWith(
    METADATA_URL,
    JSON_FETCH_OPTS,
  );
});

test("should fetch a same-origin logo and render it from a blob url", async () => {
  const fetchMock = setupFetchMock(
    Response.json({ name: "Example App", logo: "/assets/logo.png" }),
    imageResponse(),
  );
  const { createImageBitmap, createObjectURL, toBlob } = setupImageMock();

  const result = await fetchAppMetadata(ORIGIN);

  expect(result).toEqual({
    name: "Example App",
    logo: LOGO_OBJECT_URL,
  });
  // The downloaded bytes reach the decoder as a blob, never as a JS buffer.
  expect(createImageBitmap).toHaveBeenCalledWith(expect.any(Blob));
  // The rendered bytes are II's own re-encoding, held in the browser's blob
  // store: no attacker-controlled payload reaches the DOM or the JS heap, as a
  // `data:` URL would.
  expect(toBlob).toHaveBeenCalledOnce();
  expect(createObjectURL).toHaveBeenCalledOnce();
  expect(fetchMock).toHaveBeenNthCalledWith(1, METADATA_URL, JSON_FETCH_OPTS);
  expect(fetchMock).toHaveBeenNthCalledWith(
    2,
    `${ORIGIN}/assets/logo.png`,
    IMAGE_FETCH_OPTS,
  );
});

test("should resolve relative logo paths against the app origin", async () => {
  const fetchMock = setupFetchMock(
    Response.json({ logo: "assets/logo.webp" }),
    imageResponse(PNG_BYTES, "image/webp"),
  );
  setupImageMock();

  const result = await fetchAppMetadata(ORIGIN);

  expect(result?.logo).toBe(LOGO_OBJECT_URL);
  expect(fetchMock).toHaveBeenNthCalledWith(
    2,
    `${ORIGIN}/assets/logo.webp`,
    IMAGE_FETCH_OPTS,
  );
});

test("should work for http origins with a port (local development)", async () => {
  const fetchMock = setupFetchMock(Response.json({ name: "Local App" }));

  const result = await fetchAppMetadata("http://localhost:5173");

  expect(result).toEqual({ name: "Local App" });
  expect(fetchMock).toHaveBeenCalledExactlyOnceWith(
    `http://localhost:5173${APP_METADATA_PATH}`,
    JSON_FETCH_OPTS,
  );
});

test("should return undefined when the file is missing", async () => {
  setupFetchMock(new Response(undefined, { status: 404 }));

  expect(await fetchAppMetadata(ORIGIN)).toBeUndefined();
});

test("should return undefined when the fetch fails (e.g. missing CORS headers)", async () => {
  setupFetchMock(new TypeError("Failed to fetch"));

  expect(await fetchAppMetadata(ORIGIN)).toBeUndefined();
});

test("should abort and give up when the origin never responds", async () => {
  // A hanging origin must not leave the request pending forever: the timeout
  // aborts the signal and the metadata falls back. Guards against the timeout
  // being removed or scoped so it can't fire.
  vi.useFakeTimers();
  try {
    const fetchMock = vi.fn(
      (_url: string, init?: RequestInit) =>
        new Promise<Response>((_resolve, reject) => {
          init?.signal?.addEventListener("abort", () =>
            reject(
              new DOMException("The operation was aborted.", "AbortError"),
            ),
          );
        }),
    );
    global.fetch = fetchMock as unknown as typeof fetch;

    const metadata = fetchAppMetadata(ORIGIN);
    await vi.advanceTimersByTimeAsync(APP_METADATA_FETCH_TIMEOUT_MILLIS);

    await expect(metadata).resolves.toBeUndefined();
    expect(fetchMock.mock.calls[0][1]?.signal?.aborted).toBe(true);
  } finally {
    vi.useRealTimers();
  }
});

test("should keep waiting until the timeout elapses", async () => {
  // Sanity check on the guard above: the signal must not be aborted early,
  // otherwise the test would pass even with a near-zero timeout.
  vi.useFakeTimers();
  try {
    const fetchMock = vi.fn(
      (_url: string, _init?: RequestInit) => new Promise<Response>(() => {}),
    );
    global.fetch = fetchMock as unknown as typeof fetch;

    void fetchAppMetadata(ORIGIN);
    await vi.advanceTimersByTimeAsync(APP_METADATA_FETCH_TIMEOUT_MILLIS - 1);

    expect(fetchMock.mock.calls[0][1]?.signal?.aborted).toBe(false);
  } finally {
    vi.useRealTimers();
  }
});

test("should return undefined on redirects", async () => {
  setupFetchMock(Response.redirect("https://evil.com/metadata"));

  expect(await fetchAppMetadata(ORIGIN)).toBeUndefined();
});

test("should return undefined for malformed bodies", async () => {
  for (const body of [
    "<!doctype html><title>not json</title>",
    JSON.stringify(["name"]),
    JSON.stringify("name"),
    JSON.stringify(null),
    JSON.stringify(42),
  ]) {
    setupFetchMock(new Response(body, { status: 200 }));
    expect(await fetchAppMetadata(ORIGIN), body).toBeUndefined();
  }
});

test("should return undefined for malformed UTF-8", async () => {
  // 0xff is never valid in UTF-8; fatal decoding must reject the file
  // rather than smuggling U+FFFD replacement characters into the metadata.
  setupFetchMock(
    new Response(new Uint8Array([0x7b, 0xff, 0x7d]), { status: 200 }),
  );

  expect(await fetchAppMetadata(ORIGIN)).toBeUndefined();
});

test("should return undefined when no usable field is present", async () => {
  for (const body of [{}, { unrelated: "field" }]) {
    setupFetchMock(Response.json(body));
    expect(
      await fetchAppMetadata(ORIGIN),
      JSON.stringify(body),
    ).toBeUndefined();
  }
});

test("should return undefined when the file exceeds the size limit", async () => {
  setupFetchMock(
    Response.json({
      name: "Example App",
      padding: "a".repeat(MAX_APP_METADATA_SIZE),
    }),
  );

  expect(await fetchAppMetadata(ORIGIN)).toBeUndefined();
});

test("should return undefined when content-length exceeds the size limit", async () => {
  // Header-only guard: the body itself is small but the declared length is not.
  setupFetchMock(
    new Response(JSON.stringify({ name: "Example App" }), {
      status: 200,
      headers: { "Content-Length": `${MAX_APP_METADATA_SIZE + 1}` },
    }),
  );

  expect(await fetchAppMetadata(ORIGIN)).toBeUndefined();
});

test("should reject the whole document when a text field exceeds its limit", async () => {
  // Not just the offending field: an app that ships a too-long name sees its
  // metadata not applied at all, which is noticeable and points at the fix,
  // rather than silently losing one field on a screen it doesn't control.
  setupFetchMock(
    Response.json({
      name: "a".repeat(MAX_APP_NAME_LENGTH + 1),
      description: "b".repeat(MAX_APP_DESCRIPTION_LENGTH),
    }),
  );

  expect(await fetchAppMetadata(ORIGIN)).toBeUndefined();
});

test("should reject the whole document when a field has the wrong type", async () => {
  for (const body of [
    { name: 42, description: "An example app" },
    { name: "Example App", description: true },
    { name: "Example App", description: null },
    { name: "Example App", logo: [] },
    { name: "Example App", logo: "" },
  ]) {
    setupFetchMock(Response.json(body));

    expect(
      await fetchAppMetadata(ORIGIN),
      JSON.stringify(body),
    ).toBeUndefined();
  }
});

test("should name the offending field when rejecting a document", async () => {
  // The console warning is the only signal an app's developers get, so it has
  // to say which field is at fault.
  setupFetchMock(Response.json({ name: "a".repeat(MAX_APP_NAME_LENGTH + 1) }));

  expect(await fetchAppMetadata(ORIGIN)).toBeUndefined();
  expect(console.warn).toHaveBeenCalledWith(expect.stringContaining("`name`"));
  expect(console.warn).toHaveBeenCalledWith(
    expect.stringContaining(APP_METADATA_PATH),
  );
});

test("should normalize whitespace in text fields", async () => {
  // Whitespace is legitimate in a JSON document but would render as gaps, so
  // runs of it collapse to single spaces and the value is trimmed. This is
  // presentation only: it never rescues a value that breaks a requirement.
  setupFetchMock(
    Response.json({
      name: "  Example \t App\n",
      description: "Multi\r\nline   description ",
    }),
  );

  const result = await fetchAppMetadata(ORIGIN);

  expect(result).toEqual({
    name: "Example App",
    description: "Multi line description",
  });
});

test("should reject documents whose text fields carry reordering controls", async () => {
  // Control characters, and the bidi embeddings and overrides: the latter make
  // text render in an order other than the one it is written in, which is how a
  // name could read as something it doesn't contain.
  for (const char of [
    "\u0000", // NUL
    "\u0007", // BEL
    "\u001b", // ESC
    "\u202a", // left-to-right embedding
    "\u202b", // right-to-left embedding
    "\u202c", // pop directional formatting
    "\u202d", // left-to-right override
    "\u202e", // right-to-left override
    "\ufeff", // zero-width no-break space
  ]) {
    setupFetchMock(
      Response.json({ name: `Example${char}App`, description: "An example" }),
    );

    expect(
      await fetchAppMetadata(ORIGIN),
      char.codePointAt(0)?.toString(16),
    ).toBeUndefined();
  }
});

test("should accept the bidi characters mixed-direction names need", async () => {
  // These only hint at where neutral characters land, or isolate a run; none of
  // them can reorder text. Refusing them would break exactly the names that
  // need them: RTL text with an embedded Latin word, or ending in punctuation
  // whose side would otherwise follow the paragraph direction.
  for (const name of [
    `\u200fשלום Example!`, // RTL mark
    `Example \u200eעברית`, // LTR mark
    `\u061cالعربية Example`, // arabic letter mark
    `\u2068Example\u2069 في المتجر`, // first-strong isolate, balanced
    `\u2066Example\u2069 و\u2067עברית\u2069`, // nested, balanced
    `ราคา\u200bถูก`, // zero-width space as a Thai line-break opportunity
  ]) {
    setupFetchMock(Response.json({ name }));

    expect(await fetchAppMetadata(ORIGIN), name).toEqual({ name });
  }
});

test("should reject unbalanced bidi isolates", async () => {
  // An isolate only contains its contents while it is closed. Left open, it
  // runs to the end of the paragraph -- past the app's own name and into the
  // sentence II renders around it.
  for (const name of [
    `Example\u2066`, // opened, never closed
    `\u2069Example`, // closed without being opened
    `\u2066Example\u2069\u2069`, // one close too many
    `\u2068\u2067Example\u2069`, // one close too few
  ]) {
    setupFetchMock(Response.json({ name, description: "An example" }));

    expect(await fetchAppMetadata(ORIGIN), name).toBeUndefined();
  }
});

test("should reject text fields with nothing visible in them", async () => {
  // Whitespace, bidi marks, isolate controls and zero-width characters all
  // render as nothing, so a field made only of those is an absent field.
  for (const name of [
    "   ",
    "\u200b\u200b",
    "\u200e\u200f",
    ` \u2066\u2069 `,
  ]) {
    setupFetchMock(Response.json({ name, description: "An example" }));

    expect(
      await fetchAppMetadata(ORIGIN),
      JSON.stringify(name),
    ).toBeUndefined();
  }
});

test("should preserve the zero-width joiners scripts and emoji need", async () => {
  // ZWNJ (U+200C) drives correct shaping in scripts such as Persian, and ZWJ
  // (U+200D) holds emoji sequences together. Neither is a control or bidi
  // character, so stripping them would silently mangle legitimate names --
  // a single joined emoji would decay into two.
  const name = "Acme \u{1f469}\u200d\u{1f4bb}";
  const description = "Zero\u200cwidth non-joiner survives too";
  setupFetchMock(Response.json({ name, description }));

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name, description });
});

test("should count text limits in code points, not UTF-16 units", async () => {
  // Each emoji is one code point but two UTF-16 units: a name of exactly
  // MAX_APP_NAME_LENGTH emoji is within the documented limit.
  const emojiName = "🌍".repeat(MAX_APP_NAME_LENGTH);
  setupFetchMock(Response.json({ name: emojiName }));
  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: emojiName });

  setupFetchMock(Response.json({ name: "🌍".repeat(MAX_APP_NAME_LENGTH + 1) }));
  expect(await fetchAppMetadata(ORIGIN)).toBeUndefined();
});

test("should reject the whole document when the logo is not same-origin", async () => {
  for (const logo of [
    "https://evil.com/logo.png",
    "//evil.com/logo.png",
    "https://sub.app.example.com/logo.png",
    "http://app.example.com/logo.png", // scheme downgrade
    "data:image/png;base64,AAAA",
    "javascript:alert(1)",
    "https://", // unparseable
  ]) {
    const fetchMock = setupFetchMock(
      Response.json({ name: "Example App", logo }),
    );

    // A logo pointing somewhere else is an authoring mistake in the document
    // (and a privacy leak if honored), so it invalidates the document rather
    // than quietly dropping just the logo.
    expect(await fetchAppMetadata(ORIGIN), logo).toBeUndefined();
    // The logo must not even be fetched.
    expect(fetchMock, logo).toHaveBeenCalledTimes(1);
  }
});

// Failures of the logo *asset* stay non-fatal, unlike invalid fields in the
// document above: fetching a second resource can fail transiently (a 500, a
// dropped connection), and losing the name and description over that would be
// worse than rendering the app without its logo.
test("should drop logos served with a non-image content type", async () => {
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    imageResponse(PNG_BYTES, "text/html"),
  );
  setupImageMock();

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

test("should accept content types regardless of casing and parameters", async () => {
  setupFetchMock(
    Response.json({ logo: "/logo.png" }),
    imageResponse(PNG_BYTES, "IMAGE/PNG; charset=binary"),
  );
  setupImageMock();

  expect((await fetchAppMetadata(ORIGIN))?.logo).toBe(LOGO_OBJECT_URL);
});

test("should drop svg logos, which cannot be re-encoded", async () => {
  // Every logo is rendered from II's own re-encoding of the decoded pixels,
  // which is what bounds it and guarantees it is a still image; SVG can't go
  // through that across browsers, so it is not an accepted content type.
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.svg" }),
    imageResponse(PNG_BYTES, "image/svg+xml"),
  );
  setupImageMock();

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

test("should drop logos that do not decode as an image", async () => {
  // The content-type header is the app's claim; decoding is the check. HTML or
  // a corrupt file served as `image/png` never reaches an `<img>`.
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    imageResponse(),
  );
  setupImageMock({ decodable: false });

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

test("should drop logos whose decoded dimensions exceed the cap", async () => {
  // The byte cap doesn't bound this: a small file can declare enormous
  // dimensions, and the decoded bitmap costs about four bytes per pixel.
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    imageResponse(),
  );
  const { drawImage } = setupImageMock({
    width: MAX_APP_LOGO_DIMENSION + 1,
    height: 8,
  });

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
  expect(drawImage).not.toHaveBeenCalled();
});

test("should scale logos down to the size the screens render", async () => {
  setupFetchMock(Response.json({ logo: "/logo.png" }), imageResponse());
  const { drawImage, close } = setupImageMock({
    width: APP_LOGO_RENDER_SIZE * 4,
    height: APP_LOGO_RENDER_SIZE * 2,
  });

  expect((await fetchAppMetadata(ORIGIN))?.logo).toBe(LOGO_OBJECT_URL);
  // Longest side capped, aspect ratio kept.
  expect(drawImage).toHaveBeenCalledWith(
    expect.anything(),
    0,
    0,
    APP_LOGO_RENDER_SIZE,
    APP_LOGO_RENDER_SIZE / 2,
  );
  // The decoded bitmap is released rather than left to the collector.
  expect(close).toHaveBeenCalledOnce();
});

test("should keep a logo that is already smaller than the render size", async () => {
  setupFetchMock(Response.json({ logo: "/logo.png" }), imageResponse());
  const { drawImage } = setupImageMock({ width: 48, height: 32 });

  expect((await fetchAppMetadata(ORIGIN))?.logo).toBe(LOGO_OBJECT_URL);
  expect(drawImage).toHaveBeenCalledWith(expect.anything(), 0, 0, 48, 32);
});

test("should drop logos exceeding the size limit", async () => {
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    imageResponse(new Uint8Array(MAX_APP_LOGO_SIZE + 1)),
  );
  setupImageMock();

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

test("should drop empty logos", async () => {
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    imageResponse(new Uint8Array(0)),
  );
  setupImageMock();

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

test("should keep name and description when the logo fetch fails", async () => {
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    new TypeError("Failed to fetch"),
  );

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

test("should drop logos responding with a redirect", async () => {
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    Response.redirect("https://evil.com/logo.png"),
  );

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

test("should ignore unknown fields", async () => {
  setupFetchMock(
    Response.json({ name: "Example App", futureField: { nested: true } }),
  );

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

// Gateway twin fallback: an origin the caller passes has already been remapped
// onto `ic0.app` for principal derivation, but the canister need not be served
// there. See `appMetadataOrigins`.

const CANISTER_ID = "rdmx6-jaaaa-aaaaa-aaadq-cai";
const LEGACY_ORIGIN = `https://${CANISTER_ID}.ic0.app`;
const ICP0_ORIGIN = `https://${CANISTER_ID}.icp0.io`;
const ICP_NET_ORIGIN = `https://${CANISTER_ID}.icp.net`;

test("should list the gateway twins as fallbacks for a remapped origin", () => {
  expect(appMetadataOrigins(LEGACY_ORIGIN)).toEqual([
    LEGACY_ORIGIN,
    ICP0_ORIGIN,
    ICP_NET_ORIGIN,
  ]);
});

test("should keep the `.raw` label when inverting the remap", () => {
  expect(appMetadataOrigins(`https://${CANISTER_ID}.raw.ic0.app`)).toEqual([
    `https://${CANISTER_ID}.raw.ic0.app`,
    `https://${CANISTER_ID}.raw.icp0.io`,
    `https://${CANISTER_ID}.raw.icp.net`,
  ]);
});

test("should not invent twins for a custom domain", () => {
  expect(appMetadataOrigins(ORIGIN)).toEqual([ORIGIN]);
});

test("should fall back to the icp0.io twin when ic0.app does not serve the canister", async () => {
  // What the staging test app does: the metadata is on `.icp0.io`, while
  // `.ic0.app` answers `400 client_domain_canister_mismatch`.
  const fetchMock = setupFetchMock(
    new Response("client_domain_canister_mismatch", { status: 400 }),
    Response.json({ name: "Example App" }),
  );

  expect(await fetchAppMetadata(LEGACY_ORIGIN)).toEqual({
    name: "Example App",
  });
  expect(fetchMock).toHaveBeenNthCalledWith(
    1,
    `${LEGACY_ORIGIN}${APP_METADATA_PATH}`,
    JSON_FETCH_OPTS,
  );
  expect(fetchMock).toHaveBeenNthCalledWith(
    2,
    `${ICP0_ORIGIN}${APP_METADATA_PATH}`,
    JSON_FETCH_OPTS,
  );
});

test("should fall back to the icp.net twin when neither ic0.app nor icp0.io serves the canister", async () => {
  const fetchMock = setupFetchMock(
    new Response("", { status: 400 }),
    new Response("", { status: 500 }),
    Response.json({ name: "Example App" }),
  );

  expect(await fetchAppMetadata(LEGACY_ORIGIN)).toEqual({
    name: "Example App",
  });
  expect(fetchMock).toHaveBeenCalledTimes(3);
  expect(fetchMock).toHaveBeenNthCalledWith(
    3,
    `${ICP_NET_ORIGIN}${APP_METADATA_PATH}`,
    JSON_FETCH_OPTS,
  );
});

test("should fall back to a twin when the remapped origin cannot be reached at all", async () => {
  // A CORS rejection or a network error surfaces as a rejected promise with no
  // status, which says nothing about whether the app publishes the document.
  const fetchMock = setupFetchMock(
    new TypeError("Failed to fetch"),
    Response.json({ name: "Example App" }),
  );

  expect(await fetchAppMetadata(LEGACY_ORIGIN)).toEqual({
    name: "Example App",
  });
  expect(fetchMock).toHaveBeenCalledTimes(2);
});

test("should not try the twins when the remapped origin answers 404", async () => {
  // All three domains resolve to the same canister, so a canister that answers
  // "no such file" has answered for every one of them. This keeps the common
  // case of an app that publishes nothing at one request.
  const fetchMock = setupFetchMock(new Response("", { status: 404 }));

  expect(await fetchAppMetadata(LEGACY_ORIGIN)).toBeUndefined();
  expect(fetchMock).toHaveBeenCalledExactlyOnceWith(
    `${LEGACY_ORIGIN}${APP_METADATA_PATH}`,
    JSON_FETCH_OPTS,
  );
});

test("should not try the twins when the remapped origin serves an invalid document", async () => {
  // The origin answered with a document; that it fails validation is its own
  // answer, not a reason to go looking for a different one.
  const fetchMock = setupFetchMock(Response.json({ name: "" }));

  expect(await fetchAppMetadata(LEGACY_ORIGIN)).toBeUndefined();
  expect(fetchMock).toHaveBeenCalledTimes(1);
});

test("should give up after every gateway origin has failed", async () => {
  const fetchMock = setupFetchMock(
    new Response("", { status: 400 }),
    new Response("", { status: 400 }),
    new Response("", { status: 400 }),
  );

  expect(await fetchAppMetadata(LEGACY_ORIGIN)).toBeUndefined();
  expect(fetchMock).toHaveBeenCalledTimes(3);
});

test("should resolve a twin's logo against that twin, not the remapped origin", async () => {
  const fetchMock = setupFetchMock(
    new Response("", { status: 400 }),
    Response.json({ name: "Example App", logo: "/assets/logo.png" }),
    imageResponse(),
  );
  setupImageMock();

  expect(await fetchAppMetadata(LEGACY_ORIGIN)).toEqual({
    name: "Example App",
    logo: LOGO_OBJECT_URL,
  });
  expect(fetchMock).toHaveBeenNthCalledWith(
    3,
    `${ICP0_ORIGIN}/assets/logo.png`,
    IMAGE_FETCH_OPTS,
  );
});

test("should reject a twin document whose logo points at the remapped origin", async () => {
  // The same-origin rule follows the document: the logo must live on whichever
  // origin served it, so a cross-origin reference is rejected as it would be
  // anywhere else.
  const fetchMock = setupFetchMock(
    new Response("", { status: 400 }),
    Response.json({
      name: "Example App",
      logo: `${LEGACY_ORIGIN}/assets/logo.png`,
    }),
  );

  expect(await fetchAppMetadata(LEGACY_ORIGIN)).toBeUndefined();
  expect(fetchMock).toHaveBeenCalledTimes(2);
});
