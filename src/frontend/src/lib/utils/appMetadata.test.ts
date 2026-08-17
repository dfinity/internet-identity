import {
  APP_METADATA_PATH,
  MAX_APP_DESCRIPTION_LENGTH,
  MAX_APP_LOGO_SIZE,
  MAX_APP_METADATA_SIZE,
  MAX_APP_NAME_LENGTH,
  fetchAppMetadata,
} from "$lib/utils/appMetadata";
import { expect, test, vi } from "vitest";

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
const PNG_DATA_URL = `data:image/png;base64,${btoa(
  String.fromCharCode(...PNG_BYTES),
)}`;

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

test("should fetch a same-origin logo and inline it as a data url", async () => {
  const fetchMock = setupFetchMock(
    Response.json({ name: "Example App", logo: "/assets/logo.png" }),
    imageResponse(),
  );

  const result = await fetchAppMetadata(ORIGIN);

  expect(result).toEqual({
    name: "Example App",
    logo: PNG_DATA_URL,
  });
  expect(fetchMock).toHaveBeenNthCalledWith(1, METADATA_URL, JSON_FETCH_OPTS);
  expect(fetchMock).toHaveBeenNthCalledWith(
    2,
    `${ORIGIN}/assets/logo.png`,
    IMAGE_FETCH_OPTS,
  );
});

test("should resolve relative logo paths against the app origin", async () => {
  const fetchMock = setupFetchMock(
    Response.json({ logo: "logo.svg" }),
    imageResponse(PNG_BYTES, "image/svg+xml"),
  );

  const result = await fetchAppMetadata(ORIGIN);

  expect(result?.logo).toBe(
    `data:image/svg+xml;base64,${btoa(String.fromCharCode(...PNG_BYTES))}`,
  );
  expect(fetchMock).toHaveBeenNthCalledWith(
    2,
    `${ORIGIN}/logo.svg`,
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
  for (const body of [
    {},
    { unrelated: "field" },
    { name: 42, description: true, logo: [] },
    { name: "", description: "   " },
  ]) {
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

test("should drop text fields exceeding their length limits", async () => {
  setupFetchMock(
    Response.json({
      name: "a".repeat(MAX_APP_NAME_LENGTH + 1),
      description: "b".repeat(MAX_APP_DESCRIPTION_LENGTH),
    }),
  );

  const result = await fetchAppMetadata(ORIGIN);

  expect(result).toEqual({
    description: "b".repeat(MAX_APP_DESCRIPTION_LENGTH),
  });
});

test("should strip control and bidi characters and normalize whitespace", async () => {
  setupFetchMock(
    Response.json({
      name: "  Example \u0000\u202e \n\t App\u200f\u061c ",
      description: "Multi\r\nline\u2066   description\u0007 ",
    }),
  );

  const result = await fetchAppMetadata(ORIGIN);

  expect(result).toEqual({
    name: "Example App",
    description: "Multi line description",
  });
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

test("should drop logos that are not same-origin", async () => {
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

    const result = await fetchAppMetadata(ORIGIN);

    expect(result, logo).toEqual({ name: "Example App" });
    // The logo must not even be fetched.
    expect(fetchMock, logo).toHaveBeenCalledTimes(1);
  }
});

test("should drop logos served with a non-image content type", async () => {
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    imageResponse(PNG_BYTES, "text/html"),
  );

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

test("should accept content types regardless of casing and parameters", async () => {
  setupFetchMock(
    Response.json({ logo: "/logo.png" }),
    imageResponse(PNG_BYTES, "IMAGE/PNG; charset=binary"),
  );

  expect((await fetchAppMetadata(ORIGIN))?.logo).toBe(PNG_DATA_URL);
});

test("should drop logos exceeding the size limit", async () => {
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    imageResponse(new Uint8Array(MAX_APP_LOGO_SIZE + 1)),
  );

  expect(await fetchAppMetadata(ORIGIN)).toEqual({ name: "Example App" });
});

test("should drop empty logos", async () => {
  setupFetchMock(
    Response.json({ name: "Example App", logo: "/logo.png" }),
    imageResponse(new Uint8Array(0)),
  );

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
