import { describe, it, expect, vi, afterEach } from "vitest";
import {
  AUTH_CALLBACKS_PATH,
  AUTH_CALLBACKS_MAX_SIZE,
  matchDeclaredCallback,
} from "./authCallbacks";

const ORIGIN = "https://mcp.example.com";
const CALLBACK = `${ORIGIN}/mcp/connect`;

/** Stubs the allow-list fetch. By default: 200, application/json, a list
 *  declaring `CALLBACK`. Every knob a test needs to misbehave is a param. */
const stubFetch = ({
  status = 200,
  contentType = "application/json",
  body = JSON.stringify({ callbacks: [CALLBACK] }),
  headers = {},
}: {
  status?: number;
  contentType?: string;
  body?: string;
  headers?: Record<string, string>;
} = {}): void => {
  vi.stubGlobal(
    "fetch",
    vi.fn(() =>
      Promise.resolve(
        new Response(body, {
          status,
          headers: { "content-type": contentType, ...headers },
        }),
      ),
    ),
  );
};

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("matchDeclaredCallback", () => {
  it("resolves with the callback when the server declares it", async () => {
    stubFetch();
    await expect(matchDeclaredCallback(ORIGIN, CALLBACK)).resolves.toBe(
      CALLBACK,
    );
  });

  it("fetches the fixed well-known path, redirect-refusing, credential- and cache-free", async () => {
    // The transport posture is the security boundary: an open redirect at the
    // well-known path must not let a third party serve the list, no ambient
    // credentials ride the GET, and the match is never against a stale copy.
    stubFetch();
    await matchDeclaredCallback(ORIGIN, CALLBACK);

    expect(vi.mocked(fetch)).toHaveBeenCalledWith(
      `${ORIGIN}${AUTH_CALLBACKS_PATH}`,
      expect.objectContaining({
        redirect: "error",
        credentials: "omit",
        cache: "no-store",
      }),
    );
  });

  it("rejects a callback the server does not declare", async () => {
    stubFetch();
    await expect(
      matchDeclaredCallback(ORIGIN, `${ORIGIN}/attacker-echo`),
    ).rejects.toThrow(/does not declare this callback/);
  });

  it("matches exactly — no prefix, suffix, or case slack", async () => {
    stubFetch();
    for (const near of [
      `${CALLBACK}/`,
      `${CALLBACK}extra`,
      `${ORIGIN}/mcp`,
      CALLBACK.toUpperCase(),
      `${CALLBACK}?x=1`,
    ]) {
      await expect(matchDeclaredCallback(ORIGIN, near)).rejects.toThrow(
        /does not declare this callback/,
      );
    }
  });

  it("rejects a declared cross-origin callback rather than honouring it", async () => {
    // Even when the list blesses it, an off-origin entry must never be
    // honoured — nothing ships off the trusted origin.
    const foreign = "https://evil.example.com/connect";
    stubFetch({ body: JSON.stringify({ callbacks: [foreign] }) });
    await expect(matchDeclaredCallback(ORIGIN, foreign)).rejects.toThrow(
      /not on the server's origin/,
    );
  });

  it("rejects a declared fragment-carrying callback", async () => {
    // The connect flow appends its own fragment; a declared one is a
    // misconfiguration at best.
    const withFragment = `${ORIGIN}/connect#frag`;
    stubFetch({ body: JSON.stringify({ callbacks: [withFragment] }) });
    await expect(matchDeclaredCallback(ORIGIN, withFragment)).rejects.toThrow(
      /must not carry a fragment/,
    );
  });

  it("rejects an HTTP error (no allow-list served)", async () => {
    stubFetch({ status: 404 });
    await expect(matchDeclaredCallback(ORIGIN, CALLBACK)).rejects.toThrow(
      /could not be fetched \(HTTP 404\)/,
    );
  });

  it("rejects a non-JSON content type", async () => {
    // A reflecting or error page at the well-known path must not be parsed
    // into an allow-list.
    stubFetch({ contentType: "text/html" });
    await expect(matchDeclaredCallback(ORIGIN, CALLBACK)).rejects.toThrow(
      /is not JSON/,
    );
  });

  it("rejects an oversized document", async () => {
    const padded = JSON.stringify({
      callbacks: [CALLBACK],
      pad: "x".repeat(AUTH_CALLBACKS_MAX_SIZE),
    });
    stubFetch({ body: padded });
    await expect(matchDeclaredCallback(ORIGIN, CALLBACK)).rejects.toThrow(
      /too large/,
    );
  });

  it("rejects an oversize declared Content-Length before reading the body", async () => {
    // The cap must not require buffering the body to be enforced: a declared
    // oversize length is rejected up front (and a body without one is capped
    // while streaming — the previous test's path).
    stubFetch({
      headers: { "content-length": String(AUTH_CALLBACKS_MAX_SIZE + 1) },
    });
    await expect(matchDeclaredCallback(ORIGIN, CALLBACK)).rejects.toThrow(
      /too large/,
    );
  });

  it("rejects malformed JSON and a missing/non-array `callbacks`", async () => {
    stubFetch({ body: "not json {" });
    await expect(matchDeclaredCallback(ORIGIN, CALLBACK)).rejects.toThrow(
      /not valid JSON/,
    );

    for (const body of [
      JSON.stringify({}),
      JSON.stringify({ callbacks: "not-an-array" }),
      JSON.stringify(null),
      JSON.stringify([CALLBACK]),
    ]) {
      stubFetch({ body });
      await expect(matchDeclaredCallback(ORIGIN, CALLBACK)).rejects.toThrow(
        /missing `callbacks`/,
      );
    }
  });

  it("propagates a network/redirect failure (fail-closed)", async () => {
    // `redirect: "error"` surfaces as a rejected fetch; so does any network
    // failure. Either way the flow fails before anything is delivered.
    vi.stubGlobal(
      "fetch",
      vi.fn(() => Promise.reject(new TypeError("Failed to fetch"))),
    );
    await expect(matchDeclaredCallback(ORIGIN, CALLBACK)).rejects.toThrow(
      /Failed to fetch/,
    );
  });
});
