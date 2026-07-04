import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { Authenticated } from "$lib/stores/authentication.store";
import { fromBase64URL, toBase64URL } from "$lib/utils/utils";
import { mcpAuthorize } from "./utils";

const IDENTITY_NUMBER = BigInt(42);
const MCP_ORIGIN = "https://mcp.id.ai";
const CALLBACK = `${MCP_ORIGIN}/callback`;
const TTL_SECONDS = 3600;
/** The server's session public key, returned by the mocked key-request fetch. */
const SERVER_PUBKEY = toBase64URL(new Uint8Array(32).fill(7));

const expirationNanos = (): bigint =>
  BigInt(Date.now() + 60 * 60 * 1000) * BigInt(1_000_000);

/** Fake actor recording the exact candid arguments the flow sends. */
const makeActor = () => ({
  mcp_register: vi
    .fn()
    .mockResolvedValue({ Ok: { expiration: expirationNanos() } }),
});

const authorize = (
  actor: ReturnType<typeof makeActor>,
  accessLevel: "read-only" | "full-access",
) =>
  mcpAuthorize({
    authenticated: {
      identityNumber: IDENTITY_NUMBER,
      actor: actor as unknown as ActorSubclass<_SERVICE>,
    } as unknown as Authenticated,
    ttlSeconds: TTL_SECONDS,
    accessLevel,
    callback: CALLBACK,
    state: "opaque-state",
  });

/** Mocks the two callback requests of the connect flow: the key request (no
 *  `expiration` in the body) answers with `keyBody`; the completion
 *  notification (carries `expiration`) just succeeds. The canister arguments
 *  to `mcp_register` — and the returned `finish_url` — are what's under test. */
const stubFetch = (keyBody: Record<string, unknown>): void => {
  vi.stubGlobal(
    "fetch",
    vi.fn((_url: string, init?: RequestInit) => {
      const body = JSON.parse((init?.body as string) ?? "{}") as {
        expiration?: string;
      };
      const response =
        body.expiration !== undefined
          ? new Response("{}", { status: 200 })
          : new Response(JSON.stringify(keyBody), { status: 200 });
      return Promise.resolve(response);
    }),
  );
};

beforeEach(() => {
  // Default stand-in server: answers the key request with its pubkey and no
  // `finish_url`. Tests exercising `finish_url` re-stub with one included.
  stubFetch({ public_key: SERVER_PUBKEY });
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("mcpAuthorize access-level wiring", () => {
  it("registers the session queries-only when read-only", async () => {
    const actor = makeActor();
    await authorize(actor, "read-only");

    // The session key comes from the trusted server's callback (not the link),
    // and read-only is persisted on the grant via the permissions argument.
    expect(actor.mcp_register).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      fromBase64URL(SERVER_PUBKEY),
      BigInt(TTL_SECONDS) * BigInt(1_000_000_000),
      [{ queries: null }],
    );
  });

  it("registers full access explicitly (never relies on the backend default)", async () => {
    const actor = makeActor();
    await authorize(actor, "full-access");

    expect(actor.mcp_register).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      fromBase64URL(SERVER_PUBKEY),
      BigInt(TTL_SECONDS) * BigInt(1_000_000_000),
      [{ all: null }],
    );
  });
});

describe("mcpAuthorize completion notification", () => {
  /** The body of the completion POST (the callback fetch that carries
   *  `expiration`), parsed from the recorded fetch calls. */
  const completionBody = (): { expiration: string; permissions: string } => {
    const call = vi
      .mocked(fetch)
      .mock.calls.find(([, init]) =>
        ((init?.body as string) ?? "").includes("expiration"),
      );
    if (call === undefined) throw new Error("no completion POST was sent");
    return JSON.parse(call[1]?.body as string);
  };

  it("reports the access level so the server learns read-only up front", async () => {
    await authorize(makeActor(), "read-only");
    expect(completionBody().permissions).toBe("queries");
  });

  it("reports full access as `all`", async () => {
    await authorize(makeActor(), "full-access");
    expect(completionBody().permissions).toBe("all");
  });
});

describe("mcpAuthorize finish_url handling", () => {
  it("resolves undefined when the server sends no finish_url", async () => {
    await expect(authorize(makeActor(), "read-only")).resolves.toBeUndefined();
  });

  it("treats a null or empty finish_url as absent (common optional serializations)", async () => {
    // serde `Option::None`, Go string zero-values, and Python `None` all
    // serialize an omitted optional as null (or ""): those connects must
    // succeed without a redirect, not hard-fail.
    for (const finish_url of [null, ""]) {
      const actor = makeActor();
      stubFetch({ public_key: SERVER_PUBKEY, finish_url });
      await expect(authorize(actor, "read-only")).resolves.toBeUndefined();
      expect(actor.mcp_register).toHaveBeenCalledOnce();
    }
  });

  it("returns the finish_url when it is on the callback's origin", async () => {
    // The finish redirect lets the server complete a flow of its own (e.g.
    // hand an OAuth code to an MCP client) after the session is registered.
    const finishUrl = `${MCP_ORIGIN}/oauth/finish?sid=abc123`;
    stubFetch({ public_key: SERVER_PUBKEY, finish_url: finishUrl });
    const actor = makeActor();

    await expect(authorize(actor, "read-only")).resolves.toBe(finishUrl);
    // The session was registered and the completion notification still sent
    // (key request + completion = two callback fetches) before any navigation.
    expect(actor.mcp_register).toHaveBeenCalledOnce();
    expect(vi.mocked(fetch)).toHaveBeenCalledTimes(2);
  });

  it("rejects a cross-origin finish_url before registering anything", async () => {
    // II only ever navigates to the trusted origin: a finish_url elsewhere
    // fails the connect — and fails it up front, so no session materializes.
    stubFetch({
      public_key: SERVER_PUBKEY,
      finish_url: "https://evil.example.com/finish",
    });
    const actor = makeActor();

    await expect(authorize(actor, "read-only")).rejects.toThrow(
      /not on the server's own origin/,
    );
    expect(actor.mcp_register).not.toHaveBeenCalled();
  });

  it("rejects a plain-http finish_url on the same host", async () => {
    // Origin comparison includes the scheme: the same host over http is a
    // different origin than the https callback. This pins the https-only
    // invariant against refactors that would compare hostnames instead.
    stubFetch({
      public_key: SERVER_PUBKEY,
      finish_url: `http://${new URL(MCP_ORIGIN).host}/finish`,
    });
    const actor = makeActor();

    await expect(authorize(actor, "read-only")).rejects.toThrow(
      /not on the server's own origin/,
    );
    expect(actor.mcp_register).not.toHaveBeenCalled();
  });

  it("rejects a scheme with no comparable origin (javascript:)", async () => {
    // javascript:/data: URLs parse but have an opaque ("null") origin, which
    // can never equal the https callback's origin.
    stubFetch({
      public_key: SERVER_PUBKEY,
      finish_url: "javascript:alert(1)",
    });
    const actor = makeActor();

    await expect(authorize(actor, "read-only")).rejects.toThrow(
      /not on the server's own origin/,
    );
    expect(actor.mcp_register).not.toHaveBeenCalled();
  });

  it("rejects a malformed finish_url before registering anything", async () => {
    // Relative paths included: finish_url must be an absolute URL.
    stubFetch({ public_key: SERVER_PUBKEY, finish_url: "/oauth/finish" });
    const actor = makeActor();

    await expect(authorize(actor, "read-only")).rejects.toThrow(
      /not a valid URL/,
    );
    expect(actor.mcp_register).not.toHaveBeenCalled();
  });
});
