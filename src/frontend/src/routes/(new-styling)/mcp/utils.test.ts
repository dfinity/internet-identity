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

  it("rejects a non-string finish_url before registering anything", async () => {
    // Present but not a string (e.g. a number or object) is a misbehaving
    // server, not an omitted optional — fail the connect up front.
    for (const finish_url of [42, { url: "https://mcp.id.ai" }, true]) {
      const actor = makeActor();
      stubFetch({ public_key: SERVER_PUBKEY, finish_url });
      await expect(authorize(actor, "read-only")).rejects.toThrow(
        /not a string/,
      );
      expect(actor.mcp_register).not.toHaveBeenCalled();
    }
  });
});

describe("mcpAuthorize failure paths", () => {
  it("rejects a key response without a usable public_key, registering nothing", async () => {
    // Missing key and non-string key are both a misbehaving (or wrong) server;
    // the connect must abort before anything binds to the identity.
    for (const keyBody of [
      {},
      { public_key: 42 },
      { public_key: null },
    ] as Record<string, unknown>[]) {
      const actor = makeActor();
      stubFetch(keyBody);
      await expect(authorize(actor, "read-only")).rejects.toThrow(
        /missing `public_key`/,
      );
      expect(actor.mcp_register).not.toHaveBeenCalled();
    }
  });

  it("propagates an mcp_register refusal and sends no completion POST", async () => {
    // A backend refusal (untrusted config, key bound to another identity, ...)
    // fails the connect: the error surfaces to the caller, and the server must
    // NOT be told the session is live — no completion POST follows the key
    // request.
    const actor = makeActor();
    actor.mcp_register.mockResolvedValue({
      Err: "MCP is not enabled for this identity",
    });

    await expect(authorize(actor, "full-access")).rejects.toThrow(
      /not enabled for this identity/,
    );
    // Exactly one callback fetch: the key request. No completion.
    expect(vi.mocked(fetch)).toHaveBeenCalledTimes(1);
  });

  it("still resolves (best effort) when the completion POST fails", async () => {
    // The completion notification is advisory: the session is already
    // registered, so a network failure on the POST must not fail the connect —
    // and a finish_url from the key response must still be handed back for
    // navigation.
    const finishUrl = `${MCP_ORIGIN}/oauth/finish?sid=xyz`;
    vi.stubGlobal(
      "fetch",
      vi.fn((_url: string, init?: RequestInit) => {
        const body = JSON.parse((init?.body as string) ?? "{}") as {
          expiration?: string;
        };
        // Key request succeeds; completion POST dies on the network.
        return body.expiration === undefined
          ? Promise.resolve(
              new Response(
                JSON.stringify({
                  public_key: SERVER_PUBKEY,
                  finish_url: finishUrl,
                }),
                { status: 200 },
              ),
            )
          : Promise.reject(new TypeError("network down"));
      }),
    );
    const actor = makeActor();

    await expect(authorize(actor, "read-only")).resolves.toBe(finishUrl);
    expect(actor.mcp_register).toHaveBeenCalledOnce();
    // Both fetches were attempted: key request + the failed completion.
    expect(vi.mocked(fetch)).toHaveBeenCalledTimes(2);
  });
});

// Two properties the server-side H3 mitigation ("Consent-Bound Completion")
// relies on from the II frontend. They hold today; these tests pin them so a
// future refactor of the connect flow can't silently regress them.
//   P1  — the key request is issued exactly once per connect and never
//         auto-retried (a single-use connect-state means a retry would either
//         brick the user or reopen the attacker race; recovery is a fresh
//         connect, not a retry).
//   P2  — finish_url (which carries the server's one-time finish_secret) is
//         never written to a log/telemetry sink by the frontend.
describe("H3 preconditions the fix depends on", () => {
  /** Callback POSTs that are key requests (no `expiration` in the body), as
   *  opposed to the completion notification (which carries `expiration`). */
  const keyRequestCount = (): number =>
    vi.mocked(fetch).mock.calls.filter(([, init]) => {
      const body = JSON.parse((init?.body as string) ?? "{}") as {
        expiration?: string;
      };
      return body.expiration === undefined;
    }).length;

  it("P1: issues exactly one key request per connect", async () => {
    await authorize(makeActor(), "read-only");
    expect(keyRequestCount()).toBe(1);
  });

  it("P1: does not retry the key request (or register) when the server rejects it", async () => {
    // A rejected key request aborts the connect; there is no retry loop, and
    // nothing is registered — the only recovery is a fresh /oauth/authorize
    // (new connect-state), never a retry of the same one.
    vi.stubGlobal(
      "fetch",
      vi.fn(() => Promise.resolve(new Response("", { status: 403 }))),
    );
    const actor = makeActor();

    await expect(authorize(actor, "read-only")).rejects.toThrow(
      /rejected the connect request/,
    );
    expect(vi.mocked(fetch)).toHaveBeenCalledTimes(1);
    expect(actor.mcp_register).not.toHaveBeenCalled();
  });

  it("P2: never writes finish_url (which carries finish_secret) to the console", async () => {
    const SECRET = "fs-DO-NOT-LOG-9f83k2";
    stubFetch({
      public_key: SERVER_PUBKEY,
      finish_url: `${MCP_ORIGIN}/oauth/finish?fs=${SECRET}`,
    });
    const methods = ["log", "info", "warn", "error", "debug"] as const;
    const spies = methods.map((m) =>
      vi.spyOn(console, m).mockImplementation(() => undefined),
    );

    // Returns the finish_url to the caller (which navigates to it) — but must
    // not emit it to any console sink along the way.
    await expect(authorize(makeActor(), "read-only")).resolves.toContain(
      SECRET,
    );
    const logged = spies.flatMap((s) => s.mock.calls.flat()).join(" ");
    expect(logged).not.toContain(SECRET);
  });
});
