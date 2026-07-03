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

beforeEach(() => {
  // The connect flow fetches the server's session key from its callback, then
  // POSTs a completion notification. Mock both: the key request (no
  // `expiration` in the body) returns the server's pubkey; the completion
  // (carries `expiration`) just succeeds. The canister arguments to
  // `mcp_register` are what's under test.
  vi.stubGlobal(
    "fetch",
    vi.fn((_url: string, init?: RequestInit) => {
      const body = JSON.parse((init?.body as string) ?? "{}") as {
        expiration?: string;
      };
      const response =
        body.expiration !== undefined
          ? new Response("{}", { status: 200 })
          : new Response(JSON.stringify({ public_key: SERVER_PUBKEY }), {
              status: 200,
            });
      return Promise.resolve(response);
    }),
  );
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
