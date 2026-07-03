import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { Authenticated } from "$lib/stores/authentication.store";
import { toBase64URL } from "$lib/utils/utils";
import { mcpAuthorize } from "./utils";

const IDENTITY_NUMBER = BigInt(42);
const MCP_ORIGIN = "https://mcp.id.ai";
const TTL_SECONDS = 3600;

const expirationNanos = (): bigint =>
  BigInt(Date.now() + 60 * 60 * 1000) * BigInt(1_000_000);

/** Fake actor recording the exact candid arguments the flow sends. */
const makeActor = () => {
  const expiration = expirationNanos();
  return {
    mcp_set_access: vi.fn().mockResolvedValue({ Ok: null }),
    prepare_account_delegation: vi.fn().mockResolvedValue({
      Ok: { user_key: new Uint8Array(32).fill(2), expiration },
    }),
    get_account_delegation: vi.fn().mockResolvedValue({
      Ok: {
        delegation: {
          pubkey: new Uint8Array(32).fill(3),
          expiration,
          targets: [],
          permissions: [],
        },
        signature: new Uint8Array(64),
      },
    }),
  };
};

const authorize = (
  actor: ReturnType<typeof makeActor>,
  accessLevel: "read-only" | "full-access",
) =>
  mcpAuthorize({
    authenticated: {
      identityNumber: IDENTITY_NUMBER,
      actor: actor as unknown as ActorSubclass<_SERVICE>,
    } as unknown as Authenticated,
    publicKey: toBase64URL(new Uint8Array(32).fill(4)),
    mcpServerOrigin: MCP_ORIGIN,
    ttlSeconds: TTL_SECONDS,
    accessLevel,
    callback: `${MCP_ORIGIN}/callback`,
    state: "opaque-state",
  });

beforeEach(() => {
  // jsdom does not implement form submission (the flow ends in a top-level
  // form-POST navigation); the canister arguments are what's under test.
  vi.spyOn(HTMLFormElement.prototype, "submit").mockImplementation(() => {});
});

afterEach(() => {
  vi.restoreAllMocks();
});

describe("mcpAuthorize access-level wiring", () => {
  it("persists a read-only grant but keeps the standing delegation full-access", async () => {
    const actor = makeActor();
    await authorize(actor, "read-only");

    // The grant carries the restriction...
    expect(actor.mcp_set_access).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      MCP_ORIGIN,
      true,
      [true],
    );
    // ...but the standing delegation must stay update-capable, or the MCP
    // server could never call the (update) prepare endpoint again.
    expect(actor.prepare_account_delegation).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      MCP_ORIGIN,
      [],
      expect.anything(),
      [BigInt(TTL_SECONDS) * BigInt(1_000_000_000)],
      [false],
    );
    expect(actor.get_account_delegation.mock.calls[0][5]).toEqual([false]);
  });

  it("persists a full-access grant explicitly (never relies on the backend default)", async () => {
    const actor = makeActor();
    await authorize(actor, "full-access");

    expect(actor.mcp_set_access).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      MCP_ORIGIN,
      true,
      [false],
    );
    expect(actor.prepare_account_delegation.mock.calls[0][5]).toEqual([false]);
  });
});
