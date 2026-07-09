import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { Authenticated } from "$lib/stores/authentication.store";
import { DelegationChain } from "@icp-sdk/core/identity";
import { toHex } from "$lib/utils/utils";
import { mcpAuthorize } from "./utils";

const IDENTITY_NUMBER = BigInt(42);
const MCP_ORIGIN = "https://mcp.id.ai";
/** The trusted server's declared connect callback (exact-matched by the caller
 *  against the server's /.well-known/ii-auth-callbacks allow-list): where the
 *  caller delivers the registration delegation. */
const CALLBACK = `${MCP_ORIGIN}/mcp/connect`;
const STATE = "opaque-state";
const TTL_SECONDS = 3600;

/** The MCP server's per-connect registration public key `X` (DER). II mints a
 *  `P_reg -> X` delegation for it — nothing secret rides this. */
const REGISTRATION_KEY = new Uint8Array(44).fill(9);
/** `P_reg`'s DER public key, returned by the mocked prepare call — the chain's
 *  public key, and what makes the server's redemption `caller() == P_reg`. */
const USER_KEY = new Uint8Array(62).fill(3);

const expirationNanos = (): bigint =>
  BigInt(Date.now() + 60 * 60 * 1000) * BigInt(1_000_000);

/** Fake actor recording the exact candid arguments the flow sends. `prepare`
 *  returns `P_reg`'s key + an expiration; `get` returns a well-formed (but
 *  arbitrary) signed `P_reg -> X` delegation — the chain is only assembled and
 *  serialized on the frontend, never verified, so the bytes needn't be real. */
const makeActor = () => {
  const expiration = expirationNanos();
  return {
    expiration,
    prepare_mcp_registration_delegation: vi
      .fn()
      .mockResolvedValue({ Ok: { user_key: USER_KEY, expiration } }),
    get_mcp_registration_delegation: vi.fn().mockResolvedValue({
      Ok: {
        delegation: {
          pubkey: Array.from(REGISTRATION_KEY),
          expiration,
          targets: [],
          permissions: [],
        },
        signature: Array.from(new Uint8Array(64).fill(1)),
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
    ttlSeconds: TTL_SECONDS,
    accessLevel,
    callback: CALLBACK,
    state: STATE,
    registrationKey: REGISTRATION_KEY,
  });

beforeEach(() => {
  // The v2 connect makes no network calls of its own — delivery is by
  // navigation. Stub fetch so any accidental call is observable (and asserted
  // against below), rather than hitting the real network.
  vi.stubGlobal("fetch", vi.fn());
});

afterEach(() => {
  vi.unstubAllGlobals();
  vi.restoreAllMocks();
});

describe("mcpAuthorize registration-delegation minting", () => {
  it("prepares the delegation for the server's key with the chosen TTL and read-only", async () => {
    const actor = makeActor();
    await authorize(actor, "read-only");

    // Minted for the identity and the server's registration key, with the
    // user-chosen grant TTL and the read-only access level (recorded on the
    // index entry by the backend, never folded into the signature).
    expect(actor.prepare_mcp_registration_delegation).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      REGISTRATION_KEY,
      [{ queries: null }],
      [BigInt(TTL_SECONDS) * BigInt(1_000_000_000)],
    );
  });

  it("passes full access explicitly (never relies on the backend default)", async () => {
    const actor = makeActor();
    await authorize(actor, "full-access");

    expect(actor.prepare_mcp_registration_delegation).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      REGISTRATION_KEY,
      [{ all: null }],
      [BigInt(TTL_SECONDS) * BigInt(1_000_000_000)],
    );
  });

  it("fetches the certified delegation for the expiration prepare returned", async () => {
    const actor = makeActor();
    await authorize(actor, "read-only");

    expect(actor.get_mcp_registration_delegation).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      REGISTRATION_KEY,
      actor.expiration,
    );
  });

  it("makes no network request of its own (delivery is by navigation)", async () => {
    await authorize(makeActor(), "read-only");
    expect(vi.mocked(fetch)).not.toHaveBeenCalled();
  });
});

describe("mcpAuthorize delivery URL", () => {
  it("targets the declared callback and carries the state echo", async () => {
    const url = await authorize(makeActor(), "read-only");

    expect(url.startsWith(`${CALLBACK}#`)).toBe(true);
    const fragment = new URLSearchParams(url.slice(url.indexOf("#") + 1));
    expect(fragment.get("state")).toBe(STATE);
  });

  it("carries a reconstructable P_reg -> X delegation chain", async () => {
    const url = await authorize(makeActor(), "read-only");

    const fragment = new URLSearchParams(url.slice(url.indexOf("#") + 1));
    const delegation = fragment.get("delegation");
    if (delegation === null) throw new Error("no delegation in the fragment");

    // The server round-trips the chain out of the fragment exactly like this.
    const chain = DelegationChain.fromJSON(JSON.parse(delegation));
    // Its public key is P_reg's DER key, so the redemption is caller() == P_reg.
    expect(toHex(new Uint8Array(chain.publicKey))).toBe(toHex(USER_KEY));
    expect(chain.delegations).toHaveLength(1);
    // The delegated-to key is the server's registration key X.
    expect(toHex(new Uint8Array(chain.delegations[0].delegation.pubkey))).toBe(
      toHex(REGISTRATION_KEY),
    );
  });
});

describe("mcpAuthorize failure paths", () => {
  it("propagates a prepare refusal and never fetches the delegation", async () => {
    const actor = makeActor();
    actor.prepare_mcp_registration_delegation.mockResolvedValue({
      Err: "MCP registration failed: 2vxsx-fae could not be authenticated.",
    });

    await expect(authorize(actor, "full-access")).rejects.toThrow(
      /could not be authenticated/,
    );
    // Aborted before the certified delegation is even fetched.
    expect(actor.get_mcp_registration_delegation).not.toHaveBeenCalled();
  });

  it("propagates a get refusal", async () => {
    const actor = makeActor();
    actor.get_mcp_registration_delegation.mockResolvedValue({
      Err: "MCP registration failed: no such delegation.",
    });

    await expect(authorize(actor, "read-only")).rejects.toThrow(
      /no such delegation/,
    );
  });
});
