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

/** The MCP server's per-connect registration public key `X` (DER), from the
 *  link. The browser-signed final hop targets it — the canister never sees it. */
const REGISTRATION_KEY = new Uint8Array(44).fill(9);
/** `P_reg`'s DER public key, returned by the mocked prepare call — the chain's
 *  public key, and what makes the server's redemption `caller() == P_reg`. */
const USER_KEY = new Uint8Array(62).fill(3);

const expirationNanos = (): bigint =>
  BigInt(Date.now() + 60 * 60 * 1000) * BigInt(1_000_000);

/** Fake actor recording the exact candid arguments the flow sends. `prepare`
 *  returns `P_reg`'s key + an expiration; `get` echoes back a well-formed
 *  signed delegation for whatever key was requested (the browser-generated
 *  `Y`, unknown to the test up front) — the chain is only assembled and
 *  serialized on the frontend, never verified, so the signature needn't be
 *  real. */
const makeActor = () => {
  const expiration = expirationNanos();
  return {
    expiration,
    prepare_mcp_registration_delegation: vi
      .fn()
      .mockResolvedValue({ Ok: { user_key: USER_KEY, expiration } }),
    get_mcp_registration_delegation: vi.fn(
      (
        _anchor: bigint,
        requestedKey: Uint8Array,
        _permissions: [{ queries: null } | { all: null }],
        _maxTtl: [bigint],
        _expiration: bigint,
      ): Promise<
        | {
            Ok: {
              delegation: {
                pubkey: number[];
                expiration: bigint;
                targets: never[];
                permissions: never[];
              };
              signature: number[];
            };
          }
        | { Err: string }
      > =>
        Promise.resolve({
          Ok: {
            delegation: {
              pubkey: Array.from(requestedKey),
              expiration,
              targets: [],
              permissions: [],
            },
            signature: Array.from(new Uint8Array(64).fill(1)),
          },
        }),
    ),
  };
};

/** The browser-generated registration key `Y` the flow sent to `prepare` —
 *  readable only after the flow ran. */
const browserKeyOf = (actor: ReturnType<typeof makeActor>): Uint8Array =>
  actor.prepare_mcp_registration_delegation.mock.calls[0][1] as Uint8Array;

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

    // Minted for the identity and a browser-generated ephemeral key Y, with
    // the user-chosen grant TTL and the read-only access level (folded into
    // the derived registration principal, never into the signature).
    expect(actor.prepare_mcp_registration_delegation).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      expect.any(Uint8Array),
      [{ queries: null }],
      [BigInt(TTL_SECONDS) * BigInt(1_000_000_000)],
    );
  });

  it("never sends the link's key X to the canister", async () => {
    // The canister-signed hop must be inert to a transport-level observer:
    // it targets the browser-held Y, never the (attacker-craftable) link's X.
    const actor = makeActor();
    await authorize(actor, "read-only");

    const browserKey = browserKeyOf(actor);
    expect(browserKey.length).toBeGreaterThan(0);
    expect(toHex(browserKey)).not.toBe(toHex(REGISTRATION_KEY));
    const [, getKey] = actor.get_mcp_registration_delegation.mock.calls[0];
    expect(toHex(getKey)).toBe(toHex(browserKey));
  });

  it("generates a fresh Y per connect attempt", async () => {
    const first = makeActor();
    await authorize(first, "read-only");
    const second = makeActor();
    await authorize(second, "read-only");
    expect(toHex(browserKeyOf(first))).not.toBe(toHex(browserKeyOf(second)));
  });

  it("passes full access explicitly (never relies on the backend default)", async () => {
    const actor = makeActor();
    await authorize(actor, "full-access");

    expect(actor.prepare_mcp_registration_delegation).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      expect.any(Uint8Array),
      [{ all: null }],
      [BigInt(TTL_SECONDS) * BigInt(1_000_000_000)],
    );
  });

  it("fetches the certified delegation with the same consent parameters", async () => {
    const actor = makeActor();
    await authorize(actor, "read-only");

    // `get` re-derives the seed from its arguments (nothing is stored), so it
    // takes the exact consent tuple prepare folded, plus prepare's expiration.
    expect(actor.get_mcp_registration_delegation).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      browserKeyOf(actor),
      [{ queries: null }],
      [BigInt(TTL_SECONDS) * BigInt(1_000_000_000)],
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

  it("carries the consent tuple the server must echo to mcp_register_v2", async () => {
    // The derivation authenticates the echo (a tampered value fails to
    // redeem), but the server needs the plain values to echo: the anchor, the
    // access level as its wire string, and the grant TTL in nanoseconds.
    const url = await authorize(makeActor(), "read-only");

    const fragment = new URLSearchParams(url.slice(url.indexOf("#") + 1));
    expect(fragment.get("anchor")).toBe(IDENTITY_NUMBER.toString());
    expect(fragment.get("permissions")).toBe("queries");
    expect(fragment.get("ttl")).toBe(
      (BigInt(TTL_SECONDS) * BigInt(1_000_000_000)).toString(),
    );
  });

  it("marks a full-access connect as such in the consent tuple", async () => {
    const url = await authorize(makeActor(), "full-access");

    const fragment = new URLSearchParams(url.slice(url.indexOf("#") + 1));
    expect(fragment.get("permissions")).toBe("all");
  });

  it("carries a reconstructable two-hop P_reg -> Y -> X chain", async () => {
    const actor = makeActor();
    const url = await authorize(actor, "read-only");

    const fragment = new URLSearchParams(url.slice(url.indexOf("#") + 1));
    const delegation = fragment.get("delegation");
    if (delegation === null) throw new Error("no delegation in the fragment");

    // The server round-trips the chain out of the fragment exactly like this.
    const chain = DelegationChain.fromJSON(JSON.parse(delegation));
    // Its public key is P_reg's DER key, so the redemption is caller() == P_reg.
    expect(toHex(new Uint8Array(chain.publicKey))).toBe(toHex(USER_KEY));
    expect(chain.delegations).toHaveLength(2);
    const [canisterHop, browserHop] = chain.delegations;
    // Hop 1 (canister-signed) targets the browser-held Y...
    expect(toHex(new Uint8Array(canisterHop.delegation.pubkey))).toBe(
      toHex(browserKeyOf(actor)),
    );
    // ...hop 2 (browser-signed) targets the server's X, expiring no later
    // than the canister hop.
    expect(toHex(new Uint8Array(browserHop.delegation.pubkey))).toBe(
      toHex(REGISTRATION_KEY),
    );
    expect(
      browserHop.delegation.expiration <= canisterHop.delegation.expiration,
    ).toBe(true);
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
