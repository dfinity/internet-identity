import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { Authenticated } from "$lib/stores/authentication.store";
import { toBase64URL } from "$lib/utils/utils";
import { cliAuthorize, CLI_GENERIC_DERIVATION_ORIGIN } from "./utils";

const IDENTITY_NUMBER = BigInt(42);
const TTL_MINUTES = 30;

const expirationNanos = (): bigint =>
  BigInt(Date.now() + 60 * 60 * 1000) * BigInt(1_000_000);

/** Fake actor recording the exact candid arguments the flow sends. */
const makeActor = () => {
  const expiration = expirationNanos();
  return {
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
  cliAuthorize({
    authenticated: {
      identityNumber: IDENTITY_NUMBER,
      actor: actor as unknown as ActorSubclass<_SERVICE>,
    } as unknown as Authenticated,
    publicKey: toBase64URL(new Uint8Array(32).fill(4)),
    domain: undefined,
    ttlMinutes: TTL_MINUTES,
    callback: "http://127.0.0.1:8000/callback",
    nonce: "test-nonce",
    accessLevel,
  });

beforeEach(() => {
  // jsdom does not implement form submission (the flow ends in a top-level
  // form-POST navigation); the canister arguments are what's under test.
  vi.spyOn(HTMLFormElement.prototype, "submit").mockImplementation(() => {});
});

afterEach(() => {
  vi.restoreAllMocks();
});

describe("cliAuthorize access-level wiring", () => {
  it("requests a queries-only delegation for read-only access", async () => {
    const actor = makeActor();
    await authorize(actor, "read-only");

    expect(actor.prepare_account_delegation).toHaveBeenCalledWith(
      IDENTITY_NUMBER,
      CLI_GENERIC_DERIVATION_ORIGIN,
      [],
      expect.anything(),
      [BigInt(TTL_MINUTES) * BigInt(60) * BigInt(1_000_000_000)],
      [true],
    );
    // `get` must echo the same value or the signature lookup fails.
    expect(actor.get_account_delegation.mock.calls[0][5]).toEqual([true]);
  });

  it("requests an unrestricted delegation for full access, explicitly", async () => {
    const actor = makeActor();
    await authorize(actor, "full-access");

    expect(actor.prepare_account_delegation.mock.calls[0][5]).toEqual([false]);
    expect(actor.get_account_delegation.mock.calls[0][5]).toEqual([false]);
  });
});
