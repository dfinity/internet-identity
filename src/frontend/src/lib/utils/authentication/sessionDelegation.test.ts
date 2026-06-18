import { describe, it, expect, vi, beforeEach } from "vitest";
import {
  ECDSAKeyIdentity,
  DelegationChain,
  Delegation,
  DelegationIdentity,
} from "@icp-sdk/core/identity";
import type { Signature, ActorSubclass } from "@icp-sdk/core/agent";
import { Principal } from "@icp-sdk/core/principal";
import type { _SERVICE } from "$lib/generated/internet_identity_types";

vi.mock("$app/environment", () => ({ browser: true }));

const nowMs = Date.now();
const EXPIRATION_NS = BigInt(nowMs + 7 * 24 * 3600 * 1000) * BigInt(1_000_000);

const makeSignedDelegation = (pubkey: Uint8Array, expiration: bigint) => ({
  Ok: {
    delegation: {
      pubkey: Array.from(pubkey),
      expiration,
      targets: [] as [],
    },
    signature: Array.from(new Uint8Array(64)),
  },
});

const prepareMock = vi.fn().mockResolvedValue({
  Ok: {
    user_key: Array.from(new Uint8Array(32).fill(1)),
    expiration: EXPIRATION_NS,
  },
});

const getDelegationMock = vi
  .fn()
  .mockImplementation((_anchor, sessionKey, expiration) =>
    Promise.resolve(
      makeSignedDelegation(new Uint8Array(sessionKey), expiration),
    ),
  );

const mockActor = {
  prepare_session_delegation: prepareMock,
  get_session_delegation: getDelegationMock,
} as unknown as ActorSubclass<_SERVICE>;

describe("mintSessionDelegation", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    prepareMock.mockResolvedValue({
      Ok: {
        user_key: Array.from(new Uint8Array(32).fill(1)),
        expiration: EXPIRATION_NS,
      },
    });
    getDelegationMock.mockImplementation((_anchor, sessionKey, expiration) =>
      Promise.resolve(
        makeSignedDelegation(new Uint8Array(sessionKey), expiration),
      ),
    );
  });

  it("returns a record with key pair, chain JSON, and expiry", async () => {
    const { mintSessionDelegation } =
      await import("$lib/utils/authentication/sessionDelegation");

    const record = await mintSessionDelegation({
      identityNumber: BigInt(123),
      actor: mockActor,
    });

    expect(record.identityNumber).toBe(BigInt(123));
    expect(typeof record.chainJson).toBe("string");
    expect(record.keyPair.privateKey).toBeDefined();
    expect(record.keyPair.publicKey).toBeDefined();
  });

  it("generates a non-extractable private key (public key is always extractable per WebCrypto spec)", async () => {
    const { mintSessionDelegation } =
      await import("$lib/utils/authentication/sessionDelegation");

    const record = await mintSessionDelegation({
      identityNumber: BigInt(123),
      actor: mockActor,
    });

    expect(record.keyPair.privateKey.extractable).toBe(false);
    expect(record.keyPair.publicKey.extractable).toBe(true);
  });

  it("calls prepare_session_delegation with the identity number, session key, and default max_ttl", async () => {
    const { mintSessionDelegation } =
      await import("$lib/utils/authentication/sessionDelegation");

    await mintSessionDelegation({
      identityNumber: BigInt(55),
      actor: mockActor,
    });

    expect(prepareMock).toHaveBeenCalledWith(
      BigInt(55),
      expect.any(Uint8Array),
      [],
    );
  });

  it("calls prepare_session_delegation on the provided actor, not a fresh one", async () => {
    const { mintSessionDelegation } =
      await import("$lib/utils/authentication/sessionDelegation");

    await mintSessionDelegation({
      identityNumber: BigInt(55),
      actor: mockActor,
    });

    expect(prepareMock).toHaveBeenCalledTimes(1);
    expect(getDelegationMock).toHaveBeenCalledTimes(1);
  });

  it("converts expiration from nanoseconds to milliseconds without precision loss", async () => {
    const { mintSessionDelegation } =
      await import("$lib/utils/authentication/sessionDelegation");

    const record = await mintSessionDelegation({
      identityNumber: BigInt(1),
      actor: mockActor,
    });

    const expectedMs = Number(EXPIRATION_NS / BigInt(1_000_000));
    expect(record.expiresAtMillis).toBe(expectedMs);
  });

  it("assembles the chain from the returned signed delegation and user_key", async () => {
    const { mintSessionDelegation } =
      await import("$lib/utils/authentication/sessionDelegation");

    const record = await mintSessionDelegation({
      identityNumber: BigInt(7),
      actor: mockActor,
    });

    const parsed = JSON.parse(record.chainJson);
    expect(Array.isArray(parsed.delegations)).toBe(true);
    expect(parsed.delegations).toHaveLength(1);
  });
});

describe("sessionDelegationIdentity", () => {
  it("rebuilds a DelegationIdentity from a stored key pair and chain JSON", async () => {
    const { sessionDelegationIdentity } =
      await import("$lib/utils/authentication/sessionDelegation");

    const keyIdentity = await ECDSAKeyIdentity.generate({ extractable: false });
    const keyPair = keyIdentity.getKeyPair();

    const chain = DelegationChain.fromDelegations(
      [
        {
          delegation: new Delegation(
            new Uint8Array(32).fill(2),
            BigInt(Date.now() + 3_600_000) * BigInt(1_000_000),
          ),
          signature: new Uint8Array(64) as unknown as Signature,
        },
      ],
      new Uint8Array(32).fill(3),
    );

    const identity = await sessionDelegationIdentity(
      keyPair,
      JSON.stringify(chain.toJSON()),
    );

    expect(identity).toBeInstanceOf(DelegationIdentity);
    expect(identity.getPrincipal().toText()).not.toBe(
      Principal.anonymous().toText(),
    );
  });
});
