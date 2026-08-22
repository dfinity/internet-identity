import { beforeEach, describe, expect, it, vi } from "vitest";
import "fake-indexeddb/auto";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";

const CANISTER_ID_TEXT = "rwlgt-iiaaa-aaaaa-aaaaa-cai";
const ORIGIN = "https://app.example.com";

vi.mock("$lib/globals", async () => {
  const { Principal } = await import("@icp-sdk/core/principal");
  return {
    canisterId: Principal.fromText("rwlgt-iiaaa-aaaaa-aaaaa-cai"),
    backendCanisterConfig: { openid_configs: [] },
    frontendCanisterConfig: { related_origins: [], dev_csp: [] },
  };
});
vi.mock("$lib/utils/validateDerivationOrigin", () => ({
  validateDerivationOrigin: vi.fn(() => Promise.resolve({ result: "valid" })),
}));
vi.mock("$lib/utils/iiConnection", () => ({
  remapToLegacyDomain: (origin: string) => origin,
}));

const setRequestContext = vi.fn();
vi.mock("$lib/stores/authorization.store", () => ({
  authorizationStore: {
    setRequestContext: (...args: unknown[]) => setRequestContext(...args),
  },
  authorizedStore: { subscribe: () => () => {} },
}));

import { handleSessionDelegationRequest } from "./sessionDelegation";
import {
  purgeAppSessions,
  storeAppSession,
} from "$lib/stores/app-session.store";

const channelWith = () => {
  const sent: unknown[] = [];
  return {
    channel: {
      origin: ORIGIN,
      closed: false,
      resumeToken: "token",
      addEventListener: () => () => {},
      send: (response: unknown) => {
        sent.push(response);
        return Promise.resolve();
      },
      close: async () => {},
    },
    sent,
  };
};

const storedSession = async (identityNumber: bigint) => {
  const key = await ECDSAKeyIdentity.generate({ extractable: false });
  const root = await ECDSAKeyIdentity.generate({ extractable: true });
  const chain = await DelegationChain.create(
    root,
    key.getPublicKey(),
    new Date(Date.now() + 60 * 60 * 1000),
  );
  await storeAppSession(
    { identityNumber, origin: ORIGIN },
    {
      keyPair: key.getKeyPair(),
      chainJson: JSON.stringify(chain.toJSON()),
      expiresAtMillis: Date.now() + 60 * 60 * 1000,
      createdAtNanos: BigInt(1_000),
      accessLevel: "full-access" as const,
      accountPrincipal: "2vxsx-fae",
    },
  );
};

const appKey = async () => {
  const identity = await ECDSAKeyIdentity.generate({ extractable: true });
  const der = new Uint8Array(identity.getPublicKey().toDer());
  return btoa(String.fromCharCode(...der));
};

describe("ii_session_delegation", () => {
  beforeEach(async () => {
    setRequestContext.mockClear();
    await purgeAppSessions(BigInt(10_000));
    await purgeAppSessions(BigInt(10_001));
  });

  it("ignores a request for another method", async () => {
    const { channel, sent } = channelWith();
    const onError = vi.fn();

    await handleSessionDelegationRequest(
      channel,
      onError,
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "icrc34_delegation",
    });

    expect(sent).toEqual([]);
    expect(onError).not.toHaveBeenCalled();
  });

  it("rejects params that carry no session key", async () => {
    const { channel, sent } = channelWith();
    const onError = vi.fn();

    await handleSessionDelegationRequest(
      channel,
      onError,
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: {},
    });

    expect(sent).toHaveLength(1);
    expect(sent[0]).toMatchObject({ id: 1, error: { code: -32602 } });
    expect(onError).toHaveBeenCalledWith("invalid-request");
  });

  it("re-issues from a held session without a ceremony", async () => {
    await storedSession(BigInt(10_000));
    const { channel, sent } = channelWith();
    const onError = vi.fn();

    await handleSessionDelegationRequest(
      channel,
      onError,
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: { sessionPublicKey: await appKey() },
    });

    expect(onError).not.toHaveBeenCalled();
    expect(setRequestContext).not.toHaveBeenCalled();
    expect(sent).toHaveLength(1);
    const result = (sent[0] as { result: Record<string, unknown> }).result;
    // The chain is the whole answer: nothing else travels with it, and nothing has to be
    // attached to the calls the app makes with it.
    expect(Object.keys(result).sort()).toEqual([
      "publicKey",
      "signerDelegation",
    ]);
  });

  it("restricts the session chain to the II canister", async () => {
    await storedSession(BigInt(10_000));
    const { channel, sent } = channelWith();

    await handleSessionDelegationRequest(
      channel,
      vi.fn(),
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: { sessionPublicKey: await appKey() },
    });

    const result = (
      sent[0] as {
        result: { signerDelegation: { delegation: { targets?: string[] } }[] };
      }
    ).result;
    const targets = result.signerDelegation
      .map((signed) => signed.delegation.targets)
      .filter((value): value is string[] => value !== undefined);
    expect(targets).toEqual([[CANISTER_ID_TEXT]]);
  });

  it("asks for a ceremony when more than one identity holds a session", async () => {
    await storedSession(BigInt(10_000));
    await storedSession(BigInt(10_001));
    const { channel } = channelWith();

    const handled = handleSessionDelegationRequest(
      channel,
      vi.fn(),
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: { sessionPublicKey: await appKey() },
    });
    await Promise.race([
      handled,
      new Promise((resolve) => setTimeout(resolve, 50)),
    ]);

    expect(setRequestContext).toHaveBeenCalledWith(ORIGIN, undefined);
  });
});

describe("recovering from a revoked session", () => {
  it("does not answer from a held record once the ceremony path is taken", async () => {
    await storedSession(BigInt(10_000));
    await storedSession(BigInt(10_001));
    const { channel } = channelWith();

    const handled = handleSessionDelegationRequest(
      channel,
      vi.fn(),
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: { sessionPublicKey: await appKey() },
    });
    await Promise.race([
      handled,
      new Promise((resolve) => setTimeout(resolve, 50)),
    ]);

    expect(setRequestContext).toHaveBeenCalled();
  });
});
