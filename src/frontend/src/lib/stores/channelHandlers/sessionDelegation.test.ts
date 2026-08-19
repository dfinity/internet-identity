import { beforeEach, describe, expect, it, vi } from "vitest";
import "fake-indexeddb/auto";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";

const CANISTER_ID_TEXT = "rwlgt-iiaaa-aaaaa-aaaaa-cai";
const ORIGIN = "https://app.example.com";

vi.mock("$lib/globals", async () => {
  const { Principal } = await import("@icp-sdk/core/principal");
  return {
    agentOptions: {},
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

const checkSession = vi.fn(() => Promise.resolve(true));
vi.mock("@icp-sdk/core/agent", async () => {
  const actual = await vi.importActual<typeof import("@icp-sdk/core/agent")>(
    "@icp-sdk/core/agent",
  );
  return {
    ...actual,
    HttpAgent: { ...actual.HttpAgent, createSync: () => ({}) },
    Actor: {
      ...actual.Actor,
      createActor: () => ({ check_session: checkSession }),
    },
  };
});

const setRequestContext = vi.fn();
vi.mock("$lib/stores/authorization.store", async () => {
  const { writable } = await import("svelte/store");
  return {
    authorizationStore: {
      setRequestContext: (...args: unknown[]) => setRequestContext(...args),
    },
    authorizedStore: { subscribe: () => () => {} },
    authorizationPromptStore: writable<{ prompt?: string; hint?: string }>({}),
  };
});

import { handleSessionDelegationRequest } from "./sessionDelegation";
import {
  appSessionsForOrigin,
  purgeAppSessions,
  storeAppSession,
} from "$lib/stores/app-session.store";
import { INTERACTION_REQUIRED_ERROR_CODE } from "$lib/utils/transport/utils";

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

  it("answers a malformed silent request without rendering anything", async () => {
    const { authorizationPromptStore } =
      await import("$lib/stores/authorization.store");
    authorizationPromptStore.set({ prompt: "none" });
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
    expect(onError).not.toHaveBeenCalled();
  });

  it("re-issues from a held session when silence is asked for", async () => {
    await storedSession(BigInt(10_000));
    const { authorizationPromptStore } =
      await import("$lib/stores/authorization.store");
    authorizationPromptStore.set({ prompt: "none" });
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
    const { authorizationPromptStore } =
      await import("$lib/stores/authorization.store");
    authorizationPromptStore.set({ prompt: "none" });
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

  it("answers a silent request it cannot satisfy without rendering", async () => {
    const { authorizationPromptStore } =
      await import("$lib/stores/authorization.store");
    authorizationPromptStore.set({ prompt: "none" });
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

    expect(setRequestContext).not.toHaveBeenCalled();
    expect(onError).not.toHaveBeenCalled();
    expect(sent[0]).toMatchObject({
      error: { code: 3002, data: { reason: "login_required" } },
    });
    authorizationPromptStore.set({});
  });
});

describe("a session the canister no longer holds", () => {
  const promptStore = async () =>
    (await import("$lib/stores/authorization.store")).authorizationPromptStore;

  beforeEach(async () => {
    checkSession.mockClear();
    await purgeAppSessions(BigInt(10_000));
    (await promptStore()).set({});
  });

  it("denies a silent request when the canister no longer holds the session", async () => {
    checkSession.mockResolvedValueOnce(false);
    await storedSession(BigInt(10_000));
    const { channel, sent } = channelWith();
    (await promptStore()).set({ prompt: "none" });

    await handleSessionDelegationRequest(
      channel,
      vi.fn(),
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: { sessionPublicKey: await appKey() },
    });

    expect(sent).toHaveLength(1);
    expect(sent[0]).toMatchObject({
      error: { code: INTERACTION_REQUIRED_ERROR_CODE },
    });
  });

  it("forgets a record the canister no longer holds", async () => {
    checkSession.mockResolvedValueOnce(false);
    await storedSession(BigInt(10_000));
    const { channel } = channelWith();
    (await promptStore()).set({ prompt: "none" });

    await handleSessionDelegationRequest(
      channel,
      vi.fn(),
    )({
      jsonrpc: "2.0",
      id: 1,
      method: "ii_session_delegation",
      params: { sessionPublicKey: await appKey() },
    });

    expect(await appSessionsForOrigin(ORIGIN)).toEqual([]);
  });
});

describe("silent requests never paint", () => {
  const promptStore = async () =>
    (await import("$lib/stores/authorization.store")).authorizationPromptStore;

  it("answers rather than surfacing an unverified origin", async () => {
    const { validateDerivationOrigin } =
      await import("$lib/utils/validateDerivationOrigin");
    vi.mocked(validateDerivationOrigin).mockResolvedValueOnce({
      result: "invalid",
      message: "nope",
    });
    (await promptStore()).set({ prompt: "none" });
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
    expect(sent[0]).toMatchObject({ error: { code: 3002 } });
    (await promptStore()).set({});
  });

  it("runs a ceremony for prompt=login even when a session is held", async () => {
    await storedSession(BigInt(10_000));
    (await promptStore()).set({ prompt: "login" });
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
    (await promptStore()).set({});
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

// Ordered last: each leaves a ceremony pending, which holds the shared authorization queue.
describe("requests that fall through to a ceremony", () => {
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

  it("runs the ceremony when silence was not asked for", async () => {
    await storedSession(BigInt(10_000));
    const { authorizationPromptStore } =
      await import("$lib/stores/authorization.store");
    authorizationPromptStore.set({});
    const { channel, sent } = channelWith();

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

    // A held session is not handed back: the ceremony starts and nothing is answered from
    // local state, because silence is something an app has to ask for.
    expect(setRequestContext).toHaveBeenCalled();
    expect(sent).toEqual([]);
  });
});
