import { beforeEach, describe, expect, it, vi } from "vitest";
import "fake-indexeddb/auto";
import { DelegationChain, ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { Principal } from "@icp-sdk/core/principal";
import type { Writable } from "svelte/store";

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
vi.mock("$lib/stores/authentication.store", async () => {
  const { writable } = await import("svelte/store");
  return { authenticationStore: writable<unknown>(undefined) };
});
vi.mock("$lib/stores/browser-key.store", async (importOriginal) => ({
  ...(await importOriginal<typeof import("$lib/stores/browser-key.store")>()),
  withBrowserProof: (
    _identityNumber: bigint,
    _sessionKey: Uint8Array,
    signIn: (proof: unknown) => Promise<unknown>,
  ) =>
    signIn({
      publicKey: new Uint8Array(),
      nextPublicKey: new Uint8Array(),
      signature: new Uint8Array(),
      nextSignature: new Uint8Array(),
      accept: () => Promise.resolve(),
    }),
}));
vi.mock("$lib/stores/channelHandlers/describeBrowser", () => ({
  describeBrowser: () => Promise.resolve("a browser"),
}));

const setRequestContext = vi.fn();
vi.mock("$lib/stores/authorization.store", async () => {
  const { writable } = await import("svelte/store");
  return {
    authorizationStore: {
      setRequestContext: (...args: unknown[]) => setRequestContext(...args),
    },
    authorizedStore: writable<unknown>(undefined),
    authorizationPromptStore: writable<{ prompt?: string; hint?: string }>({}),
  };
});

import {
  asBrowserKeyError,
  handleSessionDelegationRequest,
} from "./sessionDelegation";
import { StaleBrowserKeyError } from "$lib/stores/browser-key.store";
import { CanisterError } from "$lib/utils/utils";
import {
  appAccountsForOrigin,
  appSessionsForOrigin,
  rememberAppAccount,
  purgeAppSessions,
  storeAppSession,
} from "$lib/stores/app-session.store";
import { INTERACTION_REQUIRED_ERROR_CODE } from "$lib/utils/transport/utils";

/** Drives one ceremony to the point where the session it created is either kept or
 *  discarded, which is the whole of what `resumable` decides. */
const runCeremony = async (
  resumable?: boolean,
  extraParams: Record<string, unknown> = {},
) => {
  const { authorizationPromptStore, authorizedStore } =
    await import("$lib/stores/authorization.store");
  authorizationPromptStore.set(resumable === undefined ? {} : { resumable });

  const identityNumber = BigInt(10_000);
  const sessionKey = await ECDSAKeyIdentity.generate({ extractable: true });
  const expiration = BigInt(Date.now() + 60 * 60 * 1000) * BigInt(1_000_000);
  const chain = await DelegationChain.create(
    sessionKey,
    sessionKey.getPublicKey(),
    new Date(Number(expiration / BigInt(1_000_000))),
  );
  const signed = chain.delegations[0];

  const prepared: Record<string, unknown>[] = [];
  const actor = {
    prepare_account_session: (request: Record<string, unknown>) => (
      prepared.push(request),
      Promise.resolve({
        Ok: {
          user_key: new Uint8Array(chain.publicKey),
          expiration,
          session_id: BigInt(1_000),
          account_principal: Principal.fromText("2vxsx-fae"),
          device_id: BigInt(1),
        },
      })
    ),
    get_account_session: () =>
      Promise.resolve({
        Ok: {
          signed_delegation: {
            delegation: {
              pubkey: new Uint8Array(signed.delegation.pubkey),
              expiration: signed.delegation.expiration,
              targets: [],
            },
            signature: new Uint8Array(signed.signature),
          },
        },
      }),
  };
  const { authenticationStore } =
    await import("$lib/stores/authentication.store");
  // Both stores are mocked as plain writables above; only their real types are in
  // scope here, and neither is writable or shaped like what the handler reads.
  (authenticationStore as unknown as Writable<unknown>).set({
    identityNumber,
    actor,
    authMethod: { passkey: { credentialId: new Uint8Array() } },
  });
  (authorizedStore as unknown as Writable<unknown>).set({
    accountNumberPromise: Promise.resolve(undefined),
    accessLevel: "full-access",
  });

  const { channel, sent } = channelWith();
  await handleSessionDelegationRequest(
    channel,
    vi.fn(),
  )({
    jsonrpc: "2.0",
    id: 1,
    method: "ii_session_delegation",
    params: { sessionPublicKey: await appKey(), ...extraParams },
  });
  return { sent, prepared };
};

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
  await rememberAppAccount(
    { identityNumber, origin: ORIGIN },
    { accountPrincipal: "2vxsx-fae" },
  );
  await storeAppSession(
    { identityNumber, origin: ORIGIN },
    {
      keyPair: key.getKeyPair(),
      chainJson: JSON.stringify(chain.toJSON()),
      expiresAtMillis: Date.now() + 60 * 60 * 1000,
      sessionId: BigInt(1_000),
      accessLevel: "full-access" as const,
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

describe("keeping a session for later", () => {
  beforeEach(async () => {
    await purgeAppSessions(BigInt(10_000));
    await purgeAppSessions(BigInt(10_001));
  });

  // Earlier tests in this file start a handler and only race it against a
  // timeout, so one can still be waiting on the auth stores when a later
  // fixture sets them — and then it prepares a session of its own. The TTL this
  // ceremony asks for is what picks its request out of the ones captured.
  const ourRequest = (
    prepared: Record<string, unknown>[],
    validForNs: bigint,
  ): Record<string, unknown> => {
    const found = prepared.find(
      (request) =>
        Array.isArray(request.valid_for) && request.valid_for[0] === validForNs,
    );
    expect(found, "this ceremony's prepare_account_session call").toBeDefined();
    return found as Record<string, unknown>;
  };

  it("carries the app's idle bound to the canister", async () => {
    const { sent, prepared } = await runCeremony(true, {
      maxTimeToLive: "3600000000000",
      maxTimeToIdle: "600000000000",
    });

    expect(sent).toHaveLength(1);
    expect(ourRequest(prepared, BigInt(3_600_000_000_000)).max_idle).toEqual([
      BigInt(600_000_000_000),
    ]);
  });

  it("leaves the bound to the canister when the app names none", async () => {
    // Absent rather than a number this frontend picked: the default belongs to
    // the canister, and sending one here would override it.
    const { prepared } = await runCeremony(true, {
      maxTimeToLive: "7200000000000",
    });

    expect(ourRequest(prepared, BigInt(7_200_000_000_000)).max_idle).toEqual(
      [],
    );
  });

  it("keeps a session the app asked to be resumable", async () => {
    const { sent } = await runCeremony(true);

    expect(sent).toHaveLength(1);
    await expect(appSessionsForOrigin(ORIGIN)).resolves.toHaveLength(1);
  });

  it("answers without keeping a session the app did not ask to be resumable", async () => {
    const { sent } = await runCeremony();

    // The app is served either way: what `resumable` decides is only whether this
    // browser can answer the next request without another ceremony.
    expect(sent).toHaveLength(1);
    await expect(appSessionsForOrigin(ORIGIN)).resolves.toEqual([]);
  });

  it("remembers the account even when the session is not kept", async () => {
    await runCeremony();

    await expect(appAccountsForOrigin(ORIGIN)).resolves.toMatchObject([
      { record: { accountPrincipal: "2vxsx-fae" } },
    ]);
  });
});

describe("asBrowserKeyError", () => {
  it("names a retired browser key so the key store can promote its successor", () => {
    const stale = asBrowserKeyError(
      new CanisterError({ StaleDeviceKey: null }),
    );

    expect(stale).toBeInstanceOf(StaleBrowserKeyError);
  });

  it("leaves every other canister error alone", () => {
    const other = new CanisterError({ NoSuchAccount: null });

    expect(asBrowserKeyError(other)).toBe(other);
  });

  it("leaves a transport failure alone", () => {
    const network = new Error("network");

    expect(asBrowserKeyError(network)).toBe(network);
  });
});
