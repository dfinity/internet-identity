import {
  describe,
  it,
  expect,
  vi,
  beforeAll,
  beforeEach,
  afterEach,
} from "vitest";
import { IDBFactory } from "fake-indexeddb";
import {
  set as idbSet,
  get as idbGet,
  del as idbDel,
  createStore as idbCreateStore,
} from "idb-keyval";
import { Principal } from "@icp-sdk/core/principal";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";

vi.mock("$app/environment", () => ({ browser: true }));

vi.mock("$lib/globals", () => ({
  canisterId: Principal.anonymous(),
  agentOptions: {},
}));

const makeMockActor = () =>
  ({
    get_accounts: vi.fn(),
    get_default_account: vi.fn(),
    set_default_account: vi.fn(),
  }) as unknown as ActorSubclass<_SERVICE>;

const IDENTITY_NUMBER = BigInt(42);
const NOW = 1_700_000_000_000;
const FAR_FUTURE = NOW + 10 * 60 * 1000;
const NEAR_EXPIRY = NOW + 4 * 60 * 1000;

const TEST_STORE = idbCreateStore("ii-session-delegations", "keys");

const makeKeyPair = async (): Promise<CryptoKeyPair> => {
  const { ECDSAKeyIdentity } = await import("@icp-sdk/core/identity");
  const id = await ECDSAKeyIdentity.generate({ extractable: false });
  return id.getKeyPair();
};

const makeChainJson = async (): Promise<string> => {
  const { DelegationChain, Delegation } =
    await import("@icp-sdk/core/identity");
  const chain = DelegationChain.fromDelegations(
    [
      {
        delegation: new Delegation(
          new Uint8Array(32).fill(1),
          BigInt(FAR_FUTURE) * BigInt(1_000_000),
        ),
        signature: new Uint8Array(
          64,
        ) as unknown as import("@icp-sdk/core/agent").Signature,
      },
    ],
    new Uint8Array(32).fill(2),
  );
  return JSON.stringify(chain.toJSON());
};

beforeAll(() => {
  global.indexedDB = new IDBFactory();
});

beforeEach(async () => {
  vi.useFakeTimers();
  vi.setSystemTime(NOW);
  await idbDel(IDENTITY_NUMBER.toString(), TEST_STORE);
});

afterEach(() => {
  vi.useRealTimers();
  vi.restoreAllMocks();
});

vi.mock("$lib/stores/authentication.store", () => ({
  authenticationStore: {
    subscribe: (cb: (v: unknown) => void) => {
      cb(undefined);
      return () => {};
    },
  },
}));

describe("actorForIdentity — authenticated store precedence", () => {
  it("returns the authenticated actor when the authenticated store has a matching identity number", async () => {
    const mockActor = makeMockActor();
    const { authenticationStore } =
      await import("$lib/stores/authentication.store");
    vi.spyOn(authenticationStore, "subscribe").mockImplementation((cb) => {
      cb({ identityNumber: IDENTITY_NUMBER, actor: mockActor } as Parameters<
        typeof cb
      >[0]);
      return () => {};
    });

    const { actorForIdentity } =
      await import("$lib/stores/session-delegation.store");

    const result = await actorForIdentity(IDENTITY_NUMBER);
    expect(result).toBe(mockActor);
  });

  it("does not return the authenticated actor when identity number differs", async () => {
    const mockActor = makeMockActor();
    const { authenticationStore } =
      await import("$lib/stores/authentication.store");
    vi.spyOn(authenticationStore, "subscribe").mockImplementation((cb) => {
      cb({ identityNumber: BigInt(999), actor: mockActor } as Parameters<
        typeof cb
      >[0]);
      return () => {};
    });

    const { actorForIdentity } =
      await import("$lib/stores/session-delegation.store");

    const result = await actorForIdentity(IDENTITY_NUMBER);
    expect(result).toBeUndefined();
  });
});

describe("actorForIdentity — IDB resolution", () => {
  it("returns undefined when no record exists in IDB", async () => {
    const { actorForIdentity } =
      await import("$lib/stores/session-delegation.store");
    const result = await actorForIdentity(IDENTITY_NUMBER);
    expect(result).toBeUndefined();
  });

  it("returns an actor when a valid unexpired record exists in IDB", async () => {
    const keyPair = await makeKeyPair();
    const chainJson = await makeChainJson();

    await idbSet(
      IDENTITY_NUMBER.toString(),
      {
        identityNumber: IDENTITY_NUMBER,
        keyPair,
        chainJson,
        expiresAtMillis: FAR_FUTURE,
      },
      TEST_STORE,
    );

    const { actorForIdentity } =
      await import("$lib/stores/session-delegation.store");
    const result = await actorForIdentity(IDENTITY_NUMBER);
    expect(result).toBeDefined();
  });

  it("returns undefined and purges when record is within the 5-minute expiry margin", async () => {
    const keyPair = await makeKeyPair();
    const chainJson = await makeChainJson();

    await idbSet(
      IDENTITY_NUMBER.toString(),
      {
        identityNumber: IDENTITY_NUMBER,
        keyPair,
        chainJson,
        expiresAtMillis: NEAR_EXPIRY,
      },
      TEST_STORE,
    );

    const { actorForIdentity } =
      await import("$lib/stores/session-delegation.store");
    const result = await actorForIdentity(IDENTITY_NUMBER);
    expect(result).toBeUndefined();

    await vi.runAllTimersAsync();
    const remaining = await idbGet(IDENTITY_NUMBER.toString(), TEST_STORE);
    expect(remaining).toBeUndefined();
  });

  it("returns undefined and purges when sessionDelegationIdentity throws", async () => {
    const keyPair = await makeKeyPair();

    await idbSet(
      IDENTITY_NUMBER.toString(),
      {
        identityNumber: IDENTITY_NUMBER,
        keyPair,
        chainJson: "not valid json at all }{",
        expiresAtMillis: FAR_FUTURE,
      },
      TEST_STORE,
    );

    const { actorForIdentity } =
      await import("$lib/stores/session-delegation.store");
    const result = await actorForIdentity(IDENTITY_NUMBER);
    expect(result).toBeUndefined();

    await vi.runAllTimersAsync();
    const remaining = await idbGet(IDENTITY_NUMBER.toString(), TEST_STORE);
    expect(remaining).toBeUndefined();
  });

  it("round-trips mintSession → actorForIdentity with non-extractable keys", async () => {
    const expirationNs = BigInt(FAR_FUTURE) * BigInt(1_000_000);

    const mintActor = {
      prepare_session_delegation: vi.fn().mockResolvedValue({
        Ok: {
          user_key: Array.from(new Uint8Array(32).fill(9)),
          expiration: expirationNs,
        },
      }),
      get_session_delegation: vi
        .fn()
        .mockImplementation(
          (_anchor: unknown, sessionKey: number[], expiration: bigint) =>
            Promise.resolve({
              Ok: {
                delegation: {
                  pubkey: Array.from(sessionKey),
                  expiration,
                  targets: [],
                  permissions: [],
                },
                signature: Array.from(new Uint8Array(64)),
              },
            }),
        ),
    } as unknown as ActorSubclass<_SERVICE>;

    const { mintSession, actorForIdentity } =
      await import("$lib/stores/session-delegation.store");

    await mintSession({ identityNumber: IDENTITY_NUMBER, actor: mintActor });

    const result = await actorForIdentity(IDENTITY_NUMBER);
    expect(result).toBeDefined();
  });
});

describe("purgeSession", () => {
  it("removes the record from IDB", async () => {
    const keyPair = await makeKeyPair();
    const chainJson = await makeChainJson();

    await idbSet(
      IDENTITY_NUMBER.toString(),
      {
        identityNumber: IDENTITY_NUMBER,
        keyPair,
        chainJson,
        expiresAtMillis: FAR_FUTURE,
      },
      TEST_STORE,
    );

    const { purgeSession } =
      await import("$lib/stores/session-delegation.store");
    await purgeSession(IDENTITY_NUMBER);

    const remaining = await idbGet(IDENTITY_NUMBER.toString(), TEST_STORE);
    expect(remaining).toBeUndefined();
  });

  it("does not throw when the record does not exist", async () => {
    const { purgeSession } =
      await import("$lib/stores/session-delegation.store");
    await expect(purgeSession(IDENTITY_NUMBER)).resolves.toBeUndefined();
  });
});

describe("mintSession — failure is swallowed", () => {
  it("does not throw when mintSessionDelegation rejects", async () => {
    const sdModule =
      await import("$lib/utils/authentication/sessionDelegation");
    vi.spyOn(sdModule, "mintSessionDelegation").mockRejectedValue(
      new Error("backend not deployed"),
    );

    const { mintSession } =
      await import("$lib/stores/session-delegation.store");
    const actor = makeMockActor();

    await expect(
      mintSession({ identityNumber: IDENTITY_NUMBER, actor }),
    ).resolves.toBeUndefined();
  });

  it("does not write to IDB when get_session_delegation returns Err", async () => {
    const keyPair = await makeKeyPair();
    const chainJson = await makeChainJson();
    const existingRecord = {
      identityNumber: IDENTITY_NUMBER,
      keyPair,
      chainJson,
      expiresAtMillis: FAR_FUTURE,
    };
    await idbSet(IDENTITY_NUMBER.toString(), existingRecord, TEST_STORE);

    const sdModule =
      await import("$lib/utils/authentication/sessionDelegation");
    vi.spyOn(sdModule, "mintSessionDelegation").mockRejectedValue(
      new Error("NoSuchDelegation"),
    );

    const { mintSession } =
      await import("$lib/stores/session-delegation.store");
    const actor = makeMockActor();

    await mintSession({ identityNumber: IDENTITY_NUMBER, actor });

    const remaining = await idbGet(IDENTITY_NUMBER.toString(), TEST_STORE);
    expect(remaining).toEqual(existingRecord);
  });
});

describe("forgetIdentity", () => {
  const BROWSER_KEY_STORE = idbCreateStore("ii-browser-keys", "keys");

  const storedSessionDelegation = async () => {
    await idbSet(
      IDENTITY_NUMBER.toString(),
      {
        identityNumber: IDENTITY_NUMBER,
        keyPair: await makeKeyPair(),
        chainJson: await makeChainJson(),
        expiresAtMillis: FAR_FUTURE,
      },
      TEST_STORE,
    );
  };

  const storedAppSession = async () => {
    const { storeAppSession } = await import("$lib/stores/app-session.store");
    await storeAppSession(
      { identityNumber: IDENTITY_NUMBER, origin: "https://app.example.com" },
      {
        keyPair: await makeKeyPair(),
        chainJson: await makeChainJson(),
        expiresAtMillis: FAR_FUTURE,
        sessionId: BigInt(1),
        accessLevel: "full-access",
      },
    );
  };

  const knownBrowser = (deviceId: number) =>
    idbSet(
      IDENTITY_NUMBER.toString(),
      { keyPair: undefined, deviceId },
      BROWSER_KEY_STORE,
    );

  beforeEach(async () => {
    await idbDel(IDENTITY_NUMBER.toString(), BROWSER_KEY_STORE);
    const { purgeAppSessions } = await import("$lib/stores/app-session.store");
    await purgeAppSessions(IDENTITY_NUMBER);
  });

  /// The gap this exists to close: dropping the local records alone leaves the apps
  /// holding chains rooted at session records the canister still has, so they stay
  /// signed in and keep refreshing.
  it("ends this browser's sessions for the identity it forgets", async () => {
    const revoke = vi.fn(() => Promise.resolve({ Ok: null }));
    const actor = {
      revoke_device_sessions: revoke,
    } as unknown as ActorSubclass<_SERVICE>;
    const { authenticationStore } =
      await import("$lib/stores/authentication.store");
    vi.spyOn(authenticationStore, "subscribe").mockImplementation((cb) => {
      cb({ identityNumber: IDENTITY_NUMBER, actor } as Parameters<
        typeof cb
      >[0]);
      return () => {};
    });
    await knownBrowser(7);
    await storedSessionDelegation();
    await storedAppSession();

    const { forgetIdentity } =
      await import("$lib/stores/session-delegation.store");
    await forgetIdentity(IDENTITY_NUMBER);

    expect(revoke).toHaveBeenCalledWith({
      identity_number: IDENTITY_NUMBER,
      device_id: 7,
    });
    const { appSessionsForOrigin } =
      await import("$lib/stores/app-session.store");
    await expect(
      appSessionsForOrigin("https://app.example.com"),
    ).resolves.toEqual([]);
    await expect(
      idbGet(IDENTITY_NUMBER.toString(), TEST_STORE),
    ).resolves.toBeUndefined();
  });

  /// The local half must not depend on the canister being reachable: keeping the records
  /// because a call failed would leave II able to sign the user back in silently, which
  /// is the thing they asked it to stop doing.
  it("forgets locally even when the canister call fails", async () => {
    const actor = {
      revoke_device_sessions: vi.fn(() => Promise.reject(new Error("offline"))),
    } as unknown as ActorSubclass<_SERVICE>;
    const { authenticationStore } =
      await import("$lib/stores/authentication.store");
    vi.spyOn(authenticationStore, "subscribe").mockImplementation((cb) => {
      cb({ identityNumber: IDENTITY_NUMBER, actor } as Parameters<
        typeof cb
      >[0]);
      return () => {};
    });
    await knownBrowser(7);
    await storedSessionDelegation();
    await storedAppSession();

    const { forgetIdentity } =
      await import("$lib/stores/session-delegation.store");
    await expect(forgetIdentity(IDENTITY_NUMBER)).resolves.toBeUndefined();

    const { appSessionsForOrigin } =
      await import("$lib/stores/app-session.store");
    await expect(
      appSessionsForOrigin("https://app.example.com"),
    ).resolves.toEqual([]);
  });

  it("forgets locally when this browser has never signed in as the identity", async () => {
    await storedSessionDelegation();
    await storedAppSession();

    const { forgetIdentity } =
      await import("$lib/stores/session-delegation.store");
    await forgetIdentity(IDENTITY_NUMBER);

    const { appSessionsForOrigin } =
      await import("$lib/stores/app-session.store");
    await expect(
      appSessionsForOrigin("https://app.example.com"),
    ).resolves.toEqual([]);
  });
});
