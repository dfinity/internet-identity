import { beforeEach, describe, expect, it, vi } from "vitest";
import type { SignIdentity } from "@icp-sdk/core/agent";

const registeredIdentityNumbers = vi.fn<() => Promise<bigint[]>>();
const browserKeyIdentity =
  vi.fn<(identityNumber: bigint) => Promise<SignIdentity | undefined>>();
const readAppKey =
  vi.fn<() => Promise<{ keyPair: CryptoKeyPair } | undefined>>();
const fromKeyPair = vi.fn((keyPair: CryptoKeyPair) =>
  Promise.resolve({ keyPair } as unknown as SignIdentity),
);

vi.mock("$lib/stores/browser-key.store", () => ({
  registeredIdentityNumbers: () => registeredIdentityNumbers(),
  browserKeyIdentity: (identityNumber: bigint) =>
    browserKeyIdentity(identityNumber),
}));
vi.mock("$lib/utils/notifications/notificationAppKey", () => ({
  readAppKey: () => readAppKey(),
}));
vi.mock("$lib/utils/notifications/workerConfig", () => ({
  config: {
    canisterId: "rdmx6-jaaaa-aaaaa-aaadq-cai",
    agentOptions: { host: "http://127.0.0.1:4943" },
  },
}));
const anchorsAnswer = vi.fn<() => Promise<bigint[]>>(() => Promise.resolve([]));
vi.mock("$lib/utils/notifications/notificationAppActor", () => ({
  appKeyActorWith: () =>
    Promise.resolve({ notification_app_anchors: () => anchorsAnswer() }),
}));
vi.mock("@icp-sdk/core/identity", () => ({
  ECDSAKeyIdentity: { fromKeyPair: (pair: CryptoKeyPair) => fromKeyPair(pair) },
}));

const { signingIdentities } =
  await import("$lib/utils/notifications/signingIdentities");

const APP_KEY_PAIR = { app: true } as unknown as CryptoKeyPair;
const BROWSER = BigInt(10_000);
const APP = BigInt(10_001);
/** An identity whose own browser claimed this app, so only the canister names it. */
const CLAIMED = BigInt(10_002);

const signsAs = (identityNumber: bigint): SignIdentity =>
  ({ identityNumber }) as unknown as SignIdentity;

beforeEach(() => {
  vi.clearAllMocks();
  registeredIdentityNumbers.mockResolvedValue([]);
  readAppKey.mockResolvedValue(undefined);
  browserKeyIdentity.mockImplementation((identityNumber) =>
    Promise.resolve(signsAs(identityNumber)),
  );
});

describe("signingIdentities", () => {
  it("answers one entry per identity signed in to this browser", async () => {
    registeredIdentityNumbers.mockResolvedValue([BROWSER, APP]);

    expect(await signingIdentities()).toEqual([
      { identityNumber: BROWSER, identity: signsAs(BROWSER) },
      { identityNumber: APP, identity: signsAs(APP) },
    ]);
  });

  // The whole of the iOS path: the Home Screen app has its own storage partition, so the
  // browser key store there is empty and the identities pointing at the app are the only
  // thing the worker can ask the canister as.
  it("answers the identities pointing at the app where nothing has signed in", async () => {
    readAppKey.mockResolvedValue({ keyPair: APP_KEY_PAIR });
    anchorsAnswer.mockResolvedValueOnce([APP]);

    expect(await signingIdentities()).toEqual([
      { identityNumber: APP, identity: { keyPair: APP_KEY_PAIR } },
    ]);
    expect(fromKeyPair).toHaveBeenCalledWith(APP_KEY_PAIR);
  });

  /// A second identity's browser points it at this app without the app running, so the
  /// canister is the only party that can name it.
  it("answers every identity the canister says points at the app", async () => {
    readAppKey.mockResolvedValue({ keyPair: APP_KEY_PAIR });
    anchorsAnswer.mockResolvedValueOnce([APP, CLAIMED]);

    expect(await signingIdentities()).toEqual([
      { identityNumber: APP, identity: { keyPair: APP_KEY_PAIR } },
      { identityNumber: CLAIMED, identity: { keyPair: APP_KEY_PAIR } },
    ]);
  });

  it("answers both where a browser has signed in and an app is pointed at", async () => {
    registeredIdentityNumbers.mockResolvedValue([BROWSER]);
    readAppKey.mockResolvedValue({ keyPair: APP_KEY_PAIR });
    anchorsAnswer.mockResolvedValueOnce([APP]);

    expect(await signingIdentities()).toHaveLength(2);
  });

  // A key the canister has no record for names no anchor, and every call needs one.
  it("leaves out an app key nothing points at", async () => {
    readAppKey.mockResolvedValue({ keyPair: APP_KEY_PAIR });

    expect(await signingIdentities()).toEqual([]);
  });

  // A browser that has not completed a sign-in holds a key the canister cannot place.
  it("leaves out an identity whose key is not registered", async () => {
    registeredIdentityNumbers.mockResolvedValue([BROWSER]);
    browserKeyIdentity.mockResolvedValue(undefined);

    expect(await signingIdentities()).toEqual([]);
  });

  /// Who points at this app is the canister's to say, so a wake-up that cannot reach it
  /// has nothing to sign for rather than a guess.
  it("answers nothing where the anchors cannot be read", async () => {
    readAppKey.mockResolvedValue({ keyPair: APP_KEY_PAIR });
    anchorsAnswer.mockRejectedValueOnce(new Error("offline"));

    expect(await signingIdentities()).toEqual([]);
  });

  it("answers what it can where one source throws", async () => {
    registeredIdentityNumbers.mockRejectedValue(new Error("no storage"));
    readAppKey.mockResolvedValue({ keyPair: APP_KEY_PAIR });
    anchorsAnswer.mockResolvedValueOnce([APP]);

    expect(await signingIdentities()).toHaveLength(1);
  });
});
