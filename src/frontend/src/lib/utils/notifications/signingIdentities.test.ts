import { beforeEach, describe, expect, it, vi } from "vitest";
import type { SignIdentity } from "@icp-sdk/core/agent";

const registeredIdentityNumbers = vi.fn<() => Promise<bigint[]>>();
const browserKeyIdentity =
  vi.fn<(identityNumber: bigint) => Promise<SignIdentity | undefined>>();
const readAppKey =
  vi.fn<
    () => Promise<
      { keyPair: CryptoKeyPair; identityNumber?: bigint } | undefined
    >
  >();
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
vi.mock("@icp-sdk/core/identity", () => ({
  ECDSAKeyIdentity: { fromKeyPair: (pair: CryptoKeyPair) => fromKeyPair(pair) },
}));

const { signingIdentities } =
  await import("$lib/utils/notifications/signingIdentities");

const APP_KEY_PAIR = { app: true } as unknown as CryptoKeyPair;
const BROWSER = BigInt(10_000);
const APP = BigInt(10_001);

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

  // The whole of the iOS path: the Home Screen app has its own storage partition, so
  // the browser key store there is empty and its linked entry is the only thing the
  // worker can ask the canister as.
  it("answers the app's linked entry where nothing has signed in", async () => {
    readAppKey.mockResolvedValue({
      keyPair: APP_KEY_PAIR,
      identityNumber: APP,
    });

    expect(await signingIdentities()).toEqual([
      { identityNumber: APP, identity: { keyPair: APP_KEY_PAIR } },
    ]);
    expect(fromKeyPair).toHaveBeenCalledWith(APP_KEY_PAIR);
  });

  it("answers both where a browser has signed in and claimed an app", async () => {
    registeredIdentityNumbers.mockResolvedValue([BROWSER]);
    readAppKey.mockResolvedValue({
      keyPair: APP_KEY_PAIR,
      identityNumber: APP,
    });

    expect(await signingIdentities()).toHaveLength(2);
  });

  // A key generated but never linked names no anchor, and every call needs one.
  it("leaves out an app key the canister has not accepted", async () => {
    readAppKey.mockResolvedValue({ keyPair: APP_KEY_PAIR });

    expect(await signingIdentities()).toEqual([]);
  });

  // A browser that has not completed a sign-in holds a key the canister cannot place.
  it("leaves out an identity whose key is not registered", async () => {
    registeredIdentityNumbers.mockResolvedValue([BROWSER]);
    browserKeyIdentity.mockResolvedValue(undefined);

    expect(await signingIdentities()).toEqual([]);
  });

  it("answers what it can where one source throws", async () => {
    registeredIdentityNumbers.mockRejectedValue(new Error("no storage"));
    readAppKey.mockResolvedValue({
      keyPair: APP_KEY_PAIR,
      identityNumber: APP,
    });

    expect(await signingIdentities()).toHaveLength(1);
  });
});
