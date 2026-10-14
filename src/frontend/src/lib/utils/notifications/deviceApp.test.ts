import { beforeEach, describe, expect, it, vi } from "vitest";

const registeredIdentityNumbers = vi.fn<() => Promise<bigint[]>>();
const browserKeyActor = vi.fn();

vi.mock("$lib/stores/browser-key.store", () => ({
  registeredIdentityNumbers: () => registeredIdentityNumbers(),
}));
vi.mock("$lib/utils/notifications/browserActor", () => ({
  browserKeyActor: (identityNumber: bigint) => browserKeyActor(identityNumber),
}));

const { findDeviceApp } = await import("$lib/utils/notifications/deviceApp");

const ASKING = BigInt(10_000);
const INSTALLER = BigInt(10_001);

const LINK = {
  app_key: Uint8Array.from([1, 2]),
  endpoint: "https://web.push.apple.com/abc",
  vapid_public_key: Uint8Array.from([3]),
  jwt_signatures: [Uint8Array.from([4])],
  jwt_issued_at_ns: BigInt(7),
};

/** The canister, answering the link only for the identities named. */
const canisterHolding = (...holders: bigint[]) => {
  const asked: bigint[] = [];
  browserKeyActor.mockImplementation((identityNumber: bigint) => {
    asked.push(identityNumber);
    return Promise.resolve({
      notification_app_link: () =>
        Promise.resolve(holders.includes(identityNumber) ? [LINK] : []),
    });
  });
  return asked;
};

beforeEach(() => {
  vi.clearAllMocks();
  registeredIdentityNumbers.mockResolvedValue([ASKING, INSTALLER]);
});

describe("findDeviceApp", () => {
  // The whole point: the app belongs to the identity that installed it, and the
  // identity being asked about has nothing of its own to read.
  it("finds the app another identity in this browser installed", async () => {
    canisterHolding(INSTALLER);

    expect(await findDeviceApp(ASKING)).toEqual({
      appKey: LINK.app_key,
      endpoint: LINK.endpoint,
      vapidPublicKey: LINK.vapid_public_key,
      jwtSignatures: LINK.jwt_signatures,
      jwtIssuedAtNs: LINK.jwt_issued_at_ns,
    });
  });

  it("never asks the identity being asked about", async () => {
    const asked = canisterHolding(INSTALLER);

    await findDeviceApp(ASKING);

    expect(asked).toEqual([INSTALLER]);
  });

  it("answers nothing where no identity here has one", async () => {
    canisterHolding();

    expect(await findDeviceApp(ASKING)).toBeUndefined();
  });

  // A browser holding a key it cannot call with is not a device without an app.
  it("passes over an identity it cannot call as", async () => {
    registeredIdentityNumbers.mockResolvedValue([
      ASKING,
      BigInt(10_002),
      INSTALLER,
    ]);
    canisterHolding(INSTALLER);
    browserKeyActor.mockImplementationOnce(() =>
      Promise.reject(new Error("this browser has not completed a sign-in")),
    );

    expect(await findDeviceApp(ASKING)).toBeDefined();
  });

  it("answers nothing where the key store cannot be read", async () => {
    registeredIdentityNumbers.mockRejectedValue(new Error("no storage"));

    expect(await findDeviceApp(ASKING)).toBeUndefined();
  });
});
