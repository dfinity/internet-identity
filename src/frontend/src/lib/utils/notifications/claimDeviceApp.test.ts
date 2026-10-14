import { beforeEach, describe, expect, it, vi } from "vitest";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { DeviceApp } from "$lib/utils/notifications/deviceApp";

const findDeviceApp = vi.fn<() => Promise<DeviceApp | undefined>>();
const recordClaimedIdentity = vi.fn((_identityNumber?: bigint) =>
  Promise.resolve(),
);
const browserKeyActor = vi.fn();

vi.mock("$lib/utils/notifications/deviceRecord", () => ({
  recordClaimedIdentity: (identityNumber: bigint) =>
    recordClaimedIdentity(identityNumber),
}));
vi.mock("$lib/utils/notifications/deviceApp", () => ({
  findDeviceApp: () => findDeviceApp(),
}));
vi.mock("$lib/utils/notifications/browserActor", () => ({
  browserKeyActor: () => browserKeyActor(),
}));

const { claimDeviceApp } =
  await import("$lib/utils/notifications/claimDeviceApp");

const IDENTITY = BigInt(10_000);

const APP: DeviceApp = {
  appKey: Uint8Array.from([1, 2, 3]),
  endpoint: "https://web.push.apple.com/abc",
  vapidPublicKey: Uint8Array.from([4, 5, 6]),
  jwtSignatures: [Uint8Array.from([7])],
  jwtIssuedAtNs: BigInt("1700000000000000000"),
};

const actorAnswering = (reply: unknown) => {
  const claim_notification_app = vi.fn(() => Promise.resolve(reply));
  return {
    claim: claim_notification_app,
    actor: { claim_notification_app } as unknown as ActorSubclass<_SERVICE>,
  };
};

beforeEach(() => {
  vi.clearAllMocks();
  findDeviceApp.mockResolvedValue(APP);
});

describe("claimDeviceApp", () => {
  it("hands the canister the key and the subscription the app published", async () => {
    const { actor, claim } = actorAnswering({ Ok: null });

    expect(
      await claimDeviceApp({ identityNumber: IDENTITY, caller: actor }),
    ).toBe(true);

    expect(claim).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
      app_key: APP.appKey,
      endpoint: APP.endpoint,
      vapid_public_key: APP.vapidPublicKey,
      jwt_signatures: APP.jwtSignatures,
      jwt_issued_at_ns: APP.jwtIssuedAtNs,
    });
  });

  // The app is not running when this happens, so the list it reads on its next
  // wake-up is the only way it hears that another queue is now its to ask about.
  it("records the identity so the app knows to ask for its queue", async () => {
    const { actor } = actorAnswering({ Ok: null });

    await claimDeviceApp({ identityNumber: IDENTITY, caller: actor });

    expect(recordClaimedIdentity).toHaveBeenCalledWith(IDENTITY);
  });

  it("answers no where this device has no app", async () => {
    findDeviceApp.mockResolvedValue(undefined);
    const { actor, claim } = actorAnswering({ Ok: null });

    expect(
      await claimDeviceApp({ identityNumber: IDENTITY, caller: actor }),
    ).toBe(false);

    expect(claim).not.toHaveBeenCalled();
  });

  it("throws what the canister refused rather than reporting nothing to claim", async () => {
    const { actor } = actorAnswering({ Err: { InvalidBrowserKey: null } });

    await expect(
      claimDeviceApp({ identityNumber: IDENTITY, caller: actor }),
    ).rejects.toThrow();
    expect(recordClaimedIdentity).not.toHaveBeenCalled();
  });
});
