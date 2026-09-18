import { beforeEach, describe, expect, it, vi } from "vitest";
// Mocked before the unit is imported: every one of these reaches for a browser
// (IndexedDB, the push manager, WebCrypto) and what is under test is which of
// them the registration decides to use.
vi.mock("./vapidKeyStore", () => ({
  loadVapidKey: vi.fn(),
  storeVapidKey: vi.fn(() => Promise.resolve()),
}));
vi.mock("./pushSubscription", () => ({
  currentDeviceSubscription: vi.fn(),
  relayOriginOf: vi.fn(() => "https://relay.example"),
  subscribeToPush: vi.fn(() => Promise.resolve(FRESH)),
}));
vi.mock("./vapidPool", () => ({
  generateVapidKeypair: vi.fn(() =>
    Promise.resolve({ publicKeyRaw: new Uint8Array([9]), privateKey: {} }),
  ),
  signJwtPool: vi.fn(() => Promise.resolve([new Uint8Array([1])])),
}));
vi.mock("$lib/stores/browser-key.store", () => ({
  webPushProof: vi.fn(() =>
    Promise.resolve({
      browserId: 1,
      browserKey: new Uint8Array([2]),
      signature: new Uint8Array([3]),
    }),
  ),
}));

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { ensureRegisteredDevice } from "./subscribeDevice";
import { loadVapidKey, type StoredVapidKey } from "./vapidKeyStore";
import { currentDeviceSubscription, subscribeToPush } from "./pushSubscription";

const HELD = "https://relay.example/held";
const FRESH = "https://relay.example/fresh";
const IDENTITY = BigInt(10_000);

const stored = vi.mocked(loadVapidKey);
const live = vi.mocked(currentDeviceSubscription);
const subscribe = vi.mocked(subscribeToPush);

const heldKey = (endpoint: string) =>
  ({
    endpoint,
    privateKey: {},
    publicKeyRaw: new Uint8Array([9]),
  }) as unknown as StoredVapidKey;

const actorRecording = () => {
  const webpush_subscribe_device = vi.fn((_args: { endpoint: string }) =>
    Promise.resolve({ Ok: null }),
  );
  return {
    actor: { webpush_subscribe_device } as unknown as ActorSubclass<_SERVICE>,
    registered: webpush_subscribe_device,
  };
};

describe("ensureRegisteredDevice", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    subscribe.mockResolvedValue(FRESH);
  });

  // Subscribing drops the endpoint every other identity on this browser is
  // registered with, so a second identity has to register what is already there.
  it("registers the subscription this browser already holds", async () => {
    stored.mockResolvedValue(heldKey(HELD));
    live.mockResolvedValue({ endpoint: HELD } as PushSubscription);
    const { actor, registered } = actorRecording();

    await ensureRegisteredDevice(IDENTITY, actor);

    expect(subscribe).not.toHaveBeenCalled();
    expect(registered.mock.calls[0][0]).toMatchObject({ endpoint: HELD });
  });

  it("subscribes afresh when the key it holds names a dead endpoint", async () => {
    stored.mockResolvedValue(heldKey(HELD));
    live.mockResolvedValue(undefined);
    const { actor, registered } = actorRecording();

    await ensureRegisteredDevice(IDENTITY, actor);

    expect(subscribe).toHaveBeenCalledOnce();
    expect(registered.mock.calls[0][0]).toMatchObject({ endpoint: FRESH });
  });

  it("subscribes afresh for a browser that holds nothing", async () => {
    stored.mockResolvedValue(undefined);
    live.mockResolvedValue(undefined);
    const { actor } = actorRecording();

    await ensureRegisteredDevice(IDENTITY, actor);

    expect(subscribe).toHaveBeenCalledOnce();
  });
});
