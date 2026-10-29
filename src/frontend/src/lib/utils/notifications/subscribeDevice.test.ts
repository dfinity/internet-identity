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
  subscribeToPush: vi.fn(() =>
    Promise.resolve({ endpoint: FRESH } as PushSubscription),
  ),
}));
// The derivation needs a real subscription and real WebCrypto ECDH; what it produces is
// covered where it lives.
vi.mock("./payloadKeys", () => ({
  derivePayloadKeys: () =>
    Promise.resolve({
      payloadIkm: new Uint8Array(32).fill(9),
      payloadKeyid: new Uint8Array(65).fill(4),
    }),
}));
vi.mock("./vapidPool", () => ({
  generateVapidKeypair: vi.fn(() =>
    Promise.resolve({ publicKeyRaw: new Uint8Array([9]), privateKey: {} }),
  ),
  signJwtPool: vi.fn(() => Promise.resolve([new Uint8Array([1])])),
}));
vi.mock("./browserActor", () => ({
  browserKeyActor: vi.fn(() => Promise.resolve({ set_webpush_subscription })),
}));

import {
  ensureRegisteredDevice,
  registerForAnyAnchor,
} from "./subscribeDevice";
import {
  loadVapidKey,
  storeVapidKey,
  type StoredVapidKey,
} from "./vapidKeyStore";
import { currentDeviceSubscription, subscribeToPush } from "./pushSubscription";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";

const HELD = "https://relay.example/held";
const FRESH = "https://relay.example/fresh";
const IDENTITY = BigInt(10_000);

/** The registration goes out under the browser key, not the identity's session. */
const set_webpush_subscription = vi.hoisted(() =>
  vi.fn((_args: { endpoint: string }) => Promise.resolve({ Ok: null })),
);

const stored = vi.mocked(loadVapidKey);
const live = vi.mocked(currentDeviceSubscription);
const subscribe = vi.mocked(subscribeToPush);

const heldKey = (endpoint: string) =>
  ({
    endpoint,
    privateKey: {},
    publicKeyRaw: new Uint8Array([9]),
  }) as unknown as StoredVapidKey;

describe("ensureRegisteredDevice", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    subscribe.mockResolvedValue({ endpoint: FRESH } as PushSubscription);
  });

  // Subscribing drops the endpoint every other identity on this browser is
  // registered with, so a second identity has to register what is already there.
  it("registers the subscription this browser already holds", async () => {
    stored.mockResolvedValue(heldKey(HELD));
    live.mockResolvedValue({ endpoint: HELD } as PushSubscription);
    await ensureRegisteredDevice(IDENTITY);

    expect(subscribe).not.toHaveBeenCalled();
    expect(set_webpush_subscription.mock.calls[0][0]).toMatchObject({
      endpoint: HELD,
    });
  });

  it("subscribes afresh when the key it holds names a dead endpoint", async () => {
    stored.mockResolvedValue(heldKey(HELD));
    live.mockResolvedValue(undefined);
    await ensureRegisteredDevice(IDENTITY);

    expect(subscribe).toHaveBeenCalledOnce();
    expect(set_webpush_subscription.mock.calls[0][0]).toMatchObject({
      endpoint: FRESH,
    });
  });

  it("subscribes afresh for a browser that holds nothing", async () => {
    stored.mockResolvedValue(undefined);
    live.mockResolvedValue(undefined);
    await ensureRegisteredDevice(IDENTITY);

    expect(subscribe).toHaveBeenCalledOnce();
  });
});

const OTHER = BigInt(20_000);

const appCalls = vi.fn(
  (_args: { anchor_number: bigint; endpoint: string }): Promise<unknown> =>
    Promise.resolve({ Ok: null }),
);

/** The Home Screen app's own actor, which is what it signs these calls as. */
const app = {
  set_webpush_subscription: appCalls,
} as unknown as ActorSubclass<_SERVICE>;

/** The anchors the canister refuses, which both come back as a key that reaches no browser. */
const refusing = (...anchors: bigint[]) =>
  appCalls.mockImplementation(({ anchor_number }) =>
    anchors.includes(anchor_number)
      ? Promise.resolve({ Err: { InvalidBrowserKey: null } })
      : Promise.resolve({ Ok: null }),
  );

const kept = vi.mocked(storeVapidKey);

describe("registerForAnyAnchor", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    subscribe.mockResolvedValue({ endpoint: FRESH } as PushSubscription);
    stored.mockResolvedValue(heldKey(HELD));
    live.mockResolvedValue({ endpoint: HELD } as PushSubscription);
    appCalls.mockResolvedValue({ Ok: null });
  });

  it("registers under the first identity the canister accepts", async () => {
    await expect(registerForAnyAnchor([IDENTITY, OTHER], app)).resolves.toBe(
      true,
    );

    expect(appCalls).toHaveBeenCalledOnce();
    expect(appCalls.mock.calls[0][0]).toMatchObject({
      anchor_number: IDENTITY,
    });
  });

  it("carries on past an identity the canister refuses", async () => {
    refusing(IDENTITY);

    await expect(registerForAnyAnchor([IDENTITY, OTHER], app)).resolves.toBe(
      true,
    );

    expect(appCalls.mock.calls[1][0]).toMatchObject({ anchor_number: OTHER });
  });

  it("mints one subscription for the whole walk", async () => {
    stored.mockResolvedValue(undefined);
    live.mockResolvedValue(undefined);
    refusing(IDENTITY);

    await registerForAnyAnchor([IDENTITY, OTHER], app);

    expect(subscribe).toHaveBeenCalledOnce();
    expect(appCalls.mock.calls[0][0]).toMatchObject({ endpoint: FRESH });
    expect(appCalls.mock.calls[1][0]).toMatchObject({ endpoint: FRESH });
  });

  it("answers no where every identity refuses", async () => {
    refusing(IDENTITY, OTHER);

    await expect(registerForAnyAnchor([IDENTITY, OTHER], app)).resolves.toBe(
      false,
    );
  });

  it("keeps a minted key only once the canister has taken it", async () => {
    stored.mockResolvedValue(undefined);
    live.mockResolvedValue(undefined);
    refusing(IDENTITY, OTHER);

    await registerForAnyAnchor([IDENTITY, OTHER], app);

    expect(kept).not.toHaveBeenCalled();
  });

  it.each([{ StaleJwtPool: null }, { InternalCanisterError: "no room" }])(
    "stops where no other identity can answer the refusal %s",
    async (refusal) => {
      appCalls.mockResolvedValue({ Err: refusal });

      await expect(
        registerForAnyAnchor([IDENTITY, OTHER], app),
      ).rejects.toThrow();
      expect(appCalls).toHaveBeenCalledOnce();
    },
  );

  it("stops at a failure another identity cannot answer", async () => {
    appCalls.mockRejectedValue(new Error("offline"));

    await expect(registerForAnyAnchor([IDENTITY, OTHER], app)).rejects.toThrow(
      "offline",
    );
    expect(appCalls).toHaveBeenCalledOnce();
  });
});
