import { beforeEach, describe, expect, it, vi } from "vitest";
// Mocked before the unit is imported: every one of these reaches for a browser
// (IndexedDB, the push manager, WebCrypto) and what is under test is which of
// them the registration decides to use.
vi.mock("./pushSubscription", () => ({
  currentDeviceSubscription: vi.fn(),
  relayOriginOf: vi.fn(() => "https://relay.example"),
  subscribeToPush: vi.fn(() =>
    Promise.resolve({ endpoint: FRESH } as PushSubscription),
  ),
  subscribedWith: (subscription: PushSubscription, key: Uint8Array) =>
    String(subscription.options.applicationServerKey) === String(key),
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
vi.mock("./browserActor", () => ({
  anonymousActor: vi.fn(() =>
    Promise.resolve({
      get_vapid_public_key: () => Promise.resolve([new Uint8Array(65)]),
    }),
  ),
  browserKeyActor: vi.fn(() => Promise.resolve({ set_webpush_subscription })),
}));

import {
  ensureRegisteredDevice,
  registerForAnyAnchor,
} from "./subscribeDevice";
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

const live = vi.mocked(currentDeviceSubscription);
const subscribe = vi.mocked(subscribeToPush);

/** What the canister publishes, which is what the actor mock above answers with. */
const OUR_KEY = new Uint8Array(65);
const ANOTHER_KEY = new Uint8Array(65).fill(2);

const held = (endpoint = HELD, applicationServerKey = OUR_KEY) =>
  ({
    endpoint,
    options: { applicationServerKey },
  }) as unknown as PushSubscription;

describe("ensureRegisteredDevice", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    subscribe.mockResolvedValue({ endpoint: FRESH } as PushSubscription);
  });

  // Subscribing drops the endpoint every other identity on this browser is
  // registered with, so a second identity has to register what is already there.
  it("registers the subscription this browser already holds", async () => {
    live.mockResolvedValue(held());
    await ensureRegisteredDevice(IDENTITY);

    expect(subscribe).not.toHaveBeenCalled();
    expect(set_webpush_subscription.mock.calls[0][0]).toMatchObject({
      endpoint: HELD,
    });
  });

  /**
   * The relay binds a subscription to the key it was minted with and refuses a push
   * carrying any other, so one made before Internet Identity held its own is dead and
   * reusing it would register an endpoint nothing can reach.
   */
  it("subscribes afresh where what it holds names another application server key", async () => {
    live.mockResolvedValue(held(HELD, ANOTHER_KEY));
    await ensureRegisteredDevice(IDENTITY);

    expect(subscribe).toHaveBeenCalledOnce();
    expect(set_webpush_subscription.mock.calls[0][0]).toMatchObject({
      endpoint: FRESH,
    });
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

const kept = describe("registerForAnyAnchor", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    subscribe.mockResolvedValue({ endpoint: FRESH } as PushSubscription);
    live.mockResolvedValue(held());
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

  it("stops where no other identity can answer the refusal", async () => {
    appCalls.mockResolvedValue({ Err: { InternalCanisterError: "no room" } });

    await expect(
      registerForAnyAnchor([IDENTITY, OTHER], app),
    ).rejects.toThrow();
    expect(appCalls).toHaveBeenCalledOnce();
  });

  it("stops at a failure another identity cannot answer", async () => {
    appCalls.mockRejectedValue(new Error("offline"));

    await expect(registerForAnyAnchor([IDENTITY, OTHER], app)).rejects.toThrow(
      "offline",
    );
    expect(appCalls).toHaveBeenCalledOnce();
  });
});
