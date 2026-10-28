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
  browserKeyActor: vi.fn((identityNumber: bigint) =>
    Promise.resolve({
      set_webpush_subscription: (args: { endpoint: string }) =>
        set_webpush_subscription({ ...args, anchor_number: identityNumber }),
      get_webpush_subscription_status: () =>
        Promise.resolve(registrationOf(identityNumber)),
    }),
  ),
}));
vi.mock("$lib/stores/browser-key.store", () => ({
  registeredIdentityNumbers: () => Promise.resolve(signedInHere),
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
  vi.fn((_args: { endpoint: string; anchor_number?: bigint }) =>
    Promise.resolve({ Ok: null }),
  ),
);

/** Every identity that has signed in on this browser, which is not the same set as the
 *  identities that turned notifications on. */
let signedInHere: bigint[] = [];
/** The identities the canister holds a registration for, by anchor. */
let registeredHere: bigint[] = [];
const registrationOf = (identityNumber: bigint) =>
  registeredHere.includes(identityNumber) ? [{ endpoint: HELD }] : [];

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
    signedInHere = [];
    registeredHere = [];
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
   * Subscribing took the endpoint they were registered with, so an identity left naming
   * it reaches nothing and only its own owner visiting settings would ever find out.
   */
  it("moves the other identities that were registered here onto the new subscription", async () => {
    live.mockResolvedValue(undefined);
    signedInHere = [IDENTITY, OTHER];
    registeredHere = [IDENTITY, OTHER];

    await ensureRegisteredDevice(IDENTITY);

    expect(
      set_webpush_subscription.mock.calls.map((call) => call[0].anchor_number),
    ).toEqual([IDENTITY, OTHER]);
  });

  /** Writing one here would turn notifications on in the name of someone who never asked. */
  it("leaves an identity that holds no registration alone", async () => {
    live.mockResolvedValue(undefined);
    signedInHere = [IDENTITY, OTHER];
    registeredHere = [IDENTITY];

    await ensureRegisteredDevice(IDENTITY);

    expect(
      set_webpush_subscription.mock.calls.map((call) => call[0].anchor_number),
    ).toEqual([IDENTITY]);
  });

  /** Nothing was taken from anyone, so there is nothing to move. */
  it("leaves the others alone where it reused what this browser held", async () => {
    live.mockResolvedValue(held());
    signedInHere = [IDENTITY, OTHER];
    registeredHere = [IDENTITY, OTHER];

    await ensureRegisteredDevice(IDENTITY);

    expect(
      set_webpush_subscription.mock.calls.map((call) => call[0].anchor_number),
    ).toEqual([IDENTITY]);
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

describe("registerForAnyAnchor", () => {
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
