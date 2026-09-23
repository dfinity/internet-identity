import { beforeEach, describe, expect, it, vi } from "vitest";

// Keep the browser/idb deps out of the test; the reconcile is pure control flow
// over these seams.
vi.mock("./pushSubscription", () => ({
  isPushSupported: vi.fn(() => true),
  currentDeviceSubscription: vi.fn(),
  relayOriginOf: (endpoint: string) => new URL(endpoint).origin,
  requestNotificationPermission: vi.fn(),
}));
vi.mock("./vapidKeyStore", () => ({
  loadVapidKey: vi.fn(),
  purgeVapidKey: vi.fn(),
}));
vi.mock("./subscribeDevice", () => ({
  subscribeAndRegisterDevice: vi.fn(() =>
    Promise.resolve("https://relay.example/new"),
  ),
  registerStoredDevice: vi.fn(() => Promise.resolve(ENDPOINT)),
}));
vi.mock("./browserActor", () => {
  class UnregisteredBrowserError extends Error {}
  return { browserKeyActor: vi.fn(), UnregisteredBrowserError };
});

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { reconcileDeviceNotifications } from "./deviceNotifications";
import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import { loadVapidKey } from "./vapidKeyStore";
import {
  registerStoredDevice,
  subscribeAndRegisterDevice,
} from "./subscribeDevice";
import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";

const ENDPOINT = "https://relay.example/abc";
const DAY_NS = BigInt(24 * 60 * 60) * BigInt(1_000_000_000);
/** A registration on `endpoint` whose pool of 30 windows was minted `daysAgo` ago. */
const pool = (daysAgo: number, endpoint = ENDPOINT) => [
  {
    endpoint,
    pool_len: 30,
    issued_at_ns:
      BigInt(Date.now()) * BigInt(1_000_000) - BigInt(daysAgo) * DAY_NS,
  },
];

const key = (endpoint = ENDPOINT) => ({
  endpoint,
  privateKey: {} as CryptoKey,
  publicKeyRaw: new Uint8Array(),
});
const sub = (endpoint = ENDPOINT) => ({ endpoint }) as PushSubscription;

const actor = () => ({
  get_webpush_subscription_status: vi.fn(),
  remove_webpush_subscription: vi.fn(() => Promise.resolve({ Ok: null })),
});

const run = (a: ReturnType<typeof actor>) => {
  vi.mocked(browserKeyActor).mockResolvedValue(
    a as unknown as ActorSubclass<_SERVICE>,
  );
  return reconcileDeviceNotifications(BigInt(1));
};

beforeEach(() => {
  vi.clearAllMocks();
  vi.mocked(isPushSupported).mockReturnValue(true);
  vi.stubGlobal("Notification", { permission: "granted" });
});

describe("reconcileDeviceNotifications", () => {
  it("does nothing when permission isn't granted", async () => {
    vi.stubGlobal("Notification", { permission: "default" });
    const a = actor();
    await run(a);
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
    expect(a.get_webpush_subscription_status).not.toHaveBeenCalled();
  });

  it("does nothing for a browser that never enabled notifications", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(undefined);
    vi.mocked(loadVapidKey).mockResolvedValue(undefined);
    const a = actor();
    await run(a);
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
    expect(a.get_webpush_subscription_status).not.toHaveBeenCalled();
  });

  it("leaves a freshly minted pool alone", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue(pool(1));
    await run(a);
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  /**
   * The pool is spent by elapsed time, not by use, so its length never changes and a
   * count of unused signatures cannot say whether a top-up is due.
   */
  it("refreshes a pool that has nearly elapsed, though it is still full length", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue(pool(25));
    await run(a);
    expect(registerStoredDevice).toHaveBeenCalledOnce();
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  /**
   * The subscription and its VAPID key belong to the browser, not to one identity, so a
   * second identity with no row of its own registers what is already there. Rotating
   * would unsubscribe the endpoint the first identity's row still names.
   */
  it("registers the browser's existing subscription for an identity with no row", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue([]);
    await run(a);
    expect(registerStoredDevice).toHaveBeenCalledOnce();
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  /**
   * The subscription is shared, so another identity re-subscribing leaves this one
   * registered on an endpoint that is gone. Registering what the browser now holds
   * repairs it; re-subscribing would break the identity that just fixed itself.
   */
  it("re-registers an identity another identity's re-subscribe left behind", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue(
      pool(1, "https://relay.example/someone-else"),
    );
    await run(a);
    expect(registerStoredDevice).toHaveBeenCalledOnce();
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  /** The row is keyed by the browser, so re-subscribing overwrites the endpoint. */
  it("re-registers a rotated endpoint in place, without unsubscribing", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(
      sub("https://relay.example/rotated"),
    );
    vi.mocked(loadVapidKey).mockResolvedValue(key(ENDPOINT));
    const a = actor();
    await run(a);
    expect(a.remove_webpush_subscription).not.toHaveBeenCalled();
    expect(subscribeAndRegisterDevice).toHaveBeenCalledOnce();
  });

  /** The browser the caller signs as is the browser the canister answers for. */
  it("asks as the browser, without naming one", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue(pool(1));
    await run(a);
    expect(browserKeyActor).toHaveBeenCalledWith(BigInt(1));
    expect(a.get_webpush_subscription_status).toHaveBeenCalledWith({
      anchor_number: BigInt(1),
    });
  });

  it("does nothing for a browser no sign-in has registered", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    vi.mocked(browserKeyActor).mockRejectedValue(
      new UnregisteredBrowserError(),
    );
    await reconcileDeviceNotifications(BigInt(1));
    expect(a.get_webpush_subscription_status).not.toHaveBeenCalled();
    expect(registerStoredDevice).not.toHaveBeenCalled();
  });

  it("re-subscribes when the browser dropped its subscription", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(undefined);
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    await run(a);
    expect(subscribeAndRegisterDevice).toHaveBeenCalledOnce();
  });
});
