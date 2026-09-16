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
}));
vi.mock("$lib/stores/browser-key.store", () => ({
  currentBrowserId: vi.fn(() => Promise.resolve(7)),
}));
vi.mock("./browserActor", () => ({
  browserKeyActor: vi.fn(() => Promise.resolve(browserActor)),
}));
// `windowsRemaining` is pure and is the unit under test here, so it stays real.
vi.mock("./vapidPool", async (importOriginal) => ({
  ...(await importOriginal<typeof import("./vapidPool")>()),
  signJwtPool: vi.fn(() => Promise.resolve([new Uint8Array([1])])),
}));

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { reconcileDeviceNotifications } from "./deviceNotifications";
import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import { loadVapidKey } from "./vapidKeyStore";
import { subscribeAndRegisterDevice } from "./subscribeDevice";
import { currentBrowserId } from "$lib/stores/browser-key.store";

const ENDPOINT = "https://relay.example/abc";
const DAY_NS = BigInt(24 * 60 * 60) * BigInt(1_000_000_000);
/** A pool of 30 windows minted `daysAgo` days ago. */
const pool = (daysAgo: number) => [
  {
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

/** The top-up goes out under the browser key, not the identity's session. */
const browserActor = vi.hoisted(() => ({
  set_webpush_subscription: vi.fn(() => Promise.resolve({ Ok: null })),
}));

const run = (a: ReturnType<typeof actor>) =>
  reconcileDeviceNotifications(
    BigInt(1),
    a as unknown as ActorSubclass<_SERVICE>,
  );

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
    expect(browserActor.set_webpush_subscription).not.toHaveBeenCalled();
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
    expect(browserActor.set_webpush_subscription).toHaveBeenCalledOnce();
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  it("re-subscribes when the canister no longer knows the endpoint", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue([]);
    await run(a);
    expect(subscribeAndRegisterDevice).toHaveBeenCalledOnce();
    expect(browserActor.set_webpush_subscription).not.toHaveBeenCalled();
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

  it("asks about the browser the store names, not the endpoint", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue(pool(1));
    await run(a);
    expect(a.get_webpush_subscription_status).toHaveBeenCalledWith({
      anchor_number: BigInt(1),
      browser_id: 7,
    });
  });

  it("does nothing for a browser no sign-in has registered", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    vi.mocked(currentBrowserId).mockResolvedValueOnce(undefined);
    const a = actor();
    await run(a);
    expect(a.get_webpush_subscription_status).not.toHaveBeenCalled();
    expect(browserActor.set_webpush_subscription).not.toHaveBeenCalled();
  });

  it("re-subscribes when the browser dropped its subscription", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(undefined);
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    await run(a);
    expect(subscribeAndRegisterDevice).toHaveBeenCalledOnce();
  });
});
