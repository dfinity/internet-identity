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
  webpush_jwt_pool_status: vi.fn(),
  webpush_refresh_jwts: vi.fn(() => Promise.resolve({ Ok: null })),
  webpush_unsubscribe_device: vi.fn(() => Promise.resolve({ Ok: null })),
});

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
    expect(a.webpush_jwt_pool_status).not.toHaveBeenCalled();
  });

  it("does nothing for a browser that never enabled notifications", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(undefined);
    vi.mocked(loadVapidKey).mockResolvedValue(undefined);
    const a = actor();
    await run(a);
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
    expect(a.webpush_jwt_pool_status).not.toHaveBeenCalled();
  });

  it("leaves a freshly minted pool alone", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.webpush_jwt_pool_status.mockResolvedValue(pool(1));
    await run(a);
    expect(a.webpush_refresh_jwts).not.toHaveBeenCalled();
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  /**
   * The pool is spent by elapsed time, not by use, so its length never changes.
   * An earlier version gated on a canister-reported count of unused signatures,
   * which sat at 30 forever: this case took the "healthy, do nothing" branch and
   * the device went silent when the pool ran out five days later.
   */
  it("refreshes a pool that has nearly elapsed, though it is still full length", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.webpush_jwt_pool_status.mockResolvedValue(pool(25));
    await run(a);
    expect(a.webpush_refresh_jwts).toHaveBeenCalledOnce();
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  it("re-subscribes when the canister no longer knows the endpoint", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    a.webpush_jwt_pool_status.mockResolvedValue([]);
    await run(a);
    expect(subscribeAndRegisterDevice).toHaveBeenCalledOnce();
    expect(a.webpush_refresh_jwts).not.toHaveBeenCalled();
  });

  it("re-registers a rotated endpoint and drops the stale one", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(
      sub("https://relay.example/rotated"),
    );
    vi.mocked(loadVapidKey).mockResolvedValue(key(ENDPOINT));
    const a = actor();
    await run(a);
    expect(a.webpush_unsubscribe_device).toHaveBeenCalledWith(
      BigInt(1),
      ENDPOINT,
    );
    expect(subscribeAndRegisterDevice).toHaveBeenCalledOnce();
  });

  it("re-subscribes when the browser dropped its subscription", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(undefined);
    vi.mocked(loadVapidKey).mockResolvedValue(key());
    const a = actor();
    await run(a);
    expect(subscribeAndRegisterDevice).toHaveBeenCalledOnce();
  });
});
