import { beforeEach, describe, expect, it, vi } from "vitest";

// Keep the browser/idb deps out of the test; the reconcile is pure control flow
// over these seams.
vi.mock("./pushSubscription", () => ({
  isPushSupported: vi.fn(() => true),
  currentDeviceSubscription: vi.fn(),
  relayOriginOf: (endpoint: string) => new URL(endpoint).origin,
  requestNotificationPermission: vi.fn(),
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
vi.mock("./turnedOff", () => ({
  notificationsTurnedOff: vi.fn(() => Promise.resolve(false)),
}));

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { reconcileDeviceNotifications } from "./deviceNotifications";
import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import {
  registerStoredDevice,
  subscribeAndRegisterDevice,
} from "./subscribeDevice";
import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";
import { notificationsTurnedOff } from "./turnedOff";

const ENDPOINT = "https://relay.example/abc";
/** What the canister answers for a browser it holds a registration for. */
const registeredAt = (endpoint = ENDPOINT) => [{ endpoint }];

const sub = (endpoint = ENDPOINT) => ({ endpoint }) as PushSubscription;

const actor = () => ({
  get_webpush_subscription_status: vi.fn<() => Promise<{ endpoint: string }[]>>(
    () => Promise.resolve([]),
  ),
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
  vi.mocked(notificationsTurnedOff).mockResolvedValue(false);
  vi.stubGlobal("Notification", { permission: "granted" });
});

describe("reconcileDeviceNotifications", () => {
  it("registers nothing for an identity that switched notifications off", async () => {
    vi.mocked(notificationsTurnedOff).mockResolvedValue(true);
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    const a = actor();
    await run(a);
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
    expect(registerStoredDevice).not.toHaveBeenCalled();
    expect(a.get_webpush_subscription_status).not.toHaveBeenCalled();
  });

  it("does nothing when permission isn't granted", async () => {
    vi.stubGlobal("Notification", { permission: "default" });
    const a = actor();
    await run(a);
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
    expect(a.get_webpush_subscription_status).not.toHaveBeenCalled();
  });

  it("does nothing for a browser that never enabled notifications", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(undefined);
    const a = actor();
    await run(a);
    // Holding neither a subscription nor a registration is what never having turned
    // them on looks like, and subscribing here would turn them on by itself.
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
    expect(registerStoredDevice).not.toHaveBeenCalled();
  });

  it("leaves a registration that already names this subscription alone", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue(registeredAt());
    await run(a);
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  /**
   * The subscription belongs to the browser, not to one identity, so a second identity
   * with no row of its own registers what is already there. Rotating
   * would unsubscribe the endpoint the first identity's row still names.
   */
  it("registers the browser's existing subscription for an identity with no row", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
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
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue(
      registeredAt("https://relay.example/someone-else"),
    );
    await run(a);
    expect(registerStoredDevice).toHaveBeenCalledOnce();
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  /** The row is keyed by the browser, so registering again overwrites the endpoint. */
  it("re-registers a rotated endpoint in place, without unsubscribing", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(
      sub("https://relay.example/rotated"),
    );
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue(registeredAt());
    await run(a);
    expect(a.remove_webpush_subscription).not.toHaveBeenCalled();
    // The browser holds one, so it is registered rather than replaced: subscribing
    // again would take the endpoint every other identity here is registered with.
    expect(registerStoredDevice).toHaveBeenCalledOnce();
    expect(subscribeAndRegisterDevice).not.toHaveBeenCalled();
  });

  /** The browser the caller signs as is the browser the canister answers for. */
  it("asks as the browser, without naming one", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
    const a = actor();
    a.get_webpush_subscription_status.mockResolvedValue(registeredAt());
    await run(a);
    expect(browserKeyActor).toHaveBeenCalledWith(BigInt(1));
    expect(a.get_webpush_subscription_status).toHaveBeenCalledWith({
      anchor_number: BigInt(1),
    });
  });

  it("does nothing for a browser no sign-in has registered", async () => {
    vi.mocked(currentDeviceSubscription).mockResolvedValue(sub());
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
    const a = actor();
    // A registration the canister still holds is what says this identity had them on.
    a.get_webpush_subscription_status.mockResolvedValue(registeredAt());
    await run(a);
    expect(subscribeAndRegisterDevice).toHaveBeenCalledOnce();
  });
});
