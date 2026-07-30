import { beforeEach, describe, expect, it, vi } from "vitest";
import {
  notificationsEnabledFor,
  notificationsGloballyGranted,
  recordNotifOptInDecision,
  shouldOfferNotifications,
} from "./notifOptIn";

const IDENTITY = BigInt(10_000);
const OTHER_IDENTITY = BigInt(10_001);
const ORIGIN = "https://app.example";
const OTHER_ORIGIN = "https://other.example";

/** In-memory `Storage`, so these tests don't depend on the environment
 *  supplying a working `localStorage`. */
const fakeStorage = (): Storage => {
  const entries = new Map<string, string>();
  return {
    get length() {
      return entries.size;
    },
    clear: () => entries.clear(),
    getItem: (key: string) => entries.get(key) ?? null,
    key: (index: number) => [...entries.keys()][index] ?? null,
    removeItem: (key: string) => entries.delete(key),
    setItem: (key: string, value: string) => entries.set(key, value),
  };
};

const setPermission = (permission: NotificationPermission): void => {
  vi.stubGlobal("Notification", { permission });
};

/** A browser that supports Web Push, per the checks in `notifOptIn.ts`. */
const setPushSupported = (): void => {
  vi.stubGlobal("navigator", { serviceWorker: {} });
  vi.stubGlobal("window", { PushManager: class {} });
};

beforeEach(() => {
  vi.unstubAllGlobals();
  vi.stubGlobal("localStorage", fakeStorage());
  setPushSupported();
  setPermission("default");
});

describe("shouldOfferNotifications", () => {
  it("offers when the identity has not answered for this origin", () => {
    expect(shouldOfferNotifications(IDENTITY, ORIGIN)).toBe(true);
  });

  it("does not offer again after the user enabled", () => {
    recordNotifOptInDecision(IDENTITY, ORIGIN, "enabled");

    expect(shouldOfferNotifications(IDENTITY, ORIGIN)).toBe(false);
  });

  it("does not offer again after the user dismissed", () => {
    recordNotifOptInDecision(IDENTITY, ORIGIN, "dismissed");

    expect(shouldOfferNotifications(IDENTITY, ORIGIN)).toBe(false);
  });

  it("still offers for a different dApp, since consent is per origin", () => {
    recordNotifOptInDecision(IDENTITY, ORIGIN, "dismissed");

    expect(shouldOfferNotifications(IDENTITY, OTHER_ORIGIN)).toBe(true);
  });

  it("still offers for a different identity on the same browser", () => {
    recordNotifOptInDecision(IDENTITY, ORIGIN, "enabled");

    expect(shouldOfferNotifications(OTHER_IDENTITY, ORIGIN)).toBe(true);
  });

  it("does not offer when notification permission is already denied", () => {
    // A denied permission cannot be re-requested from script, so the screen's
    // primary action would silently do nothing.
    setPermission("denied");

    expect(shouldOfferNotifications(IDENTITY, ORIGIN)).toBe(false);
  });

  it("does not offer when the browser has no service worker", () => {
    vi.stubGlobal("navigator", {});

    expect(shouldOfferNotifications(IDENTITY, ORIGIN)).toBe(false);
  });

  it("does not offer when the browser has no PushManager", () => {
    vi.stubGlobal("window", {});

    expect(shouldOfferNotifications(IDENTITY, ORIGIN)).toBe(false);
  });

  it("does not offer when the decision cannot be persisted", () => {
    // Without storage the answer can't be remembered, so asking would repeat on
    // every sign-in with no way for the user to stop it.
    vi.stubGlobal("localStorage", {
      ...fakeStorage(),
      getItem: () => {
        throw new Error("storage disabled");
      },
    });

    expect(shouldOfferNotifications(IDENTITY, ORIGIN)).toBe(false);
  });
});

describe("notificationsGloballyGranted", () => {
  it("is false before the browser permission is granted", () => {
    expect(notificationsGloballyGranted()).toBe(false);
  });

  it("is true once the browser permission is granted", () => {
    setPermission("granted");

    expect(notificationsGloballyGranted()).toBe(true);
  });

  it("is false when denied", () => {
    setPermission("denied");

    expect(notificationsGloballyGranted()).toBe(false);
  });

  it("is false when the browser cannot do push at all", () => {
    setPermission("granted");
    vi.stubGlobal("navigator", {});

    expect(notificationsGloballyGranted()).toBe(false);
  });

  it("is independent of any per-app decision", () => {
    // The two layers are separate: dismissing one app must not read as "the
    // browser permission was never granted", or that app's next sign-in would
    // wrongly show the first-run explainer again.
    setPermission("granted");
    recordNotifOptInDecision(IDENTITY, ORIGIN, "dismissed");

    expect(notificationsGloballyGranted()).toBe(true);
  });
});

describe("notificationsEnabledFor", () => {
  it("is false before the user has answered", () => {
    setPermission("granted");

    expect(notificationsEnabledFor(IDENTITY, ORIGIN)).toBe(false);
  });

  it("is true once enabled for that identity and origin", () => {
    setPermission("granted");
    recordNotifOptInDecision(IDENTITY, ORIGIN, "enabled");

    expect(notificationsEnabledFor(IDENTITY, ORIGIN)).toBe(true);
  });

  it("is false when the user dismissed instead", () => {
    setPermission("granted");
    recordNotifOptInDecision(IDENTITY, ORIGIN, "dismissed");

    expect(notificationsEnabledFor(IDENTITY, ORIGIN)).toBe(false);
  });

  it("is scoped to the origin it was enabled for", () => {
    setPermission("granted");
    recordNotifOptInDecision(IDENTITY, ORIGIN, "enabled");

    expect(notificationsEnabledFor(IDENTITY, OTHER_ORIGIN)).toBe(false);
  });

  it("is scoped to the identity it was enabled for", () => {
    setPermission("granted");
    recordNotifOptInDecision(IDENTITY, ORIGIN, "enabled");

    expect(notificationsEnabledFor(OTHER_IDENTITY, ORIGIN)).toBe(false);
  });

  it("is false when the browser permission was revoked after enabling", () => {
    // The stored "enabled" only means the user said yes here; the browser is
    // the authority on whether anything can still be delivered, so claiming
    // "On" after a revoke would be a lie.
    recordNotifOptInDecision(IDENTITY, ORIGIN, "enabled");
    setPermission("denied");

    expect(notificationsEnabledFor(IDENTITY, ORIGIN)).toBe(false);
  });
});

describe("recordNotifOptInDecision", () => {
  it("does not throw when storage is unavailable", () => {
    vi.stubGlobal("localStorage", {
      ...fakeStorage(),
      setItem: () => {
        throw new Error("storage disabled");
      },
    });

    expect(() =>
      recordNotifOptInDecision(IDENTITY, ORIGIN, "enabled"),
    ).not.toThrow();
  });
});
