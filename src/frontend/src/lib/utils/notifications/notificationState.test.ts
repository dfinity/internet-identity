import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
// vapidKeyStore opens an IndexedDB store at import; the resolver under test
// never touches it. The decline cooldown is mocked so the logic stays pure.
vi.mock("./vapidKeyStore", () => ({ loadVapidKey: vi.fn() }));
vi.mock("./notificationDiagnostics", () => ({ recordPermission: vi.fn() }));
vi.mock("$lib/utils/describeBrowser", () => ({
  browserAndSystem: vi.fn(() => ({
    os: { Macos: null },
    brand: { Chrome: null },
  })),
}));
vi.mock("./browserActor", () => {
  class UnregisteredBrowserError extends Error {}
  return { browserKeyActor: vi.fn(), UnregisteredBrowserError };
});
vi.mock("./pushSubscription", () => ({
  isPushSupported: vi.fn(() => true),
  currentDeviceSubscription: vi.fn(),
}));

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  notificationsNeedInstallHere,
  notificationsUnavailableHere,
  readBrowserPushState,
  readDeviceState,
  readGranted,
  resolveOptIn,
  resolveOptInScreen,
  watchNotificationPermission,
  type BrowserPushState,
  type DeviceNotificationState,
} from "./notificationState";
import { browserAndSystem } from "$lib/utils/describeBrowser";
import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import { loadVapidKey } from "./vapidKeyStore";
import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";

const ORIGIN = "https://app.example";
const IDENTITY = BigInt(10_000);

const state = (
  over: Partial<DeviceNotificationState>,
): DeviceNotificationState => ({
  supported: true,
  permission: "default",
  subscribed: false,
  registered: false,
  ...over,
});

describe("resolveOptInScreen", () => {
  it("skips a browser that cannot do notifications at all", () => {
    expect(resolveOptInScreen(state({ supported: false }), false)).toBe("skip");
  });

  it("skips where this app is allowed and this browser delivers", () => {
    expect(
      resolveOptInScreen(
        state({ permission: "granted", subscribed: true, registered: true }),
        true,
      ),
    ).toBe("skip");
  });

  /** A permission reset to "default" leaves the rows in place while the browser shows
   *  nothing, so the registration alone is not delivery. */
  it("asks again where the permission was reset but the rows remain", () => {
    expect(
      resolveOptInScreen(
        state({ permission: "default", subscribed: true, registered: true }),
        true,
      ),
    ).toBe("enable");
  });

  it("asks where this browser is not registered, however the app stands", () => {
    expect(resolveOptInScreen(state({ registered: false }), true)).toBe(
      "enable",
    );
    expect(resolveOptInScreen(state({ registered: false }), false)).toBe(
      "enable",
    );
  });

  it("asks where the browser delivers but this app is not allowed", () => {
    expect(
      resolveOptInScreen(
        state({ permission: "granted", subscribed: true, registered: true }),
        false,
      ),
    ).toBe("enable");
  });

  /** The guidance for a refusal is reached by asking, so that the user sees what the
   *  question was before being sent to browser settings. */
  it("asks where the permission was refused", () => {
    expect(resolveOptInScreen(state({ permission: "denied" }), false)).toBe(
      "enable",
    );
  });
});

describe("notificationsUnavailableHere", () => {
  it("is false where the browser can deliver notifications", () => {
    expect(notificationsUnavailableHere()).toBe(false);
  });

  it.each([["Ios"], ["Ipados"]])("is true on %s", (os) => {
    vi.mocked(browserAndSystem).mockReturnValue({
      os: { [os]: null },
      brand: { Safari: null },
    } as never);
    expect(notificationsUnavailableHere()).toBe(true);
  });
});

describe("readBrowserPushState and readDeviceState", () => {
  const ENDPOINT = "https://relay.example/held";

  /** A canister that reports a registration on `endpoint`, or none at all. */
  const reporting = (endpoint?: string) => {
    vi.mocked(browserKeyActor).mockResolvedValue({
      get_webpush_subscription_status: vi.fn(() =>
        Promise.resolve(
          endpoint === undefined
            ? []
            : [{ endpoint, pool_len: 30, issued_at_ns: BigInt(0) }],
        ),
      ),
    } as unknown as ActorSubclass<_SERVICE>);
  };

  /** The two halves as the opt-in runs them: the browser probe, then this
   *  identity's registration read against the endpoint it found. */
  const deviceState = async () => {
    const pushState = await readBrowserPushState();
    if (pushState === undefined) {
      throw new Error("the browser probe answered nothing");
    }
    return readDeviceState(IDENTITY, pushState);
  };

  beforeEach(() => {
    // Call counts are what the "reads no further" cases assert on, and the
    // implementations below are set after, so clearing leaves them in place.
    vi.clearAllMocks();
    // Stated rather than inherited: `clearAllMocks` keeps implementations, so a case
    // that set this to iOS leaves every later one resolving for iOS, where the answer
    // is the Home Screen install instead of a prompt.
    vi.mocked(browserAndSystem).mockReturnValue({
      os: { Macos: null },
      brand: { Chrome: null },
    });
    vi.stubGlobal("Notification", { permission: "granted" });
    vi.mocked(isPushSupported).mockReturnValue(true);
    vi.mocked(currentDeviceSubscription).mockResolvedValue({
      endpoint: ENDPOINT,
    } as PushSubscription);
    vi.mocked(loadVapidKey).mockResolvedValue({
      endpoint: ENDPOINT,
      privateKey: {} as CryptoKey,
      publicKeyRaw: new Uint8Array(),
    });
  });

  it("is registered when the canister names the endpoint this browser holds", async () => {
    reporting(ENDPOINT);
    await expect(deviceState()).resolves.toMatchObject({
      subscribed: true,
      registered: true,
    });
  });

  /**
   * The subscription is shared, so another identity re-subscribing leaves this one
   * registered on an endpoint that reaches nothing.
   */
  it("is not registered when the canister names another endpoint", async () => {
    reporting("https://relay.example/someone-else");
    await expect(deviceState()).resolves.toMatchObject({
      subscribed: true,
      registered: false,
    });
  });

  it("is not registered when the canister holds nothing for this identity", async () => {
    reporting();
    await expect(deviceState()).resolves.toMatchObject({
      subscribed: true,
      registered: false,
    });
  });

  it("is not registered for a browser no sign-in has registered", async () => {
    vi.mocked(browserKeyActor).mockRejectedValue(
      new UnregisteredBrowserError(),
    );
    await expect(deviceState()).resolves.toMatchObject({
      subscribed: true,
      registered: false,
    });
  });

  /** The endpoint the browser holds is only ours while we still hold its key:
   *  anything else is another identity's re-subscribe. */
  it("is not subscribed when the key we kept names another endpoint", async () => {
    vi.mocked(loadVapidKey).mockResolvedValue({
      endpoint: "https://relay.example/stale",
      privateKey: {} as CryptoKey,
      publicKeyRaw: new Uint8Array(),
    });
    await expect(readBrowserPushState()).resolves.toMatchObject({
      endpoint: undefined,
    });
    await expect(deviceState()).resolves.toMatchObject({
      subscribed: false,
      registered: false,
    });
    expect(browserKeyActor).not.toHaveBeenCalled();
  });

  /** Nothing is on screen yet when this runs, so a failure has to be a value the
   *  resolver can turn into a screen rather than a rejection with nowhere to go. */
  it("answers nothing where the browser cannot be read", async () => {
    vi.mocked(currentDeviceSubscription).mockRejectedValue(
      new Error("no service worker here"),
    );
    await expect(readBrowserPushState()).resolves.toBeUndefined();
  });

  it("reads no further for a browser without push support", async () => {
    vi.mocked(isPushSupported).mockReturnValue(false);
    await expect(readBrowserPushState()).resolves.toEqual({
      supported: false,
      permission: "granted",
    });
    expect(currentDeviceSubscription).not.toHaveBeenCalled();
  });
});

describe("resolveOptIn", () => {
  const ENDPOINT = "https://relay.example/held";

  const actorGranting = (granted: boolean) =>
    ({
      notification_consent_granted: vi.fn(() => Promise.resolve(granted)),
    }) as unknown as ActorSubclass<_SERVICE>;

  beforeEach(() => {
    vi.clearAllMocks();
    vi.stubGlobal("Notification", { permission: "granted" });
    vi.mocked(isPushSupported).mockReturnValue(true);
    vi.mocked(currentDeviceSubscription).mockResolvedValue({
      endpoint: ENDPOINT,
    } as PushSubscription);
    vi.mocked(loadVapidKey).mockResolvedValue({
      endpoint: ENDPOINT,
      privateKey: {} as CryptoKey,
      publicKeyRaw: new Uint8Array(),
    });
    vi.mocked(browserKeyActor).mockResolvedValue({
      get_webpush_subscription_status: vi.fn(() =>
        Promise.resolve([
          { endpoint: ENDPOINT, pool_len: 30, issued_at_ns: BigInt(0) },
        ]),
      ),
    } as unknown as ActorSubclass<_SERVICE>);
  });

  const ready = {
    supported: true,
    permission: "granted" as NotificationPermission,
    subscribed: true,
    registered: true,
  };

  /** The skip answer is what the app is told, so it carries both halves this already
   *  read instead of costing a second pair of reads for the same facts. */
  it("answers from what it read where there is nothing to ask", async () => {
    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(true),
        browser: readBrowserPushState(),
      }),
    ).resolves.toEqual({ screen: "skip", granted: true });
  });

  it("asks a set-up browser whose app is not allowed, carrying what it found", async () => {
    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(false),
        browser: readBrowserPushState(),
      }),
    ).resolves.toEqual({
      screen: "enable",
      state: ready,
      consented: false,
      installFirst: false,
    });
  });

  /** A browser that could not be read is not a browser with nothing to ask. The
   *  question is offered, and answering it reads everything again. */
  it("asks anyway where the browser could not be read", async () => {
    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(false),
        browser: Promise.resolve(undefined),
      }),
    ).resolves.toMatchObject({
      screen: "enable",
      state: { registered: false },
    });
  });

  /** An error is an error whenever it arrives: the caller shows the same toast for
   *  one raised here as for one raised while the user answers. */
  it("lets a failed registration read through to the caller", async () => {
    vi.mocked(browserKeyActor).mockRejectedValue(new Error("query refused"));
    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(false),
        browser: readBrowserPushState(),
      }),
    ).rejects.toThrow("query refused");
  });

  /** The app's consent and this browser's state need nothing from each other, so
   *  whichever is slow must not hold up the other. A resolver that read the browser
   *  first would never reach the query this one resolves on. */
  it("reads the app's consent while the browser is still being read", async () => {
    let release: (state: BrowserPushState | undefined) => void = () =>
      undefined;
    const browser = new Promise<BrowserPushState | undefined>((resolve) => {
      release = resolve;
    });
    const actor = {
      notification_consent_granted: vi.fn(() => {
        release({ supported: true, permission: "granted", endpoint: ENDPOINT });
        return Promise.resolve(false);
      }),
    } as unknown as ActorSubclass<_SERVICE>;

    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        browser,
      }),
    ).resolves.toMatchObject({ screen: "enable" });
  });

  /** A consent query that fails reads as "not allowed yet", which asks a question
   *  the user can answer rather than dropping the request. */
  it("treats an unreadable consent as no consent", async () => {
    const actor = {
      notification_consent_granted: vi.fn(() =>
        Promise.reject(new Error("query refused")),
      ),
    } as unknown as ActorSubclass<_SERVICE>;
    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        browser: readBrowserPushState(),
      }),
    ).resolves.toMatchObject({ screen: "enable", consented: false });
  });
});

describe("readGranted", () => {
  const ENDPOINT = "https://relay.example/held";

  const actorGranting = (granted: boolean) =>
    ({
      notification_consent_granted: vi.fn(() => Promise.resolve(granted)),
    }) as unknown as ActorSubclass<_SERVICE>;

  const registeredOn = (endpoint?: string) =>
    vi.mocked(browserKeyActor).mockResolvedValue({
      get_webpush_subscription_status: vi.fn(() =>
        Promise.resolve(
          endpoint === undefined
            ? []
            : [{ endpoint, pool_len: 30, issued_at_ns: BigInt(0) }],
        ),
      ),
    } as unknown as ActorSubclass<_SERVICE>);

  beforeEach(() => {
    vi.clearAllMocks();
    vi.stubGlobal("Notification", { permission: "granted" });
    vi.mocked(isPushSupported).mockReturnValue(true);
    vi.mocked(currentDeviceSubscription).mockResolvedValue({
      endpoint: ENDPOINT,
    } as PushSubscription);
    vi.mocked(loadVapidKey).mockResolvedValue({
      endpoint: ENDPOINT,
      privateKey: {} as CryptoKey,
      publicKeyRaw: new Uint8Array(),
    });
    registeredOn(ENDPOINT);
  });

  it("is granted where the app is allowed and this browser delivers", async () => {
    await expect(
      readGranted({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(true),
      }),
    ).resolves.toBe(true);
  });

  it("is not granted where the app is not allowed", async () => {
    await expect(
      readGranted({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(false),
      }),
    ).resolves.toBe(false);
  });

  /** Consent is per identity and delivery is per browser, so an app allowed on
   *  another device is not an app that may notify the user here. */
  it("is not granted where this browser holds no registration", async () => {
    registeredOn();
    await expect(
      readGranted({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(true),
      }),
    ).resolves.toBe(false);
  });

  it("is not granted on a browser without push support", async () => {
    vi.mocked(isPushSupported).mockReturnValue(false);
    await expect(
      readGranted({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(true),
      }),
    ).resolves.toBe(false);
  });

  it("is not granted where the permission was reset to default", async () => {
    vi.stubGlobal("Notification", { permission: "default" });
    await expect(
      readGranted({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(true),
      }),
    ).resolves.toBe(false);
  });

  it("is not granted where the browser could not be read", async () => {
    vi.mocked(currentDeviceSubscription).mockRejectedValue(new Error("gone"));
    await expect(
      readGranted({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(true),
      }),
    ).resolves.toBe(false);
  });
});

describe("watchNotificationPermission", () => {
  let permission: NotificationPermission;

  beforeEach(() => {
    permission = "denied";
    vi.stubGlobal(
      "Notification",
      Object.defineProperty(function () {} as never, "permission", {
        get: () => permission,
      }),
    );
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.useRealTimers();
    vi.unstubAllGlobals();
  });

  it("waits while the permission is still refused", async () => {
    const onAllowed = vi.fn();
    const stop = watchNotificationPermission(onAllowed);

    await vi.advanceTimersByTimeAsync(5_000);

    expect(onAllowed).not.toHaveBeenCalled();
    stop();
  });

  it("carries on once the permission is lifted", async () => {
    const onAllowed = vi.fn();
    watchNotificationPermission(onAllowed);

    permission = "default";
    await vi.advanceTimersByTimeAsync(1_000);

    expect(onAllowed).toHaveBeenCalledTimes(1);
  });

  /** A browser that has to be asked again is one the enable step can ask, so the
   *  watcher hands back on anything that is no longer a refusal. */
  it("carries on for a permission that was granted outright", async () => {
    const onAllowed = vi.fn();
    watchNotificationPermission(onAllowed);

    permission = "granted";
    await vi.advanceTimersByTimeAsync(1_000);

    expect(onAllowed).toHaveBeenCalledTimes(1);
  });

  it("calls back once and stops watching", async () => {
    const onAllowed = vi.fn();
    watchNotificationPermission(onAllowed);

    permission = "granted";
    await vi.advanceTimersByTimeAsync(10_000);

    expect(onAllowed).toHaveBeenCalledTimes(1);
  });

  it("stops watching when told to", async () => {
    const onAllowed = vi.fn();
    const stop = watchNotificationPermission(onAllowed);
    stop();

    permission = "granted";
    await vi.advanceTimersByTimeAsync(5_000);

    expect(onAllowed).not.toHaveBeenCalled();
  });

  it("reports a permission change as soon as the browser does", async () => {
    const listeners: (() => void)[] = [];
    const status = {
      addEventListener: (_: string, fn: () => void) => listeners.push(fn),
      removeEventListener: vi.fn(),
    };
    vi.stubGlobal("navigator", {
      ...navigator,
      permissions: { query: () => Promise.resolve(status) },
    });
    const onAllowed = vi.fn();
    watchNotificationPermission(onAllowed);
    await vi.advanceTimersByTimeAsync(0);

    permission = "granted";
    listeners.forEach((fn) => fn());

    expect(onAllowed).toHaveBeenCalledTimes(1);
    expect(status.removeEventListener).toHaveBeenCalled();
  });

  /** Firefox rejects a query for a permission name it does not know, which must not
   *  take the timer down with it. */
  it("keeps watching where the browser cannot report changes", async () => {
    vi.stubGlobal("navigator", {
      ...navigator,
      permissions: { query: () => Promise.reject(new TypeError()) },
    });
    const onAllowed = vi.fn();
    watchNotificationPermission(onAllowed);
    await vi.advanceTimersByTimeAsync(0);

    permission = "granted";
    await vi.advanceTimersByTimeAsync(1_000);

    expect(onAllowed).toHaveBeenCalledTimes(1);
  });
});

describe("notificationsNeedInstallHere", () => {
  beforeEach(() => {
    vi.mocked(browserAndSystem).mockReturnValue({
      os: { Ios: null },
      brand: { Safari: null },
    });
    vi.stubGlobal("matchMedia", () => ({ matches: false }));
  });

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  it("is true in a browser tab on iOS, where this browser cannot subscribe", () => {
    expect(notificationsNeedInstallHere()).toBe(true);
  });

  /** The installed app is served this same page, and inside it notifications are
   *  ordinary: it is the thing the install was for. */
  it("is false in the installed app, which iOS reports the old way", () => {
    vi.stubGlobal("navigator", { ...navigator, standalone: true });

    expect(notificationsNeedInstallHere()).toBe(false);
  });

  it("is false in the installed app, which newer iOS reports as a display mode", () => {
    vi.stubGlobal("matchMedia", () => ({ matches: true }));

    expect(notificationsNeedInstallHere()).toBe(false);
  });

  it("is false where notifications need no app at all", () => {
    vi.mocked(browserAndSystem).mockReturnValue({
      os: { Macos: null },
      brand: { Safari: null },
    });

    expect(notificationsNeedInstallHere()).toBe(false);
  });

  /** Not every context this module loads in is a document: the service worker imports
   *  from here and has no `window`. */
  it("answers without a matchMedia to ask", () => {
    vi.stubGlobal("matchMedia", undefined);

    expect(notificationsNeedInstallHere()).toBe(true);
  });
});

describe("resolveOptIn where notifications need an app", () => {
  const actorGranting = (granted: boolean) =>
    ({
      notification_consent_granted: vi.fn(() => Promise.resolve(granted)),
    }) as unknown as Parameters<typeof resolveOptIn>[0]["actor"];

  beforeEach(() => {
    vi.clearAllMocks();
    vi.mocked(browserAndSystem).mockReturnValue({
      os: { Ios: null },
      brand: { Safari: null },
    });
    vi.stubGlobal("matchMedia", () => ({ matches: false }));
    vi.stubGlobal("Notification", { permission: "default" });
  });

  afterEach(() => {
    vi.unstubAllGlobals();
  });

  /** This browser cannot read whether the app is linked and subscribed, so what is
   *  asked turns on consent alone. */
  it("asks an identity that has not allowed this app", async () => {
    await expect(
      resolveOptIn({
        identityNumber: BigInt(10_000),
        origin: "https://app.example",
        actor: actorGranting(false),
        browser: Promise.resolve(undefined),
      }),
    ).resolves.toMatchObject({ screen: "enable", installFirst: true });
  });

  it("does not ask an identity that has allowed it before", async () => {
    await expect(
      resolveOptIn({
        identityNumber: BigInt(10_000),
        origin: "https://app.example",
        actor: actorGranting(true),
        browser: Promise.resolve(undefined),
      }),
    ).resolves.toEqual({ screen: "skip", granted: true });
  });

  /** Answering sends the user through the install rather than raising a prompt, and a
   *  browser that cannot subscribe must not turn that into nothing to ask. */
  it("asks even where this browser reports it cannot subscribe", async () => {
    // Read, not defaulted: a resolution handed no browser state assumes support, so
    // passing `undefined` here would never reach the unsupported case at all.
    await expect(
      resolveOptIn({
        identityNumber: BigInt(10_000),
        origin: "https://app.example",
        actor: actorGranting(false),
        browser: Promise.resolve({
          supported: false,
          permission: "default" as NotificationPermission,
        }),
      }),
    ).resolves.toMatchObject({ screen: "enable", installFirst: true });
  });
});
