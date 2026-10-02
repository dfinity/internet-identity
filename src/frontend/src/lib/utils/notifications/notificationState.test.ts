import { beforeEach, describe, expect, it, vi } from "vitest";
// vapidKeyStore opens an IndexedDB store at import; the resolver under test
// never touches it. The decline cooldown is mocked so the logic stays pure.
vi.mock("./vapidKeyStore", () => ({ loadVapidKey: vi.fn() }));
vi.mock("./notificationDiagnostics", () => ({
  wasDeclinedRecently: vi.fn(() => false),
  recordPermission: vi.fn(),
  recordFailure: vi.fn(),
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
  readBrowserPushState,
  readDeviceState,
  resolveOptIn,
  resolveOptInScreen,
  type BrowserPushState,
  type DeviceNotificationState,
} from "./notificationState";
import { wasDeclinedRecently } from "./notificationDiagnostics";
import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import { loadVapidKey } from "./vapidKeyStore";
import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";

const declined = vi.mocked(wasDeclinedRecently);
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
  beforeEach(() => declined.mockReturnValue(false));

  it("skips when notifications aren't supported", () => {
    expect(resolveOptInScreen(state({ supported: false }), ORIGIN, false)).toBe(
      "skip",
    );
  });

  it("skips when already fully on for this app", () => {
    expect(
      resolveOptInScreen(
        state({ permission: "granted", subscribed: true, registered: true }),
        ORIGIN,
        true,
      ),
    ).toBe("skip");
  });

  it("skips an app declined recently", () => {
    declined.mockReturnValue(true);
    expect(resolveOptInScreen(state({}), ORIGIN, false)).toBe("skip");
  });

  it("shows guidance when blocked", () => {
    expect(
      resolveOptInScreen(state({ permission: "denied" }), ORIGIN, false),
    ).toBe("blocked");
  });

  it("asks only for this app's consent when the browser is subscribed", () => {
    expect(
      resolveOptInScreen(
        state({ permission: "granted", subscribed: true, registered: true }),
        ORIGIN,
        false,
      ),
    ).toBe("allow-app");
  });

  it("offers to enable this device when the app is allowed elsewhere", () => {
    expect(resolveOptInScreen(state({ registered: false }), ORIGIN, true)).toBe(
      "new-device",
    );
  });

  it("offers to enable this device for an identity the canister has no row for", () => {
    expect(
      resolveOptInScreen(
        state({ permission: "granted", subscribed: true, registered: false }),
        ORIGIN,
        true,
      ),
    ).toBe("new-device");
  });

  /// `allow-app` never asks the browser for permission, so a registration whose
  /// permission was reset would report success while nothing can be delivered.
  it("asks for permission again when a registered browser had it reset", () => {
    expect(
      resolveOptInScreen(
        state({ permission: "default", subscribed: true, registered: true }),
        ORIGIN,
        false,
      ),
    ).toBe("first-time");
  });

  it("shows the full pitch to a first-timer", () => {
    expect(resolveOptInScreen(state({}), ORIGIN, false)).toBe("first-time");
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
    declined.mockReturnValue(false);
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

  /** The skip answer is what the app is told, so it carries the consent this
   *  already read instead of costing a second query for the same fact. */
  it("answers from what it read where there is nothing to ask", async () => {
    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(true),
        browser: readBrowserPushState(),
      }),
    ).resolves.toEqual({ screen: "skip", consented: true });
  });

  it("asks this app for consent on a browser that is already set up", async () => {
    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(false),
        browser: readBrowserPushState(),
      }),
    ).resolves.toEqual({ screen: "allow-app" });
  });

  /** A browser that could not be read is not a browser with nothing to ask: the
   *  user gets the screen they can retry from. */
  it("offers the failed screen where the browser could not be read", async () => {
    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(false),
        browser: Promise.resolve(undefined),
      }),
    ).resolves.toEqual({ screen: "failed" });
  });

  it("offers the failed screen where the registration read throws", async () => {
    vi.mocked(browserKeyActor).mockRejectedValue(new Error("query refused"));
    await expect(
      resolveOptIn({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor: actorGranting(false),
        browser: readBrowserPushState(),
      }),
    ).resolves.toEqual({ screen: "failed" });
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
    ).resolves.toEqual({ screen: "allow-app" });
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
    ).resolves.toEqual({ screen: "allow-app" });
  });
});
