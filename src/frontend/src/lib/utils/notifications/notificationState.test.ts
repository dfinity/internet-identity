import { beforeEach, describe, expect, it, vi } from "vitest";
// vapidKeyStore opens an IndexedDB store at import; the resolver under test
// never touches it. The decline cooldown is mocked so the logic stays pure.
vi.mock("./vapidKeyStore", () => ({ loadVapidKey: vi.fn() }));
vi.mock("./notificationDiagnostics", () => ({
  wasDeclinedRecently: vi.fn(() => false),
}));
vi.mock("$lib/stores/browser-key.store", () => ({
  currentBrowserId: vi.fn(),
}));
vi.mock("./pushSubscription", () => ({
  isPushSupported: vi.fn(() => true),
  currentDeviceSubscription: vi.fn(),
}));

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import {
  readDeviceState,
  resolveOptInScreen,
  type DeviceNotificationState,
} from "./notificationState";
import { wasDeclinedRecently } from "./notificationDiagnostics";
import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import { loadVapidKey } from "./vapidKeyStore";
import { currentBrowserId } from "$lib/stores/browser-key.store";

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

  it("shows the full pitch to a first-timer", () => {
    expect(resolveOptInScreen(state({}), ORIGIN, false)).toBe("first-time");
  });
});

describe("readDeviceState", () => {
  const ENDPOINT = "https://relay.example/held";

  /** A canister that reports a registration on `endpoint`, or none at all. */
  const reporting = (endpoint?: string) =>
    ({
      get_webpush_subscription_status: vi.fn(() =>
        Promise.resolve(
          endpoint === undefined
            ? []
            : [{ endpoint, pool_len: 30, issued_at_ns: BigInt(0) }],
        ),
      ),
    }) as unknown as ActorSubclass<_SERVICE>;

  beforeEach(() => {
    vi.stubGlobal("Notification", { permission: "granted" });
    vi.mocked(isPushSupported).mockReturnValue(true);
    vi.mocked(currentBrowserId).mockResolvedValue(7);
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
    await expect(
      readDeviceState(IDENTITY, reporting(ENDPOINT)),
    ).resolves.toMatchObject({ subscribed: true, registered: true });
  });

  /**
   * The subscription is shared, so another identity re-subscribing leaves this one
   * registered on an endpoint that reaches nothing.
   */
  it("is not registered when the canister names another endpoint", async () => {
    await expect(
      readDeviceState(
        IDENTITY,
        reporting("https://relay.example/someone-else"),
      ),
    ).resolves.toMatchObject({ subscribed: true, registered: false });
  });

  it("is not registered when the canister holds nothing for this identity", async () => {
    await expect(readDeviceState(IDENTITY, reporting())).resolves.toMatchObject(
      { subscribed: true, registered: false },
    );
  });
});
