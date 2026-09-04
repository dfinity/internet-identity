import { beforeEach, describe, expect, it, vi } from "vitest";
// vapidKeyStore opens an IndexedDB store at import; the resolver under test
// never touches it. The decline cooldown is mocked so the logic stays pure.
vi.mock("./vapidKeyStore", () => ({ loadVapidKey: vi.fn() }));
vi.mock("./notificationDiagnostics", () => ({
  wasDeclinedRecently: vi.fn(() => false),
}));

import {
  resolveOptInScreen,
  type DeviceNotificationState,
} from "./notificationState";
import { wasDeclinedRecently } from "./notificationDiagnostics";

const declined = vi.mocked(wasDeclinedRecently);
const ORIGIN = "https://app.example";

const state = (
  over: Partial<DeviceNotificationState>,
): DeviceNotificationState => ({
  supported: true,
  permission: "default",
  subscribed: false,
  ...over,
});

describe("resolveOptInScreen", () => {
  beforeEach(() => declined.mockReturnValue(false));

  it("skips when notifications aren't supported", () => {
    expect(resolveOptInScreen(state({ supported: false }), ORIGIN, [])).toBe(
      "skip",
    );
  });

  it("skips when already fully on for this app", () => {
    expect(
      resolveOptInScreen(
        state({ permission: "granted", subscribed: true }),
        ORIGIN,
        [ORIGIN],
      ),
    ).toBe("skip");
  });

  it("skips an app declined recently", () => {
    declined.mockReturnValue(true);
    expect(resolveOptInScreen(state({}), ORIGIN, [])).toBe("skip");
  });

  it("shows guidance when blocked", () => {
    expect(
      resolveOptInScreen(state({ permission: "denied" }), ORIGIN, []),
    ).toBe("blocked");
  });

  it("asks only for this app's consent when the browser is subscribed", () => {
    expect(
      resolveOptInScreen(
        state({ permission: "granted", subscribed: true }),
        ORIGIN,
        [],
      ),
    ).toBe("allow-app");
  });

  it("offers to enable this device when the app is allowed elsewhere", () => {
    expect(
      resolveOptInScreen(state({ subscribed: false }), ORIGIN, [ORIGIN]),
    ).toBe("new-device");
  });

  it("shows the full pitch to a first-timer", () => {
    expect(resolveOptInScreen(state({}), ORIGIN, [])).toBe("first-time");
  });
});
