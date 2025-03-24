import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { Funnel } from "./Funnel";
import { analytics } from "./analytics";
import { cleanUpWindowSessionTrackers } from "./trackWindowSession";

// Mock analytics
vi.mock("./analytics", () => ({
  analytics: {
    event: vi.fn(),
  },
}));

// Mock document.visibilityState like in trackWindowSession.test.ts
const mockVisibilityState = (state: "hidden" | "visible") => {
  Object.defineProperty(document, "visibilityState", {
    configurable: true,
    get: () => state,
  });
};

const LoginEvents = {
  NewRegistrationStart: "login-new-registration-start",
  ExistingUserStart: "login-existing-user-start",
  ExistingUserPasskey: "login-existing-user-passkey",
  ExistingUserPasskeySuccess: "login-existing-user-passkey-success",
  ExistingUserOpenId: "login-existing-user-openid",
  ExistingUserOpenIdSuccess: "login-existing-user-openid-success",
  RecoveryStart: "login-recovery-start",
} as const;

describe("Funnel", () => {
  let funnel: Funnel<typeof LoginEvents>;

  beforeEach(() => {
    funnel = new Funnel("login");
    vi.clearAllMocks();
  });

  afterEach(() => {
    cleanUpWindowSessionTrackers();
  });

  it("init() - triggers start-login event", () => {
    funnel.init();
    expect(analytics.event).toHaveBeenCalledWith("start-login");
  });

  it("init() - tracks window session enter when window becomes visible", () => {
    funnel.init();
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(analytics.event).toHaveBeenCalledWith(
      "start-login-window-session-enter"
    );
  });

  it("init() - tracks window session leave when window is hidden", () => {
    funnel.init();
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(analytics.event).toHaveBeenCalledWith(
      "start-login-window-session-leave"
    );
  });

  it("trigger() - tracks new registration start event", () => {
    funnel.trigger(LoginEvents.NewRegistrationStart);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-new-registration-start"
    );
  });

  it("trigger() - tracks complete passkey login flow", () => {
    funnel.trigger(LoginEvents.ExistingUserStart);
    expect(analytics.event).toHaveBeenCalledWith("login-existing-user-start");

    funnel.trigger(LoginEvents.ExistingUserPasskey);
    expect(analytics.event).toHaveBeenCalledWith("login-existing-user-passkey");

    funnel.trigger(LoginEvents.ExistingUserPasskeySuccess);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-existing-user-passkey-success"
    );
  });

  it("trigger() - tracks complete OpenID login flow", () => {
    funnel.trigger(LoginEvents.ExistingUserStart);
    expect(analytics.event).toHaveBeenCalledWith("login-existing-user-start");

    funnel.trigger(LoginEvents.ExistingUserOpenId);
    expect(analytics.event).toHaveBeenCalledWith("login-existing-user-openid");

    funnel.trigger(LoginEvents.ExistingUserOpenIdSuccess);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-existing-user-openid-success"
    );
  });

  it("trigger() - tracks recovery start event", () => {
    funnel.trigger(LoginEvents.RecoveryStart);
    expect(analytics.event).toHaveBeenCalledWith("login-recovery-start");
  });

  it("close() - stops tracking window session events", () => {
    funnel.init();
    funnel.close();

    // After closing, window events should not trigger analytics
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));

    expect(analytics.event).toHaveBeenCalledTimes(1); // Only the initial start-login event
  });
});
