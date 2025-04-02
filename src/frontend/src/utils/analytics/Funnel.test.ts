import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { Funnel } from "./Funnel";
import { analytics } from "./analytics";
import { cleanUpWindowSessionTrackers } from "../trackWindowSession";

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
    expect(analytics.event).toHaveBeenCalledWith("start-login", undefined);
  });

  it("init() - triggers start-login event with properties", () => {
    const properties = { userId: "123", source: "test" };
    funnel.init(properties);
    expect(analytics.event).toHaveBeenCalledWith("start-login", properties);
  });

  it("init() - tracks window session enter when window becomes visible", () => {
    funnel.init();
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(analytics.event).toHaveBeenCalledWith(
      "start-login-window-session-enter",
      undefined,
    );
  });

  it("init() - tracks window session enter with properties when window becomes visible", () => {
    const properties = { userId: "123", source: "test" };
    funnel.init(properties);
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(analytics.event).toHaveBeenCalledWith(
      "start-login-window-session-enter",
      properties,
    );
  });

  it("init() - tracks window session leave when window is hidden", () => {
    funnel.init();
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(analytics.event).toHaveBeenCalledWith(
      "start-login-window-session-leave",
      undefined,
    );
  });

  it("init() - tracks window session leave with properties when window is hidden", () => {
    const properties = { userId: "123", source: "test" };
    funnel.init(properties);
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    expect(analytics.event).toHaveBeenCalledWith(
      "start-login-window-session-leave",
      properties,
    );
  });

  it("trigger() - tracks new registration start event", () => {
    funnel.trigger(LoginEvents.NewRegistrationStart);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-new-registration-start",
      undefined,
    );
  });

  it("trigger() - tracks new registration start event with init properties", () => {
    const properties = { userId: "123", source: "test" };
    funnel.init(properties);
    funnel.trigger(LoginEvents.NewRegistrationStart);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-new-registration-start",
      properties,
    );
  });

  it("trigger() - tracks new registration start event with additional properties", () => {
    const additionalProps = { step: 1, method: "email" };
    funnel.trigger(LoginEvents.NewRegistrationStart, additionalProps);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-new-registration-start",
      additionalProps,
    );
  });

  it("trigger() - merges init properties with additional properties", () => {
    const initProps = { userId: "123", source: "test" };
    const additionalProps = { step: 1, method: "email" };
    const expectedProps = { ...initProps, ...additionalProps };

    funnel.init(initProps);
    funnel.trigger(LoginEvents.NewRegistrationStart, additionalProps);

    expect(analytics.event).toHaveBeenCalledWith(
      "login-new-registration-start",
      expectedProps,
    );
  });

  it("trigger() - tracks complete passkey login flow", () => {
    funnel.trigger(LoginEvents.ExistingUserStart);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-existing-user-start",
      undefined,
    );

    funnel.trigger(LoginEvents.ExistingUserPasskey);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-existing-user-passkey",
      undefined,
    );

    funnel.trigger(LoginEvents.ExistingUserPasskeySuccess);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-existing-user-passkey-success",
      undefined,
    );
  });

  it("trigger() - tracks complete OpenID login flow", () => {
    funnel.trigger(LoginEvents.ExistingUserStart);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-existing-user-start",
      undefined,
    );

    funnel.trigger(LoginEvents.ExistingUserOpenId);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-existing-user-openid",
      undefined,
    );

    funnel.trigger(LoginEvents.ExistingUserOpenIdSuccess);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-existing-user-openid-success",
      undefined,
    );
  });

  it("trigger() - tracks recovery start event", () => {
    funnel.trigger(LoginEvents.RecoveryStart);
    expect(analytics.event).toHaveBeenCalledWith(
      "login-recovery-start",
      undefined,
    );
  });

  it("close() - stops tracking window session events", () => {
    funnel.init();
    funnel.close();

    expect(analytics.event).toHaveBeenCalledTimes(2); // the start-login and end-login events

    // After closing, window events should not trigger analytics
    mockVisibilityState("hidden");
    document.dispatchEvent(new Event("visibilitychange"));
    mockVisibilityState("visible");
    document.dispatchEvent(new Event("visibilitychange"));

    expect(analytics.event).toHaveBeenCalledTimes(2); // same as before
  });

  it("close() - tracks duration since init", () => {
    vi.useFakeTimers();

    funnel.init();
    expect(analytics.event).toHaveBeenCalledWith("start-login", undefined);
    expect(analytics.event).toHaveBeenCalledTimes(1);

    // Advance time by 5 seconds
    vi.advanceTimersByTime(5000);

    funnel.close();
    expect(analytics.event).toHaveBeenCalledTimes(2);
    expect(analytics.event).toHaveBeenCalledWith("end-login", {
      "duration-login": 5,
    });

    vi.useRealTimers();
  });

  it("close() - tracks duration since init and includes init properties", () => {
    vi.useFakeTimers();

    const properties = { userId: "123", source: "test" };
    funnel.init(properties);
    expect(analytics.event).toHaveBeenCalledWith("start-login", properties);
    expect(analytics.event).toHaveBeenCalledTimes(1);

    // Advance time by 5 seconds
    vi.advanceTimersByTime(5000);

    funnel.close();
    expect(analytics.event).toHaveBeenCalledTimes(2);
    expect(analytics.event).toHaveBeenCalledWith("end-login", {
      ...properties,
      "duration-login": 5,
    });

    vi.useRealTimers();
  });

  it("close() - does nothing if init was not called", () => {
    funnel.close();
    expect(analytics.event).not.toHaveBeenCalled();
  });
});
