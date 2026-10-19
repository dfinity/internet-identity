import { beforeEach, describe, expect, it, vi } from "vitest";
// Mocked before the unit is imported: the device registration reaches for a browser,
// and what is under test is which steps run and in what order.
vi.mock("./subscribeDevice", () => ({
  ensureRegisteredDevice: vi.fn(() => Promise.resolve()),
}));
vi.mock("./pushSubscription", () => ({
  requestNotificationPermission: vi.fn(() => Promise.resolve("granted")),
}));
vi.mock("./notificationState", () => ({
  notificationsNeedInstallHere: vi.fn(() => false),
}));
vi.mock("./deviceApp", () => ({ claimDeviceApp: vi.fn() }));
vi.mock("$lib/utils/notifications/appStatus", () => ({
  browserAppStatus: vi.fn(),
}));
vi.mock("./browserActor", () => ({ browserKeyActor: vi.fn() }));
vi.mock("./turnedOff", () => ({ clearNotificationsOff: vi.fn() }));
vi.mock("./turnOnAndProve", () => ({ turnOnAndProve: vi.fn() }));
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  NotificationAppInfo,
  _SERVICE,
} from "$lib/generated/internet_identity_types";
import { disallowApp, turnOnNotifications } from "./enableNotifications";
import type { AppDeviceState, BrowserDeviceState } from "./notificationState";
import { ensureRegisteredDevice } from "./subscribeDevice";
import { requestNotificationPermission } from "./pushSubscription";
import {
  browserAppStatus,
  type BrowserAppStatus,
} from "$lib/utils/notifications/appStatus";
import { claimDeviceApp } from "./deviceApp";
import { turnOnAndProve } from "./turnOnAndProve";
import { browserKeyActor } from "./browserActor";
import { clearNotificationsOff } from "./turnedOff";

const register = vi.mocked(ensureRegisteredDevice);
const permission = vi.mocked(requestNotificationPermission);
const appStatus = vi.mocked(browserAppStatus);
const claim = vi.mocked(claimDeviceApp);
const browserActor = vi.mocked(browserKeyActor);
const prove = vi.mocked(turnOnAndProve);
const ORIGIN = "https://app.example";
const IDENTITY = BigInt(10_000);

/** An actor whose grant answers with `replies` in order, one per call. */
const actorAnswering = (...replies: { Ok: null }[]) => {
  const notification_grant_consent = vi.fn(() =>
    Promise.resolve(replies[notification_grant_consent.mock.calls.length - 1]),
  );
  return {
    actor: { notification_grant_consent } as unknown as ActorSubclass<_SERVICE>,
    grant: notification_grant_consent,
  };
};

const ok = { Ok: null } as const;

const state = (over: Partial<BrowserDeviceState> = {}): BrowserDeviceState => ({
  source: "browser",
  supported: true,
  permission: "default",
  subscribed: false,
  registered: false,
  ...over,
});

const appState = (over: Partial<AppDeviceState> = {}): AppDeviceState => ({
  source: "app",
  subscribed: true,
  enabled: true,
  ...over,
});

describe("withdrawing consent", () => {
  it("revokes for the app in one call", async () => {
    const revoke = vi.fn(() => Promise.resolve(ok));
    const actor = {
      notification_revoke_consent: revoke,
    } as unknown as ActorSubclass<_SERVICE>;

    await disallowApp({ identityNumber: IDENTITY, origin: ORIGIN, actor });

    expect(revoke).toHaveBeenCalledTimes(1);
    expect(revoke).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
      origin: ORIGIN,
    });
  });

  it("reports a refusal rather than swallowing it", async () => {
    const actor = {
      notification_revoke_consent: vi.fn(() =>
        Promise.resolve({ Err: { InternalCanisterError: "not enabled" } }),
      ),
    } as unknown as ActorSubclass<_SERVICE>;

    await expect(
      disallowApp({ identityNumber: IDENTITY, origin: ORIGIN, actor }),
    ).rejects.toThrow();
  });
});

describe("turnOnNotifications", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    permission.mockResolvedValue("granted");
  });

  it("registers the device before recording consent", async () => {
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: state(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "enabled" });

    // A refusal at the browser prompt must leave no consent behind, so the
    // registration has to land first.
    expect(register.mock.invocationCallOrder[0]).toBeLessThan(
      grant.mock.invocationCallOrder[0],
    );
    // Asking for them here takes back whatever the switch in settings said.
    expect(clearNotificationsOff).toHaveBeenCalledWith(IDENTITY);
    expect(grant).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
      origin: ORIGIN,
    });
  });

  /** Subscribing drops the endpoint every other identity here is registered with, so
   *  a browser that already holds a registration must not run that step again. */
  it("skips the registration for a browser already registered", async () => {
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: state({
          permission: "granted",
          subscribed: true,
          registered: true,
        }),
        consented: false,
      }),
    ).resolves.toEqual({ status: "enabled" });

    expect(permission).not.toHaveBeenCalled();
    expect(register).not.toHaveBeenCalled();
    expect(grant).toHaveBeenCalledTimes(1);
  });

  /** The app was allowed on another device, so only this browser needs setting up. */
  it("skips the consent for an app already allowed", async () => {
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: state(),
        consented: true,
      }),
    ).resolves.toEqual({ status: "enabled" });

    expect(register).toHaveBeenCalledTimes(1);
    expect(grant).not.toHaveBeenCalled();
  });

  it("asks for permission only where it is not already granted", async () => {
    const { actor } = actorAnswering(ok);

    await turnOnNotifications({
      identityNumber: IDENTITY,
      origin: ORIGIN,
      actor,
      device: state({ permission: "granted" }),
      consented: false,
    });

    expect(permission).not.toHaveBeenCalled();
    expect(register).toHaveBeenCalledTimes(1);
  });

  it("records nothing when the prompt is denied", async () => {
    permission.mockResolvedValue("denied");
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: state(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "denied" });

    expect(register).not.toHaveBeenCalled();
    expect(grant).not.toHaveBeenCalled();
  });

  it("reports a dismissed prompt apart from a denial", async () => {
    permission.mockResolvedValue("default");
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: state(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "dismissed" });

    expect(register).not.toHaveBeenCalled();
    expect(grant).not.toHaveBeenCalled();
  });

  it("reports a refused grant rather than swallowing it", async () => {
    const { actor } = actorAnswering({
      Err: { Disabled: null },
    } as unknown as { Ok: null });

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: state({
          permission: "granted",
          subscribed: true,
          registered: true,
        }),
        consented: false,
      }),
    ).rejects.toThrow();
  });
});

/** An app this device has, as the canister reports it to this browser. */
const appInfo = (
  enabled: boolean,
  over: { subscribed?: boolean } = {},
): BrowserAppStatus => ({
  kind: "app",
  app: {
    subscribed: true,
    enabled,
    ...over,
  } as unknown as NotificationAppInfo,
});

describe("turnOnNotifications where a Home Screen app delivers", () => {
  const setEnabled = vi.fn(() => Promise.resolve(ok));

  beforeEach(() => {
    vi.clearAllMocks();
    // Reset, not clear: a queued one-shot answer outlives a clear and would answer the next case here.
    appStatus.mockReset();
    setEnabled.mockResolvedValue(ok);
    browserActor.mockResolvedValue({
      set_notification_app_enabled: setEnabled,
    } as unknown as ActorSubclass<_SERVICE>);
  });

  /// `subscribed` outlives the app it names, so the switch going on is also the app
  /// being asked to show it is still there, in the one call that does both.
  it("turns this identity's flag back on by proving the app answers", async () => {
    appStatus.mockResolvedValue(appInfo(false));
    prove.mockResolvedValue(true);
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: appState(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "enabled" });

    expect(prove).toHaveBeenCalledWith(IDENTITY);
    expect(grant).toHaveBeenCalledTimes(1);
    expect(permission).not.toHaveBeenCalled();
    expect(register).not.toHaveBeenCalled();
  });

  it("asks for the install where the app no longer answers", async () => {
    appStatus.mockResolvedValue(appInfo(false));
    prove.mockResolvedValue(false);
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: appState(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "install-required" });

    expect(grant).not.toHaveBeenCalled();
  });

  it("writes nothing where the app already delivers for this identity", async () => {
    appStatus.mockResolvedValue(appInfo(true));
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: appState(),
        consented: true,
      }),
    ).resolves.toEqual({ status: "enabled" });

    expect(setEnabled).not.toHaveBeenCalled();
    expect(grant).not.toHaveBeenCalled();
  });

  /// The press is the answer. An install walked from here finishes by carrying whatever
  /// flag the entry holds, so a flag left saying off would outlive the answer and the
  /// user would walk the whole install to arrive with notifications still off.
  it("records the answer before sending them to install", async () => {
    appStatus.mockResolvedValue(appInfo(false, { subscribed: false }));
    const { actor } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: appState(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "install-required" });

    expect(setEnabled).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
      enabled: true,
    });
  });

  it("asks for the install where no app can be claimed", async () => {
    appStatus.mockResolvedValue({ kind: "no-app" });
    claim.mockResolvedValue("no-donor");
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: appState(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "install-required" });

    expect(grant).not.toHaveBeenCalled();
    expect(permission).not.toHaveBeenCalled();
    expect(register).not.toHaveBeenCalled();
  });

  it("reads the canister again after a claim that answered nothing", async () => {
    appStatus
      .mockResolvedValueOnce({ kind: "no-app" })
      .mockResolvedValueOnce(appInfo(true));
    claim.mockResolvedValue("no-donor");
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: appState(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "enabled" });

    expect(appStatus).toHaveBeenCalledTimes(2);
    expect(grant).toHaveBeenCalledTimes(1);
  });

  it("reports a re-read it could not make as neither yes nor no", async () => {
    appStatus
      .mockResolvedValueOnce({ kind: "no-app" })
      .mockResolvedValueOnce({ kind: "unanswered", reason: "offline" });
    claim.mockResolvedValue("no-donor");
    const { actor } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: appState(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "unanswered", reason: "offline" });
  });

  it("reports an app it could not read as neither yes nor no", async () => {
    appStatus.mockResolvedValue({ kind: "unanswered", reason: "offline" });
    const { actor, grant } = actorAnswering(ok);

    await expect(
      turnOnNotifications({
        identityNumber: IDENTITY,
        origin: ORIGIN,
        actor,
        device: appState(),
        consented: false,
      }),
    ).resolves.toEqual({ status: "unanswered", reason: "offline" });

    expect(claim).not.toHaveBeenCalled();
    expect(setEnabled).not.toHaveBeenCalled();
    expect(grant).not.toHaveBeenCalled();
  });
});
