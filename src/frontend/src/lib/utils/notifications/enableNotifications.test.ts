import { beforeEach, describe, expect, it, vi } from "vitest";
// Mocked before the unit is imported: the device registration reaches for a browser,
// and what is under test is which steps run and in what order.
vi.mock("./subscribeDevice", () => ({
  ensureRegisteredDevice: vi.fn(() => Promise.resolve()),
}));
vi.mock("./pushSubscription", () => ({
  requestNotificationPermission: vi.fn(() => Promise.resolve("granted")),
}));
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { disallowApp, turnOnNotifications } from "./enableNotifications";
import type { DeviceNotificationState } from "./notificationState";
import { ensureRegisteredDevice } from "./subscribeDevice";
import { requestNotificationPermission } from "./pushSubscription";

const register = vi.mocked(ensureRegisteredDevice);
const permission = vi.mocked(requestNotificationPermission);
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

const state = (
  over: Partial<DeviceNotificationState> = {},
): DeviceNotificationState => ({
  supported: true,
  permission: "default",
  subscribed: false,
  registered: false,
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
