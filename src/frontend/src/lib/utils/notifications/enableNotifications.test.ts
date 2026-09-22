import { beforeEach, describe, expect, it, vi } from "vitest";
// Mocked before the unit is imported: the device registration reaches for a browser,
// and what is under test is that it lands before the consent is recorded.
vi.mock("./subscribeDevice", () => ({
  ensureRegisteredDevice: vi.fn(() => Promise.resolve()),
}));
vi.mock("./pushSubscription", () => ({
  requestNotificationPermission: vi.fn(() => Promise.resolve("granted")),
}));
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { allowApp, enableNotifications } from "./enableNotifications";
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

describe("granting consent", () => {
  /** The canister mints the application, so one call records the consent whether or
   *  not the identity has ever signed in at the app. */
  it("records consent in one call", async () => {
    const { actor, grant } = actorAnswering(ok);

    await allowApp({ identityNumber: IDENTITY, origin: ORIGIN, actor });

    expect(grant).toHaveBeenCalledTimes(1);
    expect(grant).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
      origin: ORIGIN,
    });
  });

  it("reports a refusal rather than swallowing it", async () => {
    const { actor } = actorAnswering({
      Err: { Disabled: null },
    } as unknown as { Ok: null });

    await expect(
      allowApp({ identityNumber: IDENTITY, origin: ORIGIN, actor }),
    ).rejects.toThrow();
  });
});

describe("enabling notifications", () => {
  beforeEach(() => {
    register.mockClear();
    permission.mockResolvedValue("granted");
  });

  it("registers the device before recording consent", async () => {
    const { actor, grant } = actorAnswering(ok);

    await expect(
      enableNotifications({ identityNumber: IDENTITY, origin: ORIGIN, actor }),
    ).resolves.toEqual({ status: "enabled" });

    // A refusal at the browser prompt must leave no consent behind, so the
    // registration has to land first.
    expect(register.mock.invocationCallOrder[0]).toBeLessThan(
      grant.mock.invocationCallOrder[0],
    );
  });

  it("records nothing when the prompt is denied", async () => {
    permission.mockResolvedValue("denied");
    const { actor, grant } = actorAnswering(ok);

    await expect(
      enableNotifications({ identityNumber: IDENTITY, origin: ORIGIN, actor }),
    ).resolves.toEqual({ status: "denied" });

    expect(register).not.toHaveBeenCalled();
    expect(grant).not.toHaveBeenCalled();
  });

  it("reports a dismissed prompt apart from a denial", async () => {
    permission.mockResolvedValue("default");
    const { actor } = actorAnswering(ok);

    await expect(
      enableNotifications({ identityNumber: IDENTITY, origin: ORIGIN, actor }),
    ).resolves.toEqual({ status: "dismissed" });

    expect(register).not.toHaveBeenCalled();
  });
});
