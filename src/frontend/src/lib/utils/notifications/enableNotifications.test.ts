import { beforeEach, describe, expect, it, vi } from "vitest";
// Mocked before the unit is imported. Both reach for a browser: one subscribes a device
// and the other signs in, and what is under test is the order they are reached in.
vi.mock("./subscribeDevice", () => ({
  ensureRegisteredDevice: vi.fn(() => Promise.resolve()),
}));
vi.mock("./pushSubscription", () => ({
  requestNotificationPermission: vi.fn(() => Promise.resolve("granted")),
}));
vi.mock("./mintApplicationSession", () => ({
  mintApplicationSession: vi.fn(() => Promise.resolve()),
}));

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { allowApp, enableNotifications } from "./enableNotifications";
import { mintApplicationSession } from "./mintApplicationSession";
import { ensureRegisteredDevice } from "./subscribeDevice";
import { requestNotificationPermission } from "./pushSubscription";

const mint = vi.mocked(mintApplicationSession);
const register = vi.mocked(ensureRegisteredDevice);
const permission = vi.mocked(requestNotificationPermission);
const ORIGIN = "https://app.example";
const IDENTITY = BigInt(10_000);

/** An actor whose grant answers with `replies` in order, one per call. */
const actorAnswering = (
  ...replies: ({ Ok: null } | { Err: { NoSuchSession: null } })[]
) => {
  const notification_grant_consent = vi.fn(() =>
    Promise.resolve(replies[notification_grant_consent.mock.calls.length - 1]),
  );
  return {
    actor: { notification_grant_consent } as unknown as ActorSubclass<_SERVICE>,
    grant: notification_grant_consent,
  };
};

const ok = { Ok: null } as const;
const missing = { Err: { NoSuchSession: null } } as const;

describe("granting consent", () => {
  beforeEach(() => {
    mint.mockClear();
    mint.mockResolvedValue(undefined);
  });

  it("records consent without signing in where the app is already reached", async () => {
    const { actor, grant } = actorAnswering(ok);

    await allowApp({ identityNumber: IDENTITY, origin: ORIGIN, actor });

    expect(grant).toHaveBeenCalledTimes(1);
    expect(mint).not.toHaveBeenCalled();
  });

  it("signs in and asks again where the identity has never reached the app", async () => {
    const { actor, grant } = actorAnswering(missing, ok);

    await allowApp({ identityNumber: IDENTITY, origin: ORIGIN, actor });

    expect(mint).toHaveBeenCalledTimes(1);
    expect(grant).toHaveBeenCalledTimes(2);
  });

  it("reports an error that signing in would not fix", async () => {
    const { actor } = actorAnswering({
      Err: { Disabled: null },
    } as unknown as { Ok: null });

    await expect(
      allowApp({ identityNumber: IDENTITY, origin: ORIGIN, actor }),
    ).rejects.toThrow();
    expect(mint).not.toHaveBeenCalled();
  });
});

describe("enabling notifications", () => {
  beforeEach(() => {
    mint.mockClear();
    mint.mockResolvedValue(undefined);
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
