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
import {
  awaitSessionCreation,
  trackSessionCreation,
} from "$lib/stores/sessionCreation.store";

const mint = vi.mocked(mintApplicationSession);
const register = vi.mocked(ensureRegisteredDevice);
const permission = vi.mocked(requestNotificationPermission);
const ORIGIN = "https://app.example";
const IDENTITY = BigInt(10_000);

/** An actor whose grant answers with `replies` in order, one per call. */
const actorAnswering = (
  ...replies: ({ Ok: null } | { Err: { SessionMissing: null } })[]
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
const missing = { Err: { SessionMissing: null } } as const;

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

  // A second creation would drop the session the sign-in gave the app.
  it("waits for a sign-in already under way instead of minting its own", async () => {
    const { actor, grant } = actorAnswering(missing, ok);
    let completeSignIn: () => void = () => undefined;
    void trackSessionCreation(
      ORIGIN,
      new Promise<void>((resolve) => (completeSignIn = resolve)),
    );

    const granting = allowApp({
      identityNumber: IDENTITY,
      origin: ORIGIN,
      actor,
    });
    // One refusal so far, and it is waiting rather than minting.
    await vi.waitFor(() => expect(grant).toHaveBeenCalledTimes(1));
    expect(mint).not.toHaveBeenCalled();
    completeSignIn();
    await granting;

    expect(grant).toHaveBeenCalledTimes(2);
    expect(mint).not.toHaveBeenCalled();
  });

  // The entry outlives the sign-in, so a consent checking just after one landed still
  // finds it.
  it("does not mint for an origin a sign-in has already finished at", async () => {
    const { actor, grant } = actorAnswering(missing, ok);
    void trackSessionCreation(ORIGIN, Promise.resolve());

    await allowApp({ identityNumber: IDENTITY, origin: ORIGIN, actor });

    expect(grant).toHaveBeenCalledTimes(2);
    expect(mint).not.toHaveBeenCalled();
  });

  it("mints after a sign-in that failed to store one", async () => {
    const { actor, grant } = actorAnswering(missing, ok);
    void trackSessionCreation(
      ORIGIN,
      Promise.reject(new Error("sign-in failed")),
    );
    // The first call waits for the failure, which forgets the origin, so by the second
    // there is nothing left to defer to.
    await expect(awaitSessionCreation(ORIGIN)).resolves.toBe(true);
    await expect(awaitSessionCreation(ORIGIN)).resolves.toBe(false);

    await allowApp({ identityNumber: IDENTITY, origin: ORIGIN, actor });

    expect(grant).toHaveBeenCalledTimes(2);
    expect(mint).toHaveBeenCalledTimes(1);
  });

  it("does not wait on a sign-in at another origin", async () => {
    void trackSessionCreation(
      "https://other.example",
      new Promise<void>(() => {}),
    );

    await expect(awaitSessionCreation(ORIGIN)).resolves.toBe(false);
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
