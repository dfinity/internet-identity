import { beforeEach, describe, expect, it, vi } from "vitest";
// The permission prompt, the registration and this browser's own storage, which is what these two orchestrate.
vi.mock("$lib/utils/notifications/pushSubscription", () => ({
  requestNotificationPermission: vi.fn(),
}));
vi.mock("$lib/utils/notifications/subscribeDevice", () => ({
  ensureRegisteredDevice: vi.fn(),
}));
vi.mock("$lib/utils/notifications/turnedOff", () => ({
  clearNotificationsOff: vi.fn(),
  recordNotificationsOff: vi.fn(),
}));

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { requestNotificationPermission } from "$lib/utils/notifications/pushSubscription";
import { ensureRegisteredDevice } from "$lib/utils/notifications/subscribeDevice";
import {
  clearNotificationsOff,
  recordNotificationsOff,
} from "$lib/utils/notifications/turnedOff";
import {
  turnOffInBrowser,
  turnOnInBrowser,
} from "./browserNotificationsSwitch";

const IDENTITY = BigInt(10_000);
const BROWSER_ID = 7;

const permission = vi.mocked(requestNotificationPermission);
const register = vi.mocked(ensureRegisteredDevice);
const clearWord = vi.mocked(clearNotificationsOff);
const recordWord = vi.mocked(recordNotificationsOff);

/** The identity's own actor, answering `reply` to the one call this makes. */
const actorAnswering = (
  reply: Awaited<ReturnType<_SERVICE["remove_webpush_subscription"]>>,
) => {
  const remove_webpush_subscription = vi.fn(() => Promise.resolve(reply));
  return {
    actor: {
      remove_webpush_subscription,
    } as unknown as ActorSubclass<_SERVICE>,
    remove: remove_webpush_subscription,
  };
};

const ok = { Ok: null } as const;

/** The press, against the actor each case set up to answer it. */
const turnOff = (actor: ActorSubclass<_SERVICE>) =>
  turnOffInBrowser({ identityNumber: IDENTITY, browserId: BROWSER_ID, actor });

describe("turnOnInBrowser", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    permission.mockResolvedValue("granted");
    clearWord.mockResolvedValue(undefined);
    register.mockResolvedValue("https://push.example/endpoint");
  });

  it("takes back the word before it registers", async () => {
    await expect(turnOnInBrowser(IDENTITY)).resolves.toBe("delivering");

    expect(clearWord).toHaveBeenCalledWith(IDENTITY);
    expect(register).toHaveBeenCalledWith(IDENTITY);
    expect(clearWord.mock.invocationCallOrder[0]).toBeLessThan(
      register.mock.invocationCallOrder[0],
    );
  });

  it("registers nothing where the prompt was refused", async () => {
    permission.mockResolvedValue("denied");

    await expect(turnOnInBrowser(IDENTITY)).resolves.toBe("permission-refused");
    expect(clearWord).not.toHaveBeenCalled();
    expect(register).not.toHaveBeenCalled();
  });

  it("registers nothing where the prompt was closed", async () => {
    permission.mockResolvedValue("default");

    await expect(turnOnInBrowser(IDENTITY)).resolves.toBe("permission-refused");
    expect(register).not.toHaveBeenCalled();
  });

  it("registers nothing where the word could not be taken back", async () => {
    clearWord.mockRejectedValue(new Error("storage is blocked"));

    await expect(turnOnInBrowser(IDENTITY)).rejects.toThrow(
      "storage is blocked",
    );
    expect(register).not.toHaveBeenCalled();
  });
});

describe("turnOffInBrowser", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    recordWord.mockResolvedValue(undefined);
  });

  it("names this browser's own entry", async () => {
    const { actor, remove } = actorAnswering(ok);

    await turnOff(actor);

    expect(remove).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
      browser_id: BROWSER_ID,
    });
  });

  it("drops the registration before it stores the word", async () => {
    const { actor, remove } = actorAnswering(ok);

    await turnOff(actor);

    expect(recordWord).toHaveBeenCalledWith(IDENTITY);
    expect(remove.mock.invocationCallOrder[0]).toBeLessThan(
      recordWord.mock.invocationCallOrder[0],
    );
  });

  it("stores no word where the canister kept the registration", async () => {
    const { actor } = actorAnswering({
      Err: { InternalCanisterError: "the subnet is busy" },
    });

    await expect(turnOff(actor)).rejects.toThrow();
    expect(recordWord).not.toHaveBeenCalled();
  });

  it("reports a word it could not store", async () => {
    const { actor, remove } = actorAnswering(ok);
    recordWord.mockRejectedValue(new Error("storage is full"));

    await expect(turnOff(actor)).rejects.toThrow("storage is full");
    expect(remove).toHaveBeenCalledOnce();
  });
});
