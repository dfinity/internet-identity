import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { Principal } from "@icp-sdk/core/principal";
import type { Writable } from "svelte/store";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { BrowserCalls } from "$lib/utils/notifications/repairPath";
import { authenticationStore } from "$lib/stores/authentication.store";
import { repairNotificationPath } from "./notificationRepair";

vi.mock("$lib/stores/authentication.store", async () => {
  const { writable } = await import("svelte/store");
  return { authenticationStore: writable<unknown>(undefined) };
});
vi.mock("$lib/state/featureFlags", async (importOriginal) => {
  const { readable } = await import("svelte/store");
  return {
    ...(await importOriginal<typeof import("$lib/state/featureFlags")>()),
    PUSH_NOTIFICATIONS: readable(true),
  };
});

const IDENTITY_NUMBER = BigInt(10_000);
const OTHER_IDENTITY_NUMBER = BigInt(10_001);
/** Inside the wait for a probe's answer. */
const WHILE_THE_PROBE_WAITS_MS = 1_000;
/** Past it. */
const PAST_THE_PROBE_WAIT_MS = 60_000;

// Mocked as a plain writable above; only its real type is in scope here.
const signedIn = authenticationStore as unknown as Writable<unknown>;

// Only the consent read is reached before the identity changes.
const actor = {
  notification_consent_granted: () => Promise.resolve(true),
} as unknown as ActorSubclass<_SERVICE>;

/** A browser whose installed app is past its limit and never answers a probe. */
const silentInstalledApp: BrowserCalls = {
  get_notification_path_status: ({ path }) =>
    Promise.resolve({
      Ok: {
        subscriptions:
          "ThisBrowser" in path
            ? []
            : [
                {
                  unanswered_pushes: [
                    {
                      count: 3,
                      first_sent_at_ns: BigInt(0),
                      is_past_limit: true,
                    },
                  ],
                  probe_sent_at_ns: [BigInt(1)],
                },
              ],
      },
    }),
  probe_notification_path: () => Promise.resolve({ Ok: null }),
  remove_unanswered_pwa_webpush_subscriptions: () =>
    Promise.resolve({ Ok: null }),
};

const repair = () =>
  repairNotificationPath({
    identityNumber: IDENTITY_NUMBER,
    origin: "https://app.example",
    accountPrincipal: Principal.fromText("2vxsx-fae"),
    actor,
    browserActor: () => Promise.resolve(silentInstalledApp),
  });

describe("repairNotificationPath", () => {
  beforeEach(() => {
    vi.useFakeTimers();
    signedIn.set({ identityNumber: IDENTITY_NUMBER });
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("tells the sign-in the identity changed when the user switches while it checks", async () => {
    const repaired = repair();
    await vi.advanceTimersByTimeAsync(WHILE_THE_PROBE_WAITS_MS);

    signedIn.set({ identityNumber: OTHER_IDENTITY_NUMBER });
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    await expect(repaired).resolves.toBe("identity-changed");
  });

  it("tells the sign-in the identity changed when the user signs out while it checks", async () => {
    const repaired = repair();
    await vi.advanceTimersByTimeAsync(WHILE_THE_PROBE_WAITS_MS);

    signedIn.set(undefined);
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    await expect(repaired).resolves.toBe("identity-changed");
  });
});
