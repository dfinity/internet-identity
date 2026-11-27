import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { Principal } from "@icp-sdk/core/principal";
import type { Writable } from "svelte/store";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { BrowserCalls } from "$lib/utils/notifications/repairPath";
import { authenticationStore } from "$lib/stores/authentication.store";
import { repairNotificationPath } from "./notificationRepair";
import { notificationConsentStore } from "$lib/stores/notificationConsent.store";
import { UnregisteredBrowserError } from "$lib/utils/notifications/browserActor";
import { get } from "svelte/store";
import { notificationProbeStore } from "$lib/stores/notificationProbe.store";

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
/** Past it, and short of the check's deadline. */
const PAST_THE_PROBE_WAIT_MS = 20_000;
/** Past the longest the check may hold a sign-in. */
const PAST_THE_CHECK_DEADLINE_MS = 41_000;

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
                  has_probe_had_its_answer_time: true,
                  is_disabled: [false],
                },
              ],
      },
    }),
  probe_notification_path: () => Promise.resolve({ Ok: null }),
  remove_unanswered_pwa_webpush_subscriptions: () =>
    Promise.resolve({ Ok: null }),
  disable_notification_app: () => Promise.resolve({ Ok: null }),
};

const repair = () =>
  repairNotificationPath({
    identityNumber: IDENTITY_NUMBER,
    origin: "https://app.example",
    frontendOrigin: "https://app.example",
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

  it("skips an origin that is not a web page's, whatever this browser holds", async () => {
    const browserActor = vi.fn(() => new Promise<BrowserCalls>(() => {}));

    await expect(
      repairNotificationPath({
        identityNumber: IDENTITY_NUMBER,
        origin: "chrome-extension://abcdefghijklmnop",
        frontendOrigin: "chrome-extension://abcdefghijklmnop",
        accountPrincipal: Principal.fromText("2vxsx-fae"),
        actor,
        browserActor,
      }),
    ).resolves.toBe("done");
    expect(browserActor).not.toHaveBeenCalled();
  });

  it("skips a frontend that is not a web page's, whatever this browser holds", async () => {
    const browserActor = vi.fn(() => new Promise<BrowserCalls>(() => {}));

    await expect(
      repairNotificationPath({
        identityNumber: IDENTITY_NUMBER,
        origin: "https://app.example",
        frontendOrigin: "chrome-extension://abcdefghijklmnop",
        accountPrincipal: Principal.fromText("2vxsx-fae"),
        actor,
        browserActor,
      }),
    ).resolves.toBe("done");
    expect(browserActor).not.toHaveBeenCalled();
  });

  it("lets the sign-in go on when the check never answers", async () => {
    const repaired = repairNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: "https://app.example",
      frontendOrigin: "https://app.example",
      accountPrincipal: Principal.fromText("2vxsx-fae"),
      actor,
      browserActor: () => new Promise<BrowserCalls>(() => {}),
    });

    await vi.advanceTimersByTimeAsync(PAST_THE_CHECK_DEADLINE_MS);

    await expect(repaired).resolves.toBe("done");
  });

  it("shows the probe while it waits and takes it down once the check is over", async () => {
    const repaired = repair();
    await vi.advanceTimersByTimeAsync(WHILE_THE_PROBE_WAITS_MS);
    expect(get(notificationProbeStore)).toBe(true);

    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    await expect(repaired).resolves.toBe("done");
    expect(get(notificationProbeStore)).toBe(false);
  });

  it("shows no probe for a lookup that finishes after the deadline", async () => {
    let lookedUp: (browser: BrowserCalls) => void = () => {};
    const repaired = repairNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: "https://app.example",
      frontendOrigin: "https://app.example",
      accountPrincipal: Principal.fromText("2vxsx-fae"),
      actor,
      browserActor: () =>
        new Promise<BrowserCalls>((resolve) => {
          lookedUp = resolve;
        }),
    });
    await vi.advanceTimersByTimeAsync(PAST_THE_CHECK_DEADLINE_MS);
    await expect(repaired).resolves.toBe("done");

    lookedUp(silentInstalledApp);
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    expect(get(notificationProbeStore)).toBe(false);
  });

  it("leaves no unhandled rejection from a check stopped before it started", async () => {
    vi.useRealTimers();
    const unhandled: unknown[] = [];
    const recordUnhandled = (reason: unknown) => unhandled.push(reason);
    process.on("unhandledRejection", recordUnhandled);
    try {
      // The identity changed before the check began, and its lookup then fails.
      signedIn.set({ identityNumber: OTHER_IDENTITY_NUMBER });
      const repaired = repairNotificationPath({
        identityNumber: IDENTITY_NUMBER,
        origin: "https://app.example",
        frontendOrigin: "https://app.example",
        accountPrincipal: Principal.fromText("2vxsx-fae"),
        actor,
        browserActor: () => Promise.reject(new Error("the lookup failed")),
      });

      await expect(repaired).resolves.toBe("identity-changed");
      await new Promise((resolve) => setTimeout(resolve, 10));

      expect(unhandled).toEqual([]);
    } finally {
      process.off("unhandledRejection", recordUnhandled);
    }
  });

  it("tells the sign-in the identity changed when it switched while this browser was looked up", async () => {
    const repaired = repairNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: "https://app.example",
      frontendOrigin: "https://app.example",
      accountPrincipal: Principal.fromText("2vxsx-fae"),
      actor,
      browserActor: () => {
        signedIn.set({ identityNumber: OTHER_IDENTITY_NUMBER });
        return Promise.reject(new UnregisteredBrowserError());
      },
    });
    await vi.advanceTimersByTimeAsync(WHILE_THE_PROBE_WAITS_MS);

    await expect(repaired).resolves.toBe("identity-changed");
  });
});

describe("repairNotificationPath while the allow screen is prepared", () => {
  /** This browser's own path is past its limit and its probe never answers, so the
   *  check asks the user, on the steps that need a permission prompt. */
  const silentBrowser: BrowserCalls = {
    ...silentInstalledApp,
    get_notification_path_status: ({ path }) =>
      Promise.resolve({
        Ok: {
          subscriptions:
            "ThisBrowser" in path
              ? [
                  {
                    unanswered_pushes: [
                      {
                        count: 3,
                        first_sent_at_ns: BigInt(0),
                        is_past_limit: true,
                      },
                    ],
                    probe_sent_at_ns: [BigInt(1)],
                    has_probe_had_its_answer_time: true,
                    is_disabled: [false],
                  },
                ]
              : [],
        },
      }),
  };
  /** Answers the check's consent read, then never answers the screen's. */
  const stallingActor = () => {
    let numReads = 0;
    return {
      notification_consent_granted: () => {
        numReads += 1;
        return numReads === 1
          ? Promise.resolve(true)
          : new Promise<boolean>(() => {});
      },
    } as unknown as ActorSubclass<_SERVICE>;
  };
  const repairStalling = () =>
    repairNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: "https://app.example",
      frontendOrigin: "https://app.example",
      accountPrincipal: Principal.fromText("2vxsx-fae"),
      actor: stallingActor(),
      browserActor: () => Promise.resolve(silentBrowser),
    });

  beforeEach(() => {
    vi.useFakeTimers();
    signedIn.set({ identityNumber: IDENTITY_NUMBER });
  });
  afterEach(() => {
    vi.useRealTimers();
    notificationConsentStore.clear();
  });

  it("lets the sign-in go on when what the screen shows never loads", async () => {
    const repaired = repairStalling();

    await vi.advanceTimersByTimeAsync(PAST_THE_CHECK_DEADLINE_MS);

    await expect(repaired).resolves.toBe("done");
    expect(get(notificationConsentStore)).toBeUndefined();
  });

  it("tells the sign-in the identity changed when the user switches while the screen loads", async () => {
    const repaired = repairStalling();
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    signedIn.set({ identityNumber: OTHER_IDENTITY_NUMBER });

    await expect(repaired).resolves.toBe("identity-changed");
  });
});

describe("repairNotificationPath on an iPhone", () => {
  const originalAgent = Object.getOwnPropertyDescriptor(
    window.navigator,
    "userAgent",
  );
  beforeEach(() => {
    vi.useFakeTimers();
    signedIn.set({ identityNumber: IDENTITY_NUMBER });
    Object.defineProperty(window.navigator, "userAgent", {
      configurable: true,
      value:
        "Mozilla/5.0 (iPhone; CPU iPhone OS 17_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4 Mobile/15E148 Safari/604.1",
    });
  });
  afterEach(() => {
    vi.useRealTimers();
    notificationConsentStore.clear();
    if (originalAgent === undefined) {
      Reflect.deleteProperty(window.navigator, "userAgent");
    } else {
      Object.defineProperty(window.navigator, "userAgent", originalAgent);
    }
  });

  it("tells the sign-in the identity changed when the user switches on the allow screen", async () => {
    // Nothing installed, and ICP Alerts never answers its probe.
    const silentAlerts: BrowserCalls = {
      ...silentInstalledApp,
      get_notification_path_status: ({ path }) =>
        Promise.resolve({
          Ok: {
            subscriptions:
              "ThisBrowser" in path
                ? [
                    {
                      unanswered_pushes: [
                        {
                          count: 3,
                          first_sent_at_ns: BigInt(0),
                          is_past_limit: true,
                        },
                      ],
                      probe_sent_at_ns: [BigInt(1)],
                      has_probe_had_its_answer_time: true,
                      is_disabled: [false],
                    },
                  ]
                : [],
          },
        }),
    };
    const repaired = repairNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: "https://app.example",
      frontendOrigin: "https://app.example",
      accountPrincipal: Principal.fromText("2vxsx-fae"),
      actor,
      browserActor: () => Promise.resolve(silentAlerts),
    });
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);
    expect(get(notificationConsentStore)?.isRepair).toBe(true);

    signedIn.set({ identityNumber: OTHER_IDENTITY_NUMBER });

    await expect(repaired).resolves.toBe("identity-changed");
  });
});
