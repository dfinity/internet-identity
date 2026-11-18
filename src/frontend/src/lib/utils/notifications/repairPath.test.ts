import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import { Principal } from "@icp-sdk/core/principal";
import type {
  NotificationPath,
  NotificationPathStatus,
  SubscriptionDeliveryStatus,
} from "$lib/generated/internet_identity_types";
import {
  checkNotificationPath,
  type BrowserCalls,
  type IdentityCalls,
} from "./repairPath";
import { currentDeviceSubscription } from "$lib/utils/notifications/pushSubscription";
import { ensureRegisteredDevice } from "$lib/utils/notifications/subscribeDevice";

vi.mock("$lib/utils/notifications/pushSubscription", () => ({
  currentDeviceSubscription: vi.fn(),
}));
vi.mock("$lib/utils/notifications/subscribeDevice", () => ({
  ensureRegisteredDevice: vi.fn(),
}));

const ORIGIN = "https://app.example";
const ACCOUNT = Principal.fromText("2vxsx-fae");
const IDENTITY_NUMBER = BigInt(10_000);
/** Longer than the check waits for a probe. */
const PAST_THE_PROBE_WAIT_MS = 60_000;

const pastLimit: SubscriptionDeliveryStatus = {
  unanswered_pushes: [
    { count: 3, first_sent_at_ns: BigInt(0), is_past_limit: true },
  ],
  probe_sent_at_ns: [],
};
const underLimit: SubscriptionDeliveryStatus = {
  unanswered_pushes: [],
  probe_sent_at_ns: [],
};
/** A probe was sent and nothing fetched it, whatever the count says. */
const probeWaiting: SubscriptionDeliveryStatus = {
  unanswered_pushes: [],
  probe_sent_at_ns: [BigInt(1)],
};

const consenting: IdentityCalls = {
  notification_consent_granted: () => Promise.resolve(true),
};

/** A browser whose paths answer what `answers` gives, call by call. */
const browserAnswering = (answers: {
  installedApps: NotificationPathStatus[];
  thisBrowser: NotificationPathStatus[];
}) => {
  const probed: NotificationPath[] = [];
  const removed: unknown[] = [];
  const asked: NotificationPath[] = [];
  const disabled: bigint[] = [];
  const calls = { probed, removed, asked, disabled };
  const next = (path: NotificationPath): NotificationPathStatus => {
    const queue =
      "ThisBrowser" in path ? answers.thisBrowser : answers.installedApps;
    // The last answer stands once the queue runs out.
    return queue.length > 1 ? (queue.shift() ?? queue[0]) : queue[0];
  };
  const browser: BrowserCalls = {
    get_notification_path_status: ({ path }) => {
      calls.asked.push(path);
      return Promise.resolve({ Ok: next(path) });
    },
    probe_notification_path: ({ path }) => {
      calls.probed.push(path);
      return Promise.resolve({ Ok: null });
    },
    remove_unanswered_pwa_webpush_subscriptions: (request) => {
      calls.removed.push(request);
      return Promise.resolve({ Ok: null });
    },
    disable_notification_app: ({ anchor_number }) => {
      calls.disabled.push(anchor_number);
      return Promise.resolve({ Ok: null });
    },
  };
  return { browser, calls };
};

const check = (browser: BrowserCalls) => {
  const checked = checkNotificationPath({
    identityNumber: IDENTITY_NUMBER,
    origin: ORIGIN,
    accountPrincipal: ACCOUNT,
    actor: consenting,
    browser,
    onPastLimit: () => {},
    signal: new AbortController().signal,
  });
  void vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);
  return checked;
};

describe("checkNotificationPath", () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });
  afterEach(() => {
    vi.useRealTimers();
  });

  it("leaves this browser's own path alone while an installed app delivers", async () => {
    const { browser, calls } = browserAnswering({
      installedApps: [{ subscriptions: [underLimit] }],
      thisBrowser: [{ subscriptions: [pastLimit] }],
    });

    await expect(check(browser)).resolves.toEqual({ kind: "working" });
    expect(calls.probed).toEqual([]);
  });

  it("asks about the installed apps of the account that signed in", async () => {
    const { browser, calls } = browserAnswering({
      installedApps: [
        { subscriptions: [pastLimit] },
        { subscriptions: [probeWaiting] },
        { subscriptions: [] },
      ],
      thisBrowser: [{ subscriptions: [underLimit] }],
    });

    await check(browser);

    expect(calls.asked[0]).toEqual({
      InstalledApps: { origin: ORIGIN, account_principal: ACCOUNT },
    });
    expect(calls.removed).toEqual([
      {
        anchor_number: IDENTITY_NUMBER,
        origin: ORIGIN,
        account_principal: ACCOUNT,
      },
    ]);
  });

  it("takes a probe nobody fetched as unanswered, though the count is clear", async () => {
    const { browser } = browserAnswering({
      installedApps: [{ subscriptions: [] }],
      thisBrowser: [
        { subscriptions: [pastLimit] },
        { subscriptions: [probeWaiting] },
      ],
    });

    await expect(check(browser)).resolves.toEqual({
      kind: "ask",
      installFirst: false,
    });
  });

  it("takes a path a push service emptied while it waited as nothing to probe", async () => {
    const { browser } = browserAnswering({
      installedApps: [{ subscriptions: [] }],
      thisBrowser: [{ subscriptions: [pastLimit] }, { subscriptions: [] }],
    });

    await expect(check(browser)).resolves.toEqual({
      kind: "ask",
      installFirst: false,
    });
  });

  it("takes a probe whose record a fetch cleared as answered", async () => {
    const { browser } = browserAnswering({
      installedApps: [{ subscriptions: [] }],
      thisBrowser: [
        { subscriptions: [pastLimit] },
        { subscriptions: [underLimit] },
      ],
    });

    await expect(check(browser)).resolves.toEqual({ kind: "working" });
  });

  it("waits until a probe accepted late has had its answer time before removing", async () => {
    const { browser, calls } = browserAnswering({
      installedApps: [{ subscriptions: [pastLimit] }],
      thisBrowser: [{ subscriptions: [] }],
    });
    let acceptedAtNs = BigInt(0);
    // The push service accepts the probe 5 seconds after it is submitted, and nothing
    // fetches it.
    const lateBrowser: BrowserCalls = {
      ...browser,
      probe_notification_path: (request) => {
        acceptedAtNs = BigInt(Date.now() + 5_000) * BigInt(1_000_000);
        return browser.probe_notification_path(request);
      },
      get_notification_path_status: (request) =>
        "InstalledApps" in request.path && acceptedAtNs > BigInt(0)
          ? Promise.resolve({
              Ok: {
                subscriptions: [
                  { ...pastLimit, probe_sent_at_ns: [acceptedAtNs] },
                ],
              },
            })
          : browser.get_notification_path_status(request),
    };

    void checkNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: ORIGIN,
      accountPrincipal: ACCOUNT,
      actor: consenting,
      browser: lateBrowser,
      onPastLimit: () => {},
      signal: new AbortController().signal,
    }).catch(() => {});

    // Past the usual wait, the probe is only a few seconds old.
    await vi.advanceTimersByTimeAsync(9_000);
    expect(calls.removed).toEqual([]);

    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);
    expect(calls.removed).toHaveLength(1);
  });

  it("keeps the installed app that answered and probes nothing else", async () => {
    const { browser, calls } = browserAnswering({
      installedApps: [
        { subscriptions: [pastLimit, underLimit] },
        { subscriptions: [probeWaiting, underLimit] },
        { subscriptions: [probeWaiting, underLimit] },
        { subscriptions: [underLimit] },
      ],
      thisBrowser: [{ subscriptions: [pastLimit] }],
    });

    await expect(check(browser)).resolves.toEqual({ kind: "working" });
    expect(calls.removed).toHaveLength(1);
    expect(calls.probed).toEqual([
      { InstalledApps: { origin: ORIGIN, account_principal: ACCOUNT } },
    ]);
  });
});

describe("checkNotificationPath on an iPhone", () => {
  const originalAgent = Object.getOwnPropertyDescriptor(
    window.navigator,
    "userAgent",
  );
  beforeEach(() => {
    vi.useFakeTimers();
    Object.defineProperty(window.navigator, "userAgent", {
      configurable: true,
      value:
        "Mozilla/5.0 (iPhone; CPU iPhone OS 17_4 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4 Mobile/15E148 Safari/604.1",
    });
  });
  afterEach(() => {
    vi.useRealTimers();
    if (originalAgent === undefined) {
      Reflect.deleteProperty(window.navigator, "userAgent");
    } else {
      Object.defineProperty(window.navigator, "userAgent", originalAgent);
    }
  });

  it("switches the silent Home Screen app off before offering to install it again", async () => {
    const { browser, calls } = browserAnswering({
      installedApps: [{ subscriptions: [] }],
      thisBrowser: [
        { subscriptions: [pastLimit] },
        { subscriptions: [probeWaiting] },
      ],
    });

    await expect(check(browser)).resolves.toEqual({
      kind: "ask",
      installFirst: true,
    });
    expect(calls.disabled).toEqual([IDENTITY_NUMBER]);
  });
});

describe("checkNotificationPath where this browser may notify", () => {
  const original = Object.getOwnPropertyDescriptor(globalThis, "Notification");
  beforeEach(() => {
    vi.useFakeTimers();
    Object.defineProperty(globalThis, "Notification", {
      configurable: true,
      value: { permission: "granted" },
    });
  });
  afterEach(() => {
    vi.useRealTimers();
    if (original === undefined) {
      Reflect.deleteProperty(globalThis, "Notification");
    } else {
      Object.defineProperty(globalThis, "Notification", original);
    }
  });

  it("renews a subscription a push service called gone, without asking", async () => {
    const { browser } = browserAnswering({
      installedApps: [{ subscriptions: [] }],
      thisBrowser: [{ subscriptions: [pastLimit] }, { subscriptions: [] }],
    });
    const renewed: bigint[] = [];

    const checked = checkNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: ORIGIN,
      accountPrincipal: ACCOUNT,
      actor: consenting,
      browser,
      onPastLimit: () => {},
      signal: new AbortController().signal,
      renewSubscription: (identityNumber) => {
        renewed.push(identityNumber);
        return Promise.resolve();
      },
    });
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    await expect(checked).resolves.toEqual({ kind: "working" });
    expect(renewed).toEqual([IDENTITY_NUMBER]);
  });

  it("acts on no probe refusal that arrives after the identity changed", async () => {
    const switched = new AbortController();
    const { browser } = browserAnswering({
      installedApps: [{ subscriptions: [] }],
      thisBrowser: [{ subscriptions: [pastLimit] }],
    });
    // The user switches while the probe is on its way, and it answers that nothing on
    // the path can be reached.
    const switchingBrowser: BrowserCalls = {
      ...browser,
      probe_notification_path: () => {
        switched.abort(new Error("switched"));
        return Promise.resolve({ Err: { NotReachable: null } });
      },
    };
    const renewed: bigint[] = [];

    const checked = checkNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: ORIGIN,
      accountPrincipal: ACCOUNT,
      actor: consenting,
      browser: switchingBrowser,
      onPastLimit: () => {},
      signal: switched.signal,
      renewSubscription: (identityNumber) => {
        renewed.push(identityNumber);
        return Promise.resolve();
      },
    });
    const settled = checked.then(
      () => "resolved",
      () => "rejected",
    );
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    await expect(settled).resolves.toBe("rejected");
    expect(renewed).toEqual([]);
  });

  it("sends no probe once the identity changed during the lookup before it", async () => {
    const switched = new AbortController();
    const { browser, calls } = browserAnswering({
      installedApps: [{ subscriptions: [] }],
      thisBrowser: [{ subscriptions: [pastLimit] }],
    });
    // The user switches while this browser's path is looked up, and it answers late.
    const switchingBrowser: BrowserCalls = {
      ...browser,
      get_notification_path_status: (request) => {
        if ("ThisBrowser" in request.path) {
          switched.abort(new Error("switched"));
        }
        return browser.get_notification_path_status(request);
      },
    };

    const checked = checkNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: ORIGIN,
      accountPrincipal: ACCOUNT,
      actor: consenting,
      browser: switchingBrowser,
      onPastLimit: () => {},
      signal: switched.signal,
    });
    const settled = checked.then(
      () => "resolved",
      () => "rejected",
    );
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    await expect(settled).resolves.toBe("rejected");
    expect(calls.probed).toEqual([]);
  });

  it("drops and registers nothing once the identity changed during the renewal's lookup", async () => {
    const switched = new AbortController();
    const { browser } = browserAnswering({
      installedApps: [{ subscriptions: [] }],
      thisBrowser: [{ subscriptions: [pastLimit] }, { subscriptions: [] }],
    });
    const unsubscribe = vi.fn(() => Promise.resolve(true));
    // The user switches while this browser's subscription is looked up.
    vi.mocked(currentDeviceSubscription).mockImplementation(() => {
      switched.abort(new Error("switched"));
      return Promise.resolve({ unsubscribe } as unknown as PushSubscription);
    });

    const checked = checkNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: ORIGIN,
      accountPrincipal: ACCOUNT,
      actor: consenting,
      browser,
      onPastLimit: () => {},
      signal: switched.signal,
    });
    const settled = checked.then(
      () => "resolved",
      () => "rejected",
    );
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    await expect(settled).resolves.toBe("rejected");
    expect(unsubscribe).not.toHaveBeenCalled();
    expect(vi.mocked(ensureRegisteredDevice)).not.toHaveBeenCalled();
  });

  it("acts on no answer that arrives after the identity changed", async () => {
    const switched = new AbortController();
    const { browser } = browserAnswering({
      installedApps: [{ subscriptions: [] }],
      thisBrowser: [{ subscriptions: [pastLimit] }],
    });
    let numPolls = 0;
    // The user switches while the poll's query is on its way, and it answers that the
    // subscription is gone.
    const switchingBrowser: BrowserCalls = {
      ...browser,
      get_notification_path_status: (request) => {
        if ("ThisBrowser" in request.path) {
          numPolls += 1;
          if (numPolls > 1) {
            switched.abort(new Error("switched"));
            return Promise.resolve({ Ok: { subscriptions: [] } });
          }
        }
        return browser.get_notification_path_status(request);
      },
    };
    const renewed: bigint[] = [];

    const checked = checkNotificationPath({
      identityNumber: IDENTITY_NUMBER,
      origin: ORIGIN,
      accountPrincipal: ACCOUNT,
      actor: consenting,
      browser: switchingBrowser,
      onPastLimit: () => {},
      signal: switched.signal,
      renewSubscription: (identityNumber) => {
        renewed.push(identityNumber);
        return Promise.resolve();
      },
    });
    const settled = checked.then(
      () => "resolved",
      () => "rejected",
    );
    await vi.advanceTimersByTimeAsync(PAST_THE_PROBE_WAIT_MS);

    await expect(settled).resolves.toBe("rejected");
    expect(renewed).toEqual([]);
  });
});
