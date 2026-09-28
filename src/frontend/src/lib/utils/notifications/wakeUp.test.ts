import { beforeEach, describe, expect, it, vi } from "vitest";
import { Principal } from "@icp-sdk/core/principal";
import type { Identity } from "@icp-sdk/core/agent";

const registeredIdentityNumbers = vi.fn<() => Promise<bigint[]>>();
const loadPullIdentity = vi.fn<() => Promise<Identity | undefined>>();
const mintPullIdentity = vi.fn<() => Promise<Identity | undefined>>();
const fetchNotificationContent = vi.fn();
const reportNotificationReceived = vi.fn((_call: unknown) => Promise.resolve());
const fetchAppMetadata = vi.fn((): Promise<{ name?: string } | undefined> =>
  Promise.resolve(undefined),
);
const refillJwtPool = vi.fn((_options: unknown) => Promise.resolve(false));
const fetchAlternativeOrigins = vi.fn(() => Promise.resolve([] as string[]));

vi.mock("$lib/stores/browser-key.store", () => ({
  registeredIdentityNumbers: () => registeredIdentityNumbers(),
  browserKeyIdentity: () => Promise.resolve(undefined),
}));
vi.mock("$lib/utils/notifications/pullDelegation", () => ({
  loadPullIdentity: () => loadPullIdentity(),
  mintPullIdentity: () => mintPullIdentity(),
}));
vi.mock("$lib/utils/notifications/poolRefill", () => ({
  refillJwtPool: (options: unknown) => refillJwtPool(options),
}));
vi.mock("$lib/utils/notifications/appNotifications", () => ({
  fetchNotificationContent: (call: unknown) => fetchNotificationContent(call),
  reportNotificationReceived: (call: unknown) =>
    reportNotificationReceived(call),
}));
vi.mock("$lib/utils/appMetadata", () => ({
  fetchAppMetadata: () => fetchAppMetadata(),
  logoAsDataUrl: () => Promise.resolve("data:image/webp;base64,AA=="),
}));
vi.mock("$lib/utils/notifications/notificationLink", async (importOriginal) => {
  const actual =
    await importOriginal<
      typeof import("$lib/utils/notifications/notificationLink")
    >();
  return {
    ...actual,
    fetchAlternativeOrigins: () => fetchAlternativeOrigins(),
  };
});

const { onWakeUp, sequencer } = await import("$lib/utils/notifications/wakeUp");

const IDENTITY = BigInt(10_000);
const ORIGIN = "https://app.example";
const SENDER = Principal.fromText("un4fu-tqaaa-aaaab-qadjq-cai");
const LOCATION = {
  search: "?canisterId=rdmx6-jaaaa-aaaaa-aaadq-cai&fetchRootKey=1",
  hostname: "127.0.0.1",
  host: "127.0.0.1:4943",
  protocol: "http:",
};

const notification = {
  origin: ORIGIN,
  account_number: [] as [],
  canister_id: SENDER,
  id: BigInt(42),
};

/** A registration that remembers what is on screen, as the browser would. */
const registration = () => {
  const shown: {
    title: string;
    options: NotificationOptions;
    close: () => void;
  }[] = [];
  const self = {
    shown,
    showNotification: vi.fn((title: string, options: NotificationOptions) => {
      const entry = {
        title,
        options,
        close: () => {
          shown.splice(shown.indexOf(entry), 1);
        },
      };
      shown.push(entry);
      return Promise.resolve();
    }),
    getNotifications: vi.fn(({ tag }: { tag?: string } = {}) =>
      Promise.resolve(
        shown
          .filter((entry) => tag === undefined || entry.options.tag === tag)
          .map((entry) => ({
            data: entry.options.data,
            close: entry.close,
          })),
      ),
    ),
  };
  return self as unknown as ServiceWorkerRegistration & typeof self;
};

/** The canister, answering with one notification until it is removed. */
const internetIdentity = (queue: (typeof notification)[]) => {
  const removed: unknown[] = [];
  const actor = {
    browser_get_next_notification: vi.fn(() =>
      Promise.resolve({
        Ok: { notification: queue.length === 0 ? [] : [queue[0]] },
      }),
    ),
    browser_remove_notification: vi.fn((request: unknown) => {
      removed.push(request);
      queue.shift();
      return Promise.resolve({ Ok: {} });
    }),
  };
  return {
    actor,
    removed,
    // eslint-disable-next-line @typescript-eslint/no-explicit-any
    factory: () => Promise.resolve(actor as any),
  };
};

const identity = {} as Identity;

beforeEach(() => {
  vi.clearAllMocks();
  registeredIdentityNumbers.mockResolvedValue([IDENTITY]);
  loadPullIdentity.mockResolvedValue(identity);
  mintPullIdentity.mockResolvedValue(identity);
  fetchNotificationContent.mockResolvedValue({
    title: "New message",
    body: "See you at six",
    url: `${ORIGIN}/chats/7`,
  });
});

describe("onWakeUp", () => {
  it("shows the app's content, tells the app, and removes the entry", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    fetchAppMetadata.mockResolvedValue({ name: "Example App" });

    await onWakeUp({
      registration: shown,
      location: LOCATION,
      internetIdentity: ii.factory,
    });

    expect(shown.showNotification).toHaveBeenCalledWith("New message", {
      body: "See you at six\nExample App",
      icon: undefined,
      tag: `${IDENTITY}|${ORIGIN}||42`,
      data: expect.objectContaining({ url: `${ORIGIN}/chats/7` }),
    });
    expect(reportNotificationReceived).toHaveBeenCalledOnce();
    expect(ii.removed).toHaveLength(1);
  });

  it("carries the deployment's root key setting into the app call", async () => {
    await onWakeUp({
      registration: registration(),
      location: LOCATION,
      internetIdentity: internetIdentity([notification]).factory,
    });

    expect(fetchNotificationContent).toHaveBeenCalledWith(
      expect.objectContaining({ shouldFetchRootKey: true }),
    );
  });

  it("shows a placeholder and mints a delegation when it holds none", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    loadPullIdentity.mockResolvedValue(undefined);

    await onWakeUp({
      registration: shown,
      location: LOCATION,
      internetIdentity: ii.factory,
    });

    expect(shown.shown[0].title).toBe("Internet Identity");
    expect(mintPullIdentity).toHaveBeenCalledOnce();
    expect(fetchNotificationContent).not.toHaveBeenCalled();
    expect(ii.removed).toHaveLength(0);
  });

  it("takes content the app no longer serves as a dismissal", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    fetchNotificationContent.mockResolvedValue(undefined);

    await onWakeUp({
      registration: shown,
      location: LOCATION,
      internetIdentity: ii.factory,
    });

    expect(ii.removed).toHaveLength(1);
    expect(reportNotificationReceived).not.toHaveBeenCalled();
    // Nothing of the app's, and something to satisfy `userVisibleOnly`.
    expect(shown.shown.map((entry) => entry.title)).toEqual([
      "Internet Identity",
    ]);
  });

  it("closes what an app has dismissed since it was shown", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    await onWakeUp({
      registration: shown,
      location: LOCATION,
      internetIdentity: ii.factory,
    });
    expect(shown.shown).toHaveLength(1);

    fetchNotificationContent.mockResolvedValue(undefined);
    await onWakeUp({
      registration: shown,
      location: LOCATION,
      internetIdentity: ii.factory,
    });

    expect(shown.shown).toEqual([
      expect.objectContaining({ title: "Internet Identity" }),
    ]);
  });

  it("asks for every identity this browser holds a key for", async () => {
    const second = BigInt(10_001);
    registeredIdentityNumbers.mockResolvedValue([IDENTITY, second]);
    const ii = internetIdentity([
      notification,
      { ...notification, id: BigInt(43) },
    ]);

    await onWakeUp({
      registration: registration(),
      location: LOCATION,
      internetIdentity: ii.factory,
    });

    expect(ii.actor.browser_get_next_notification).toHaveBeenCalledTimes(2);
    expect(ii.actor.browser_get_next_notification).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
    });
    expect(ii.actor.browser_get_next_notification).toHaveBeenCalledWith({
      anchor_number: second,
    });
  });

  it("shows a placeholder where it cannot tell which canister to ask", async () => {
    const shown = registration();

    await onWakeUp({
      registration: shown,
      location: { ...LOCATION, search: "" },
      internetIdentity: internetIdentity([notification]).factory,
    });

    expect(shown.shown[0].title).toBe("Internet Identity");
    expect(fetchNotificationContent).not.toHaveBeenCalled();
  });
});

describe("topping up the wake-up pool", () => {
  it("is offered on every wake-up, whatever was shown", async () => {
    await onWakeUp({
      registration: registration(),
      location: LOCATION,
      internetIdentity: internetIdentity([notification]).factory,
    });

    expect(refillJwtPool).toHaveBeenCalledWith(
      expect.objectContaining({ identityNumber: IDENTITY }),
    );
  });

  it("does not fail a wake-up that showed its notification", async () => {
    const shown = registration();
    refillJwtPool.mockRejectedValue(new Error("no"));

    await onWakeUp({
      registration: shown,
      location: LOCATION,
      internetIdentity: internetIdentity([notification]).factory,
    });

    expect(shown.shown).toHaveLength(1);
  });
});

describe("sequencer", () => {
  it("runs wake-ups one at a time, in order", async () => {
    const order: string[] = [];
    const settle: (() => void)[] = [];
    const run = (name: string) => () =>
      new Promise<void>((resolve) => {
        order.push(`start ${name}`);
        settle.push(() => {
          order.push(`end ${name}`);
          resolve();
        });
      });
    const next = sequencer();

    const first = next(run("first"));
    const second = next(run("second"));

    // The chain starts its next link in a microtask, so let those run first.
    await new Promise((resolve) => setTimeout(resolve, 0));
    expect(order).toEqual(["start first"]);
    settle[0]();
    await first;
    await new Promise((resolve) => setTimeout(resolve, 0));
    settle[1]();
    await second;

    expect(order).toEqual([
      "start first",
      "end first",
      "start second",
      "end second",
    ]);
  });

  it("does not let a failed wake-up hold up the next", async () => {
    const next = sequencer();
    const failed = next(() => Promise.reject(new Error("no")));

    await expect(failed).rejects.toThrow("no");
    await expect(next(() => Promise.resolve())).resolves.toBeUndefined();
  });
});
