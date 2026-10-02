import { beforeEach, describe, expect, it, vi } from "vitest";
import { Principal } from "@icp-sdk/core/principal";
import type { Identity } from "@icp-sdk/core/agent";

const signing =
  vi.fn<() => Promise<{ identityNumber: bigint; identity: Identity }[]>>();
const loadPullIdentity = vi.fn<() => Promise<Identity | undefined>>();
const mintPullIdentity = vi.fn<() => Promise<Identity | undefined>>();
const fetchNotificationContent = vi.fn();
const reportNotificationReceived = vi.fn((_call: unknown) => Promise.resolve());
const fetchAppMetadata = vi.fn((): Promise<{ name?: string } | undefined> =>
  Promise.resolve(undefined),
);
const refillJwtPool = vi.fn((_options: unknown) => Promise.resolve(false));
const fetchAlternativeOrigins = vi.fn(() => Promise.resolve([] as string[]));

const createSync = vi.fn((_options: unknown) => ({}));

const AGENT_OPTIONS = {
  host: "http://127.0.0.1:4943",
  shouldFetchRootKey: true,
};

vi.mock("$lib/utils/notifications/signingIdentities", () => ({
  signingIdentities: () => signing(),
}));
vi.mock("$lib/utils/notifications/workerConfig", () => ({
  config: {
    canisterId: "rdmx6-jaaaa-aaaaa-aaadq-cai",
    agentOptions: AGENT_OPTIONS,
  },
}));
vi.mock("@icp-sdk/core/agent", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@icp-sdk/core/agent")>();
  return {
    ...actual,
    Actor: {
      createActor: () => ({
        browser_get_next_notification: () =>
          Promise.resolve({ Ok: { notification: [] } }),
      }),
    },
    HttpAgent: { createSync: (options: unknown) => createSync(options) },
  };
});
vi.mock("$lib/utils/notifications/worker/pullDelegation", () => ({
  loadPullIdentity: () => loadPullIdentity(),
  mintPullIdentity: () => mintPullIdentity(),
}));
vi.mock("$lib/utils/notifications/worker/poolRefill", () => ({
  refillJwtPool: (options: unknown) => refillJwtPool(options),
}));
vi.mock("$lib/utils/notifications/worker/appNotifications", () => ({
  fetchNotificationContent: (call: unknown) => fetchNotificationContent(call),
  reportNotificationReceived: (call: unknown) =>
    reportNotificationReceived(call),
}));
vi.mock("$lib/utils/appMetadata", () => ({
  fetchAppMetadata: () => fetchAppMetadata(),
  logoAsDataUrl: () => Promise.resolve("data:image/webp;base64,AA=="),
}));
vi.mock(
  "$lib/utils/notifications/worker/notificationLink",
  async (importOriginal) => {
    const actual =
      await importOriginal<
        typeof import("$lib/utils/notifications/worker/notificationLink")
      >();
    return {
      ...actual,
      fetchAlternativeOrigins: () => fetchAlternativeOrigins(),
    };
  },
);

const { onWakeUp } = await import("$lib/utils/notifications/worker/wakeUp");

const IDENTITY = BigInt(10_000);
const ORIGIN = "https://app.example";
const SENDER = Principal.fromText("un4fu-tqaaa-aaaab-qadjq-cai");

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

/** An entry this storage can sign for, as `signingIdentities` answers them. */
const signs = (identityNumber: bigint) => ({ identityNumber, identity });

beforeEach(() => {
  vi.clearAllMocks();
  signing.mockResolvedValue([signs(IDENTITY)]);
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
      internetIdentity: ii.factory,
    });

    expect(shown.showNotification).toHaveBeenCalledWith(
      "Example App · New message",
      {
        body: "See you at six",
        icon: undefined,
        tag: `${IDENTITY}|${ORIGIN}||42`,
        data: expect.objectContaining({ url: `${ORIGIN}/chats/7` }),
      },
    );
    expect(reportNotificationReceived).toHaveBeenCalledOnce();
    expect(ii.removed).toHaveLength(1);
  });

  it("reaches Internet Identity the way the page said to reach it", async () => {
    // Every other case injects the actor, so this is the only cover for the one the
    // worker builds: without the page's options it would talk to mainnet's default
    // host and verify against the wrong root key.
    await onWakeUp({ registration: registration() });

    expect(createSync).toHaveBeenCalledWith(
      expect.objectContaining(AGENT_OPTIONS),
    );
  });

  it("asks the app that sent the notification", async () => {
    await onWakeUp({
      registration: registration(),
      internetIdentity: internetIdentity([notification]).factory,
    });

    expect(fetchNotificationContent).toHaveBeenCalledWith(
      expect.objectContaining({ appCanisterId: SENDER, id: BigInt(42) }),
    );
  });

  it("shows a placeholder and mints a delegation when it holds none", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    loadPullIdentity.mockResolvedValue(undefined);

    await onWakeUp({
      registration: shown,
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
      internetIdentity: ii.factory,
    });

    expect(ii.removed).toHaveLength(1);
    expect(reportNotificationReceived).not.toHaveBeenCalled();
    // Nothing of the app's, and something to satisfy `userVisibleOnly`.
    expect(shown.shown.map((entry) => entry.title)).toEqual([
      "Internet Identity",
    ]);
  });

  it("keeps the notification where the app could not be asked", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    fetchNotificationContent.mockRejectedValue(new Error("unreachable"));

    await onWakeUp({
      registration: shown,
      internetIdentity: ii.factory,
    });

    // Dropping it is what an app's own "nothing" means, and cannot be undone.
    expect(ii.removed).toHaveLength(0);
    expect(reportNotificationReceived).not.toHaveBeenCalled();
    // `userVisibleOnly` still has to be satisfied.
    expect(shown.shown.map((entry) => entry.title)).toEqual([
      "Internet Identity",
    ]);
  });

  it("leaves what is on screen alone where the app could not be asked", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    fetchAppMetadata.mockResolvedValue({ name: "Example App" });
    await onWakeUp({
      registration: shown,
      internetIdentity: ii.factory,
    });
    expect(shown.shown).toHaveLength(1);

    fetchNotificationContent.mockRejectedValue(new Error("unreachable"));
    await onWakeUp({
      registration: shown,
      internetIdentity: ii.factory,
    });

    expect(shown.shown.map((entry) => entry.title)).toEqual([
      "Example App · New message",
    ]);
  });

  it("closes what an app has dismissed since it was shown", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    await onWakeUp({
      registration: shown,
      internetIdentity: ii.factory,
    });
    expect(shown.shown).toHaveLength(1);

    fetchNotificationContent.mockResolvedValue(undefined);
    await onWakeUp({
      registration: shown,
      internetIdentity: ii.factory,
    });

    expect(shown.shown).toEqual([
      expect.objectContaining({ title: "Internet Identity" }),
    ]);
  });

  it("asks for every entry this storage holds a key for", async () => {
    const second = BigInt(10_001);
    signing.mockResolvedValue([signs(IDENTITY), signs(second)]);
    const ii = internetIdentity([
      notification,
      { ...notification, id: BigInt(43) },
    ]);

    await onWakeUp({
      registration: registration(),
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

  it("shows one notification per wake-up, whatever is waiting elsewhere", async () => {
    const shown = registration();
    signing.mockResolvedValue([signs(IDENTITY), signs(BigInt(10_001))]);
    const ii = internetIdentity([notification]);

    await onWakeUp({
      registration: shown,
      internetIdentity: ii.factory,
    });

    // The canister queues a wake-up per notification, so the second identity's
    // turn comes with its own.
    expect(shown.showNotification).toHaveBeenCalledOnce();
    expect(reportNotificationReceived).toHaveBeenCalledOnce();
  });

  it("does not fail a wake-up that showed its notification", async () => {
    const shown = registration();
    refillJwtPool.mockRejectedValue(new Error("no"));

    await onWakeUp({
      registration: shown,
      internetIdentity: internetIdentity([notification]).factory,
    });

    expect(shown.shown).toHaveLength(1);
  });
});
