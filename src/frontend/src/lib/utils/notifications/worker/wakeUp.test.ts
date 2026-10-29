import { beforeEach, describe, expect, it, vi } from "vitest";
import { Principal } from "@icp-sdk/core/principal";
import type { Identity } from "@icp-sdk/core/agent";
import type { NotificationEvent } from "$lib/generated/internet_identity_types";

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
        browser_get_notification: () =>
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
  origin: [ORIGIN] as [] | [string],
  account_number: [] as [],
  canister_id: SENDER,
  id: BigInt(42),
  // Sent by an app, which is what the worker fetches text from.
  event: [] as [] | [NotificationEvent],
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
            tag: entry.options.tag,
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
    browser_get_notification: vi.fn(() =>
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

/** What a wake-up names, as the push carries it. */
const NAMED = {
  anchorNumber: IDENTITY,
  browserId: 1,
  inboxNotificationId: BigInt(3),
};

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

    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );

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
    await onWakeUp({ registration: registration() }, NAMED);

    expect(createSync).toHaveBeenCalledWith(
      expect.objectContaining(AGENT_OPTIONS),
    );
  });

  it("asks the app that sent the notification", async () => {
    await onWakeUp(
      {
        registration: registration(),
        internetIdentity: internetIdentity([notification]).factory,
      },
      NAMED,
    );

    expect(fetchNotificationContent).toHaveBeenCalledWith(
      expect.objectContaining({ appCanisterId: SENDER, id: BigInt(42) }),
    );
  });

  it("mints a delegation it holds none of and shows the app's text", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    loadPullIdentity.mockResolvedValue(undefined);
    fetchAppMetadata.mockResolvedValue({ name: "Example App" });

    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );

    expect(mintPullIdentity).toHaveBeenCalledOnce();
    expect(shown.shown.map((entry) => entry.title)).toEqual([
      "Example App · New message",
    ]);
    expect(ii.removed).toHaveLength(1);
  });

  it("shows the placeholder where the delegation does not arrive in time", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    loadPullIdentity.mockResolvedValue(undefined);
    // Answers only once the test says so, which is how the mint is still running while the screen is checked.
    let finishMint = (_identity: Identity | undefined) => undefined as void;
    mintPullIdentity.mockImplementation(
      () =>
        new Promise<Identity | undefined>((resolve) => {
          finishMint = resolve;
        }),
    );

    let returned = false;
    const wakeUp = onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    ).then(() => {
      returned = true;
    });
    // The placeholder is owed at the deadline rather than at the end of the wake-up.
    await vi.waitFor(
      () => expect(shown.shown[0]?.title).toBe("Internet Identity"),
      {
        timeout: 10_000,
        interval: 100,
      },
    );
    await new Promise((settle) => setTimeout(settle, 50));

    // Returning here would leave the delegation the still-running mint is about to answer with nowhere to be cached.
    expect(returned).toBe(false);
    finishMint(undefined);
    await wakeUp;
    expect(returned).toBe(true);

    expect(fetchNotificationContent).not.toHaveBeenCalled();
    // A wake-up that left its entry queued would put the queue one behind every
    // wake-up after it, so the entry is spent even though only the placeholder
    // could be shown for it.
    expect(ii.removed).toHaveLength(1);
  }, 15_000);

  it("replaces the placeholder with the app's own text when the mint lands late", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    loadPullIdentity.mockResolvedValue(undefined);
    let finishMint = (_identity: Identity | undefined) => undefined as void;
    mintPullIdentity.mockImplementation(
      () =>
        new Promise<Identity | undefined>((resolve) => {
          finishMint = resolve;
        }),
    );

    const wakeUp = onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );
    await vi.waitFor(
      () => expect(shown.shown[0]?.title).toBe("Internet Identity"),
      { timeout: 10_000, interval: 100 },
    );

    finishMint(identity);
    await wakeUp;

    // The placeholder carries this entry's own tag, so the app's text closes it.
    expect(shown.shown.map((entry) => entry.title)).toEqual([
      "Example App · New message",
    ]);
    expect(ii.removed).toHaveLength(1);
  }, 15_000);

  it("says the deadline ran out rather than that the mint failed", async () => {
    const logged = vi
      .spyOn(console, "error")
      .mockImplementation(() => undefined);
    loadPullIdentity.mockResolvedValue(undefined);
    let finishMint = (_identity: Identity | undefined) => undefined as void;
    mintPullIdentity.mockImplementation(
      () =>
        new Promise<Identity | undefined>((resolve) => {
          finishMint = resolve;
        }),
    );

    const wakeUp = onWakeUp(
      {
        registration: registration(),
        internetIdentity: internetIdentity([notification]).factory,
      },
      NAMED,
    );
    await vi.waitFor(() => expect(logged).toHaveBeenCalled(), {
      timeout: 10_000,
      interval: 100,
    });

    expect(String(logged.mock.calls[0][0])).toContain("still being minted");
    finishMint(undefined);
    await wakeUp;
  }, 15_000);

  it("takes the placeholder off screen once the real text arrives", async () => {
    const shown = registration();
    fetchAppMetadata.mockResolvedValue({ name: "Example App" });

    await onWakeUp(
      {
        registration: shown,
        internetIdentity: internetIdentity([]).factory,
      },
      NAMED,
    );
    expect(shown.shown.map((entry) => entry.title)).toEqual([
      "Internet Identity",
    ]);

    await onWakeUp(
      {
        registration: shown,
        internetIdentity: internetIdentity([notification]).factory,
      },
      NAMED,
    );

    expect(shown.shown.map((entry) => entry.title)).toEqual([
      "Example App · New message",
    ]);
  });

  it("shows what Internet Identity itself sent, without minting anything", async () => {
    const shown = registration();
    const ii = internetIdentity([
      {
        ...notification,
        event: [
          {
            NotificationsOn: {
              anchor_number: BigInt(10_000),
              name: ["Work"] as [string],
            },
          },
        ],
      },
    ]);

    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );

    expect(shown.shown[0].title).toBe("Notifications are on");
    // Named, because nobody recognises an identity by its number.
    expect(shown.shown[0].options.body).toContain("Work");
    // One notification, and no delegation minted for it.
    expect(shown.shown).toHaveLength(1);
    expect(mintPullIdentity).not.toHaveBeenCalled();
    // Taken off the queue, since it has been shown and nothing is coming back for it.
    expect(ii.removed).toHaveLength(1);
  });

  it("takes content the app no longer serves as a dismissal", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    fetchNotificationContent.mockResolvedValue(undefined);

    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );

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

    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );

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
    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );
    expect(shown.shown).toHaveLength(1);

    fetchNotificationContent.mockRejectedValue(new Error("unreachable"));
    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );

    expect(shown.shown.map((entry) => entry.title)).toEqual([
      "Example App · New message",
    ]);
  });

  it("closes what an app has dismissed since it was shown", async () => {
    const shown = registration();
    const ii = internetIdentity([notification]);
    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );
    expect(shown.shown).toHaveLength(1);

    fetchNotificationContent.mockResolvedValue(undefined);
    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );

    expect(shown.shown).toEqual([
      expect.objectContaining({ title: "Internet Identity" }),
    ]);
  });

  it("asks only the identity the wake-up names, for the entry it names", async () => {
    signing.mockResolvedValue([signs(BigInt(10_001)), signs(IDENTITY)]);
    const ii = internetIdentity([notification]);

    await onWakeUp(
      {
        registration: registration(),
        internetIdentity: ii.factory,
      },
      NAMED,
    );

    expect(ii.actor.browser_get_notification).toHaveBeenCalledTimes(1);
    expect(ii.actor.browser_get_notification).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
      browser_id: NAMED.browserId,
      inbox_notification_id: NAMED.inboxNotificationId,
    });
  });

  it("asks nobody where no key signs for the identity the wake-up names", async () => {
    signing.mockResolvedValue([signs(BigInt(10_001))]);
    const ii = internetIdentity([notification]);
    const shown = registration();

    await onWakeUp(
      { registration: shown, internetIdentity: ii.factory },
      NAMED,
    );

    expect(ii.actor.browser_get_notification).not.toHaveBeenCalled();
    // `userVisibleOnly` is still owed something.
    expect(shown.shown).toEqual([
      expect.objectContaining({ title: "Internet Identity" }),
    ]);
  });

  it("shows the placeholder where the push carried nothing", async () => {
    const ii = internetIdentity([notification]);
    const shown = registration();

    await onWakeUp(
      { registration: shown, internetIdentity: ii.factory },
      undefined,
    );

    expect(ii.actor.browser_get_notification).not.toHaveBeenCalled();
    expect(shown.shown).toEqual([
      expect.objectContaining({ title: "Internet Identity" }),
    ]);
  });

  it("spends the entry by the name the wake-up gave it", async () => {
    const ii = internetIdentity([notification]);
    fetchAppMetadata.mockResolvedValue({ name: "Example App" });

    await onWakeUp(
      { registration: registration(), internetIdentity: ii.factory },
      NAMED,
    );

    expect(ii.removed).toEqual([
      {
        anchor_number: IDENTITY,
        browser_id: NAMED.browserId,
        inbox_notification_id: NAMED.inboxNotificationId,
      },
    ]);
  });

  it("shows one notification per wake-up, whatever is waiting elsewhere", async () => {
    const shown = registration();
    signing.mockResolvedValue([signs(IDENTITY), signs(BigInt(10_001))]);
    const ii = internetIdentity([notification]);

    await onWakeUp(
      {
        registration: shown,
        internetIdentity: ii.factory,
      },
      NAMED,
    );

    // The canister queues a wake-up per notification, so the second identity's
    // turn comes with its own.
    expect(shown.showNotification).toHaveBeenCalledOnce();
    expect(reportNotificationReceived).toHaveBeenCalledOnce();
  });

  it("does not fail a wake-up that showed its notification", async () => {
    const shown = registration();
    refillJwtPool.mockRejectedValue(new Error("no"));

    await onWakeUp(
      {
        registration: shown,
        internetIdentity: internetIdentity([notification]).factory,
      },
      NAMED,
    );

    expect(shown.shown).toHaveLength(1);
  });
});
