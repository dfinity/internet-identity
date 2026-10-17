import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

type App = {
  subscribed: boolean;
  enabled: boolean;
  last_seen_ns: bigint;
  pending_probe: [] | [bigint];
};

type Status =
  | { kind: "app"; app: App }
  | { kind: "no-app" }
  | { kind: "unanswered"; reason: string };

const browserAppStatus = vi.fn<() => Promise<Status>>();

vi.mock("$lib/utils/notifications/appStatus", () => ({
  browserAppStatus: () => browserAppStatus(),
}));

const { watchForInstall, watchProbe } = await import("./watchInstall");

const reporting = (app: Partial<App>): Status => ({
  kind: "app",
  app: {
    subscribed: true,
    enabled: true,
    last_seen_ns: BigInt(100),
    pending_probe: [],
    ...app,
  },
});

const delivering = reporting({});
const noApp: Status = { kind: "no-app" };
const unanswered: Status = { kind: "unanswered", reason: "network" };

const IDENTITY = BigInt(10_000);

/** Long enough to carry any number of polls, which are three seconds apart. */
const A_WHILE_MS = 30_000;

beforeEach(() => {
  vi.useFakeTimers();
  browserAppStatus.mockResolvedValue(noApp);
});

afterEach(() => {
  vi.useRealTimers();
});

describe("watchForInstall", () => {
  it("says so once the app delivers", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue(delivering);

    watchForInstall(IDENTITY, onDelivering);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onDelivering).toHaveBeenCalledOnce();
  });

  it("keeps asking while the app is linked but not registered", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue(reporting({ subscribed: false }));

    watchForInstall(IDENTITY, onDelivering);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(browserAppStatus.mock.calls.length).toBeGreaterThan(1);
    expect(onDelivering).not.toHaveBeenCalled();
  });

  it("asks nothing more once it is stopped", async () => {
    const onDelivering = vi.fn();

    const stop = watchForInstall(IDENTITY, onDelivering);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    const asked = browserAppStatus.mock.calls.length;
    stop();
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(browserAppStatus).toHaveBeenCalledTimes(asked);
  });

  it("does not call back for an answer that arrives after it stopped", async () => {
    const onDelivering = vi.fn();
    let answer = (_status: Status) => undefined as void;
    browserAppStatus.mockImplementation(
      () =>
        new Promise<Status>((resolve) => {
          answer = resolve;
        }),
    );

    const stop = watchForInstall(IDENTITY, onDelivering);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    stop();
    answer(delivering);
    await vi.advanceTimersByTimeAsync(0);

    expect(onDelivering).not.toHaveBeenCalled();
  });

  it("keeps asking while the app is subscribed but turned off", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue(reporting({ enabled: false }));

    watchForInstall(IDENTITY, onDelivering);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onDelivering).not.toHaveBeenCalled();
  });

  it("does not take the record of a replaced app for the new one", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue(delivering);

    watchForInstall(IDENTITY, onDelivering, {
      replacedLastSeen: BigInt(100),
    });
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onDelivering).not.toHaveBeenCalled();
  });

  it("says so once a replaced app is gone and a new one delivers", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue(noApp);

    watchForInstall(IDENTITY, onDelivering, {
      replacedLastSeen: BigInt(100),
    });
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    browserAppStatus.mockResolvedValue(
      reporting({ last_seen_ns: BigInt(101) }),
    );
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onDelivering).toHaveBeenCalledOnce();
  });

  it("says so once the replaced record is used again, without it going away", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue(
      reporting({ last_seen_ns: BigInt(100) }),
    );

    watchForInstall(IDENTITY, onDelivering, {
      replacedLastSeen: BigInt(100),
    });
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    expect(onDelivering).not.toHaveBeenCalled();

    browserAppStatus.mockResolvedValue(
      reporting({ last_seen_ns: BigInt(101) }),
    );
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onDelivering).toHaveBeenCalledOnce();
  });

  it("keeps asking for a replaced app heard from but not registered", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue(
      reporting({ last_seen_ns: BigInt(101), subscribed: false }),
    );

    watchForInstall(IDENTITY, onDelivering, {
      replacedLastSeen: BigInt(100),
    });
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onDelivering).not.toHaveBeenCalled();
  });

  /// The baseline comes from the caller, which read it while the old app was still the
  /// one referenced. A tab Safari suspended can come back to find the replacement
  /// already answering, and a baseline taken from that first poll is the new app's own.
  it("settles on a replacement that was already answering before the first poll", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue(
      reporting({ last_seen_ns: BigInt(500) }),
    );

    watchForInstall(IDENTITY, onDelivering, {
      replacedLastSeen: BigInt(100),
    });
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onDelivering).toHaveBeenCalledOnce();
  });

  it("settles on nothing while the query cannot be made", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue(unanswered);

    watchForInstall(IDENTITY, onDelivering, {
      replacedLastSeen: BigInt(100),
    });
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onDelivering).not.toHaveBeenCalled();
  });
});

describe("watchProbe", () => {
  const waiting = reporting({ pending_probe: [BigInt(1)] });

  const callbacks = () => ({ onAnswered: vi.fn(), onGone: vi.fn() });

  it("says the app answered once this browser's queue is empty", async () => {
    const handlers = callbacks();
    browserAppStatus.mockResolvedValue(delivering);

    watchProbe(IDENTITY, handlers);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(handlers.onAnswered).toHaveBeenCalledOnce();
    expect(handlers.onGone).not.toHaveBeenCalled();
  });

  it("keeps asking while anything is still queued", async () => {
    const handlers = callbacks();
    browserAppStatus.mockResolvedValue(waiting);

    watchProbe(IDENTITY, handlers);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(browserAppStatus.mock.calls.length).toBeGreaterThan(1);
    expect(handlers.onAnswered).not.toHaveBeenCalled();
  });

  it("says the app is gone once its record is", async () => {
    const handlers = callbacks();
    browserAppStatus.mockResolvedValue(noApp);

    watchProbe(IDENTITY, handlers);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(handlers.onGone).toHaveBeenCalledOnce();
    expect(handlers.onAnswered).not.toHaveBeenCalled();
  });

  it("asks nothing more once it is stopped", async () => {
    const handlers = callbacks();
    browserAppStatus.mockResolvedValue(waiting);

    const stop = watchProbe(IDENTITY, handlers);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    const asked = browserAppStatus.mock.calls.length;
    stop();
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(browserAppStatus).toHaveBeenCalledTimes(asked);
  });

  it("does not call back for an answer that arrives after it stopped", async () => {
    const handlers = callbacks();
    let answer = (_status: Status) => undefined as void;
    browserAppStatus.mockImplementation(
      () =>
        new Promise<Status>((resolve) => {
          answer = resolve;
        }),
    );

    const stop = watchProbe(IDENTITY, handlers);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    stop();
    answer(delivering);
    await vi.advanceTimersByTimeAsync(0);

    expect(handlers.onAnswered).not.toHaveBeenCalled();
  });

  it("concludes nothing from an unanswered query", async () => {
    const handlers = callbacks();
    browserAppStatus.mockResolvedValue(unanswered);

    watchProbe(IDENTITY, handlers);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(browserAppStatus.mock.calls.length).toBeGreaterThan(1);
    expect(handlers.onGone).not.toHaveBeenCalled();
    expect(handlers.onAnswered).not.toHaveBeenCalled();
  });
});
