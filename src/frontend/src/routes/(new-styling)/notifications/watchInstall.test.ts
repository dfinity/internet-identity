import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

const notificationAppDelivers = vi.fn<() => Promise<boolean>>();

vi.mock("$lib/utils/notifications/notificationState", () => ({
  notificationAppDelivers: () => notificationAppDelivers(),
}));

const { watchForInstall } = await import("./watchInstall");

const IDENTITY = BigInt(10_000);

/** Long enough to carry any number of polls, which are three seconds apart. */
const A_WHILE_MS = 30_000;

beforeEach(() => {
  vi.useFakeTimers();
  notificationAppDelivers.mockResolvedValue(false);
});

afterEach(() => {
  vi.useRealTimers();
});

describe("watchForInstall", () => {
  it("says so once the app delivers", async () => {
    const onInstalled = vi.fn();
    notificationAppDelivers.mockResolvedValue(true);

    watchForInstall(IDENTITY, onInstalled);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onInstalled).toHaveBeenCalledOnce();
  });

  it("keeps asking while nothing has claimed an entry", async () => {
    const onInstalled = vi.fn();

    watchForInstall(IDENTITY, onInstalled);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(notificationAppDelivers.mock.calls.length).toBeGreaterThan(1);
    expect(onInstalled).not.toHaveBeenCalled();
  });

  it("asks nothing more once it is stopped", async () => {
    const onInstalled = vi.fn();

    const stop = watchForInstall(IDENTITY, onInstalled);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    const asked = notificationAppDelivers.mock.calls.length;
    stop();
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(notificationAppDelivers).toHaveBeenCalledTimes(asked);
  });

  /// The callback takes the tab away, and the answer it acts on can arrive after the
  /// caller has moved on: the query is in flight across the moment it stops watching.
  it("does not call back for an answer that arrives after it stopped", async () => {
    const onInstalled = vi.fn();
    let answer = (_delivers: boolean) => undefined as void;
    notificationAppDelivers.mockImplementation(
      () =>
        new Promise<boolean>((resolve) => {
          answer = resolve;
        }),
    );

    const stop = watchForInstall(IDENTITY, onInstalled);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    stop();
    answer(true);
    await vi.advanceTimersByTimeAsync(0);

    expect(onInstalled).not.toHaveBeenCalled();
  });
});
