import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

const browserAppStatus =
  vi.fn<() => Promise<{ subscribed: boolean; enabled: boolean } | undefined>>();

vi.mock("./appStatus", () => ({
  browserAppStatus: () => browserAppStatus(),
}));

const { watchForInstall } = await import("./watchInstall");

const IDENTITY = BigInt(10_000);

/** Long enough to carry any number of polls, which are three seconds apart. */
const A_WHILE_MS = 30_000;

beforeEach(() => {
  vi.useFakeTimers();
  browserAppStatus.mockResolvedValue(undefined);
});

afterEach(() => {
  vi.useRealTimers();
});

describe("watchForInstall", () => {
  it("says so once the app delivers", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue({ subscribed: true, enabled: true });

    watchForInstall(IDENTITY, onDelivering);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);

    expect(onDelivering).toHaveBeenCalledOnce();
  });

  /// Pointing at the app is not delivering through it: the record exists from the app's
  /// first open, and the permission may still be unanswered.
  it("keeps asking while the app is linked but not registered", async () => {
    const onDelivering = vi.fn();
    browserAppStatus.mockResolvedValue({ subscribed: false, enabled: true });

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

  /// The answer can arrive after the caller has moved on, and what the callback does to
  /// the screen cannot be undone.
  it("does not call back for an answer that arrives after it stopped", async () => {
    const onDelivering = vi.fn();
    let answer = (_status: { subscribed: boolean; enabled: boolean }) =>
      undefined as void;
    browserAppStatus.mockImplementation(
      () =>
        new Promise<{ subscribed: boolean; enabled: boolean }>((resolve) => {
          answer = resolve;
        }),
    );

    const stop = watchForInstall(IDENTITY, onDelivering);
    await vi.advanceTimersByTimeAsync(A_WHILE_MS);
    stop();
    answer({ subscribed: true, enabled: true });
    await vi.advanceTimersByTimeAsync(0);

    expect(onDelivering).not.toHaveBeenCalled();
  });
});
