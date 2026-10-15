import { beforeEach, describe, expect, it, vi } from "vitest";

const store = new Map<string, number>();
vi.mock("idb-keyval", () => ({
  createStore: () => ({}),
  get: (key: string) => Promise.resolve(store.get(key)),
  set: (key: string, value: number) => {
    store.set(key, value);
    return Promise.resolve();
  },
}));

const { recordShown, shownRecently } =
  await import("$lib/utils/notifications/worker/shownRecord");

const NOW = Date.UTC(2026, 9, 14, 12);

beforeEach(() => {
  store.clear();
  vi.clearAllMocks();
});

describe("the shown record", () => {
  it("answers no before anything has been shown", async () => {
    expect(await shownRecently(NOW)).toBe(false);
  });

  // The question it answers for a wake-up arriving beside one that just showed
  // something, which on iOS is the only way to know the screen is already served.
  it("answers yes just after something was shown", async () => {
    vi.spyOn(Date, "now").mockReturnValue(NOW);
    await recordShown();

    expect(await shownRecently(NOW + 1_000)).toBe(true);
  });

  /// Long enough and the wake-up owes the user something again, since the stamp says
  /// only that something was shown, never that it is still on screen.
  it("answers no once the stamp is stale", async () => {
    vi.spyOn(Date, "now").mockReturnValue(NOW);
    await recordShown();

    expect(await shownRecently(NOW + 31_000)).toBe(false);
  });
});
