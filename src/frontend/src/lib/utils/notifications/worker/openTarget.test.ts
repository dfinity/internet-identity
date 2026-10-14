import { beforeEach, describe, expect, it, vi } from "vitest";

const readAppKey = vi.fn<() => Promise<unknown>>();
vi.mock("$lib/utils/notifications/notificationAppKey", () => ({
  readAppKey: () => readAppKey(),
}));

const { openable } = await import(
  "$lib/utils/notifications/worker/openTarget"
);

const LINK = "https://app.example/chats/7?from=ii";

beforeEach(() => {
  vi.clearAllMocks();
  readAppKey.mockResolvedValue(undefined);
});

describe("what a tapped notification opens", () => {
  // A browser's own worker opens a tab, where a doorway page would only be a redirect
  // the user can watch happen.
  it("opens the app's link directly in a browser", async () => {
    expect(await openable(LINK)).toBe(LINK);
  });

  /// The Home Screen app opens a page of its own instead, because iOS gives it an
  /// in-app browser view for any window opened on another origin.
  it("routes through the doorway in the installed app", async () => {
    readAppKey.mockResolvedValue({ keyPair: {}, identityNumber: BigInt(1) });

    expect(await openable(LINK)).toBe(
      `/notifications/open#${encodeURIComponent(LINK)}`,
    );
  });

  it("opens directly where the key store cannot be read", async () => {
    readAppKey.mockRejectedValue(new Error("no storage"));

    expect(await openable(LINK)).toBe(LINK);
  });
});
