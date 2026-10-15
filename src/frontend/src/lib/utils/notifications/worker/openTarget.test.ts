import { beforeEach, describe, expect, it, vi } from "vitest";

const readAppKey = vi.fn<() => Promise<unknown>>();
vi.mock("$lib/utils/notifications/notificationAppKey", () => ({
  readAppKey: () => readAppKey(),
}));

const { openable } = await import("$lib/utils/notifications/worker/openTarget");

const LINK = "https://app.example/chats/7?from=ii";

beforeEach(() => {
  vi.clearAllMocks();
  readAppKey.mockResolvedValue(undefined);
});

describe("what a tapped notification opens", () => {
  // A browser's window is a tab, already carrying the user's session.
  it("opens the link itself in a browser", async () => {
    expect(await openable(LINK)).toBe(LINK);
  });

  /// The installed app opens a page of its own, because the scheme that hands a link to
  /// Safari is honoured for a page's navigation and not for a worker's window.
  it("routes through the doorway in the installed app", async () => {
    readAppKey.mockResolvedValue({ keyPair: {}, identityNumber: BigInt(1) });

    expect(await openable(LINK)).toBe(
      `/ios-icp-alerts/open#${encodeURIComponent(LINK)}`,
    );
  });

  it("leaves anything that is not https alone", async () => {
    readAppKey.mockResolvedValue({ keyPair: {}, identityNumber: BigInt(1) });

    expect(await openable("http://app.example/x")).toBe("http://app.example/x");
  });

  it("opens directly where the key store cannot be read", async () => {
    readAppKey.mockRejectedValue(new Error("no storage"));

    expect(await openable(LINK)).toBe(LINK);
  });
});
