import { beforeEach, describe, expect, it, vi } from "vitest";

const readAppKey = vi.fn<() => Promise<unknown>>();
vi.mock("$lib/utils/notifications/notificationAppKey", () => ({
  readAppKey: () => readAppKey(),
}));

const { openable } = await import("$lib/utils/notifications/worker/openTarget");

const LINK = "https://app.example/chats/7?from=ii";

const APP_KEY = { kind: "key", record: { keyPair: {} } };

beforeEach(() => {
  vi.clearAllMocks();
  readAppKey.mockResolvedValue({ kind: "no-key" });
});

describe("what a tapped notification opens", () => {
  it("opens the link itself in a browser", async () => {
    expect(await openable(LINK)).toBe(LINK);
  });

  it("routes through the doorway in the installed app", async () => {
    readAppKey.mockResolvedValue(APP_KEY);

    expect(await openable(LINK)).toBe(
      `/ios-icp-alerts/open#${encodeURIComponent(LINK)}`,
    );
  });

  it("leaves anything that is not https alone", async () => {
    readAppKey.mockResolvedValue(APP_KEY);

    expect(await openable("http://app.example/x")).toBe("http://app.example/x");
  });

  it("routes through the doorway where the key store cannot be read", async () => {
    readAppKey.mockResolvedValue({ kind: "unreadable", reason: "no storage" });

    expect(await openable(LINK)).toBe(
      `/ios-icp-alerts/open#${encodeURIComponent(LINK)}`,
    );
  });
});
