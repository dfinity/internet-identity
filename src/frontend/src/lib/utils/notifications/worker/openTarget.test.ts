import { beforeEach, describe, expect, it, vi } from "vitest";

const readAppKey = vi.fn<() => Promise<unknown>>();
vi.mock("$lib/utils/notifications/notificationAppKey", () => ({
  readAppKey: () => readAppKey(),
}));

const { openLink } = await import("$lib/utils/notifications/worker/openTarget");

const LINK = "https://app.example/chats/7?from=ii";
const VIA_SAFARI = "x-safari-https://app.example/chats/7?from=ii";

/** The worker's clients, answering `refused` to anything named in it. */
const clients = (refused: string[] = []) => {
  const opened: string[] = [];
  return {
    opened,
    api: {
      openWindow: (url: string) => {
        opened.push(url);
        return refused.includes(url)
          ? Promise.reject(new Error("not a supported scheme"))
          : Promise.resolve(null);
      },
    } as unknown as Clients,
  };
};

beforeEach(() => {
  vi.clearAllMocks();
  readAppKey.mockResolvedValue(undefined);
});

describe("opening a notification's link", () => {
  // A browser opens a tab, where the scheme would at best do the same thing.
  it("opens the link itself in a browser", async () => {
    const { api, opened } = clients();

    await openLink(api, LINK);

    expect(opened).toEqual([LINK]);
  });

  /// The installed app gets an in-app browser view for any other origin, so it asks iOS
  /// to hand the link to Safari first.
  it("asks Safari for it in the installed app", async () => {
    readAppKey.mockResolvedValue({ keyPair: {}, identityNumber: BigInt(1) });
    const { api, opened } = clients();

    await openLink(api, LINK);

    expect(opened).toEqual([VIA_SAFARI]);
  });

  /// The scheme is undocumented and has been closed in some containers, so a refusal
  /// leaves the link opening the way it does without any of this.
  it("falls back to the link where the scheme is refused", async () => {
    readAppKey.mockResolvedValue({ keyPair: {}, identityNumber: BigInt(1) });
    const { api, opened } = clients([VIA_SAFARI]);

    await openLink(api, LINK);

    expect(opened).toEqual([VIA_SAFARI, LINK]);
  });

  it("leaves anything that is not https alone", async () => {
    readAppKey.mockResolvedValue({ keyPair: {}, identityNumber: BigInt(1) });
    const { api, opened } = clients();

    await openLink(api, "http://app.example/chats/7");

    expect(opened).toEqual(["http://app.example/chats/7"]);
  });
});
