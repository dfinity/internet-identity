import { beforeEach, describe, expect, it, vi } from "vitest";
import { Principal } from "@icp-sdk/core/principal";
import type { Identity } from "@icp-sdk/core/agent";

const notificationContent = vi.fn();

vi.mock("@icp-sdk/core/agent", async (importOriginal) => {
  const actual = await importOriginal<typeof import("@icp-sdk/core/agent")>();
  return {
    ...actual,
    Actor: {
      createActor: () => ({
        _internet_identity_notification_content: (id: bigint) =>
          notificationContent(id),
      }),
    },
    HttpAgent: { createSync: () => ({}) },
  };
});

const { fetchNotificationContent } =
  await import("$lib/utils/notifications/appNotifications");

const call = {
  canisterId: Principal.fromText("un4fu-tqaaa-aaaab-qadjq-cai"),
  id: BigInt(42),
  identity: {} as Identity,
  host: "https://icp-api.io",
  shouldFetchRootKey: false,
};

beforeEach(() => {
  vi.clearAllMocks();
});

describe("fetchNotificationContent", () => {
  it("reads what the app answered", async () => {
    notificationContent.mockResolvedValue([
      { title: "New message", body: "See you at six", url: ["/chats/7"] },
    ]);

    await expect(fetchNotificationContent(call)).resolves.toEqual({
      title: "New message",
      body: "See you at six",
      url: "/chats/7",
    });
  });

  it("answers nothing where the app itself says there is nothing", async () => {
    notificationContent.mockResolvedValue([]);

    await expect(fetchNotificationContent(call)).resolves.toBeUndefined();
  });

  it("throws where the app could not be asked", async () => {
    notificationContent.mockRejectedValue(new Error("canister is stopped"));

    // A caller drops the notification on `undefined`, so a call that never
    // reached the app must not look like an answer from it.
    await expect(fetchNotificationContent(call)).rejects.toThrow(
      "canister is stopped",
    );
  });
});
