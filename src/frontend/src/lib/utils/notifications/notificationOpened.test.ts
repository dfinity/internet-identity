import { beforeEach, describe, expect, it, vi } from "vitest";
import { Principal } from "@icp-sdk/core/principal";
import type { Identity } from "@icp-sdk/core/agent";

const loadPullIdentity = vi.fn<() => Promise<Identity | undefined>>();
const reportNotificationOpened = vi.fn((_call: unknown) => Promise.resolve());

vi.mock("$lib/utils/notifications/pullDelegation", () => ({
  loadPullIdentity: () => loadPullIdentity(),
}));
vi.mock("$lib/utils/notifications/appNotifications", () => ({
  reportNotificationOpened: (call: unknown) => reportNotificationOpened(call),
}));

const { reportOpened } =
  await import("$lib/utils/notifications/notificationOpened");

const SENDER = "un4fu-tqaaa-aaaab-qadjq-cai";
const REF = {
  identityNumber: BigInt(10_000),
  origin: "https://app.example",
  accountNumber: undefined,
  canisterId: SENDER,
  id: BigInt(42),
};
const LOCATION = {
  search: "?canisterId=rdmx6-jaaaa-aaaaa-aaadq-cai&fetchRootKey=1",
  hostname: "127.0.0.1",
  host: "127.0.0.1:4943",
  protocol: "http:",
};

beforeEach(() => {
  vi.clearAllMocks();
});

describe("reportOpened", () => {
  it("tells the app which of its notifications was acted on", async () => {
    loadPullIdentity.mockResolvedValue({} as Identity);

    await expect(reportOpened({ ref: REF, location: LOCATION })).resolves.toBe(
      true,
    );
    expect(reportNotificationOpened).toHaveBeenCalledWith(
      expect.objectContaining({
        canisterId: Principal.fromText(SENDER),
        id: BigInt(42),
        shouldFetchRootKey: true,
      }),
    );
  });

  it("says nothing where the delegation has expired since it was shown", async () => {
    loadPullIdentity.mockResolvedValue(undefined);

    await expect(reportOpened({ ref: REF, location: LOCATION })).resolves.toBe(
      false,
    );
    expect(reportNotificationOpened).not.toHaveBeenCalled();
  });

  it("says nothing where the worker cannot tell which canister to ask", async () => {
    loadPullIdentity.mockResolvedValue({} as Identity);

    await expect(
      reportOpened({ ref: REF, location: { ...LOCATION, search: "" } }),
    ).resolves.toBe(false);
    expect(reportNotificationOpened).not.toHaveBeenCalled();
  });
});
