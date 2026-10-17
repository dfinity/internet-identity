import { beforeEach, describe, expect, it, vi } from "vitest";
vi.mock("$lib/utils/notifications/appStatus", () => ({
  browserAppStatus: vi.fn(),
}));
vi.mock("$lib/utils/notifications/deviceApp", () => ({
  claimDeviceApp: vi.fn(),
}));
vi.mock("$lib/utils/notifications/installUrl", () => ({
  installUrlOrNone: vi.fn(),
}));

import type { NotificationAppInfo } from "$lib/generated/internet_identity_types";
import { browserAppStatus } from "$lib/utils/notifications/appStatus";
import { claimDeviceApp } from "$lib/utils/notifications/deviceApp";
import { installUrlOrNone } from "$lib/utils/notifications/installUrl";
import { turnOnWithApp } from "./notificationsSwitch";

const IDENTITY = BigInt(10_000);

const appStatus = vi.mocked(browserAppStatus);
const claim = vi.mocked(claimDeviceApp);
const installUrl = vi.mocked(installUrlOrNone);

const app = (over: Partial<NotificationAppInfo> = {}) =>
  ({ subscribed: true, enabled: true, ...over }) as NotificationAppInfo;

describe("turnOnWithApp", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    appStatus.mockReset();
    claim.mockResolvedValue("no-donor");
    installUrl.mockResolvedValue("/ios-icp-alerts#first");
  });

  it("points this identity at the app another identity here hands over", async () => {
    claim.mockResolvedValue("claimed");
    appStatus.mockResolvedValue({ kind: "app", app: app() });

    await expect(
      turnOnWithApp({ identityNumber: IDENTITY, ownApp: undefined }),
    ).resolves.toEqual({ delivering: true });
    expect(installUrl).not.toHaveBeenCalled();
  });

  it("offers the install where the app it claimed has no subscription", async () => {
    claim.mockResolvedValue("claimed");
    appStatus.mockResolvedValue({
      kind: "app",
      app: app({ subscribed: false }),
    });

    await expect(
      turnOnWithApp({ identityNumber: IDENTITY, ownApp: undefined }),
    ).resolves.toEqual({
      delivering: false,
      installUrl: "/ios-icp-alerts#first",
    });
  });

  it("offers the install where the claim could not be read back", async () => {
    claim.mockResolvedValue("claimed");
    appStatus.mockResolvedValue({ kind: "unanswered", reason: "offline" });

    await expect(
      turnOnWithApp({ identityNumber: IDENTITY, ownApp: undefined }),
    ).resolves.toMatchObject({ delivering: false });
  });

  it("offers the install where there is no app to claim", async () => {
    await expect(
      turnOnWithApp({ identityNumber: IDENTITY, ownApp: undefined }),
    ).resolves.toEqual({
      delivering: false,
      installUrl: "/ios-icp-alerts#first",
    });
    expect(appStatus).not.toHaveBeenCalled();
  });

  it("claims nothing where this browser points at an app already", async () => {
    await expect(
      turnOnWithApp({
        identityNumber: IDENTITY,
        ownApp: app({ subscribed: false }),
      }),
    ).resolves.toMatchObject({ delivering: false });
    expect(claim).not.toHaveBeenCalled();
  });

  it("signs a fresh token for every press", async () => {
    installUrl
      .mockResolvedValueOnce("/ios-icp-alerts#first")
      .mockResolvedValueOnce("/ios-icp-alerts#second");

    await turnOnWithApp({ identityNumber: IDENTITY, ownApp: undefined });
    await expect(
      turnOnWithApp({ identityNumber: IDENTITY, ownApp: undefined }),
    ).resolves.toEqual({
      delivering: false,
      installUrl: "/ios-icp-alerts#second",
    });
    expect(installUrl).toHaveBeenCalledTimes(2);
  });

  it("carries no install url where this browser cannot sign one", async () => {
    installUrl.mockResolvedValue(undefined);

    await expect(
      turnOnWithApp({ identityNumber: IDENTITY, ownApp: undefined }),
    ).resolves.toEqual({ delivering: false, installUrl: undefined });
  });
});
