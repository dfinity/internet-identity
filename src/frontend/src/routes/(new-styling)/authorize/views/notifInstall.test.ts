import { beforeEach, describe, expect, it, vi } from "vitest";
const { point } = vi.hoisted(() => ({
  point: vi.fn<(url: string | undefined) => void>(),
}));
vi.mock("$lib/utils/notifications/installUrl", () => ({
  openPendingInstallTab: vi.fn(() => ({ opened: true, point })),
  installUrlOrNone: vi.fn(),
}));
vi.mock("$lib/utils/notifications/enableNotifications", () => ({
  allowApp: vi.fn(),
}));

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { allowApp } from "$lib/utils/notifications/enableNotifications";
import {
  openPendingInstallTab,
  installUrlOrNone,
} from "$lib/utils/notifications/installUrl";
import { startAppInstall } from "./notifInstall";

const IDENTITY = BigInt(10_000);
const ORIGIN = "https://app.example";
const ACTOR = {} as ActorSubclass<_SERVICE>;

const openTab = vi.mocked(openPendingInstallTab);
const installUrl = vi.mocked(installUrlOrNone);
const grant = vi.mocked(allowApp);

const press = (consented: boolean) =>
  startAppInstall({
    identityNumber: IDENTITY,
    origin: ORIGIN,
    actor: ACTOR,
    consented,
  });

/** Where the press pointed the tab it opened, `undefined` where it had nothing to point it at. */
const pointedAt = (): string | undefined => {
  const [url] = point.mock.calls[0];
  return url;
};

describe("startAppInstall", () => {
  beforeEach(() => {
    vi.clearAllMocks();
    installUrl.mockResolvedValue("/ios-icp-alerts#first");
    grant.mockResolvedValue(undefined);
  });

  it("opens the tab before anything is awaited", () => {
    press(false);
    expect(openTab).toHaveBeenCalledTimes(1);
  });

  it("points the tab at the token once it is signed", async () => {
    await press(false).url;
    expect(pointedAt()).toBe("/ios-icp-alerts#first");
  });

  it("hands the tab nothing where this browser cannot sign a token", async () => {
    installUrl.mockResolvedValue(undefined);
    await press(false).url;
    expect(pointedAt()).toBeUndefined();
  });

  it("signs a fresh token for every press", async () => {
    installUrl
      .mockResolvedValueOnce("/ios-icp-alerts#first")
      .mockResolvedValueOnce("/ios-icp-alerts#second");

    press(false);
    await expect(press(false).url).resolves.toBe("/ios-icp-alerts#second");
    expect(installUrl).toHaveBeenCalledTimes(2);
  });

  it("records the consent where this app does not hold it", async () => {
    await expect(press(false).granted).resolves.toBeUndefined();
    expect(grant).toHaveBeenCalledWith({
      identityNumber: IDENTITY,
      origin: ORIGIN,
      actor: ACTOR,
    });
  });

  it("records nothing where this app holds the consent already", async () => {
    await expect(press(true).granted).resolves.toBeUndefined();
    expect(grant).not.toHaveBeenCalled();
  });

  it("hands back a grant that has not landed yet", async () => {
    let settle: () => void = () => undefined;
    grant.mockReturnValue(
      new Promise<void>((resolve) => {
        settle = resolve;
      }),
    );
    const settled = vi.fn();

    const started = press(false);
    void started.granted.then(settled);
    await Promise.resolve();
    expect(settled).not.toHaveBeenCalled();

    settle();
    await expect(started.granted).resolves.toBeUndefined();
  });

  it("carries a refused grant to whoever awaits it", async () => {
    grant.mockRejectedValue(new Error("consent refused"));
    await expect(press(false).granted).rejects.toThrow("consent refused");
  });
});
