import "fake-indexeddb/auto";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";
import type {
  PrepareNotificationAppLinkError,
  PrepareNotificationAppLinkRequest,
} from "$lib/generated/internet_identity_types";

// Mocked before the unit is imported: the real one reaches for the document the canister
// id is read off, and what is under test is the call this makes with it.
vi.mock("./browserActor", () => ({
  browserKeyActor: vi.fn(),
  UnregisteredBrowserError: class UnregisteredBrowserError extends Error {},
}));

import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";
import {
  installUrlOrNone,
  openInstallTab,
  openPendingInstallTab,
  prepareInstallUrl,
} from "./installUrl";
import { NOTIFICATION_APP_PATH } from "./appPath";
import { importLinkKey } from "./linkKey";
import { decodeLinkToken } from "./linkToken";

const IDENTITY = BigInt(10_772);

/** The tab `window.open` hands back, with the two things this unit does to one. */
const tab = () => ({ close: vi.fn(), location: { href: "" } });

beforeEach(() => {
  vi.spyOn(console, "error").mockImplementation(() => undefined);
  prepare.mockClear();
  prepare.mockResolvedValue({ Ok: null });
  actor.mockResolvedValue({
    prepare_notification_app_link: prepare,
  } as unknown as Awaited<ReturnType<typeof browserKeyActor>>);
});

afterEach(() => {
  vi.restoreAllMocks();
});

/** Lets the `then` the unit attached run before the assertions. */
const settled = () => new Promise((resolve) => setTimeout(resolve, 0));

const actor = vi.mocked(browserKeyActor);

const prepare = vi.fn(
  (
    _request: PrepareNotificationAppLinkRequest,
  ): Promise<{ Ok: null } | { Err: PrepareNotificationAppLinkError }> =>
    Promise.resolve({ Ok: null }),
);

/** The link key the URL ends up carrying, as the app reads it back. */
const linkKeyOf = (url: string): Promise<CryptoKeyPair> => {
  const fragment = decodeLinkToken(url.slice(url.indexOf("#")));
  if (fragment.kind !== "token") {
    throw new Error(`the URL carries no token: ${JSON.stringify(fragment)}`);
  }
  return importLinkKey(fragment.token.linkKey);
};

describe("prepareInstallUrl", () => {
  // The canister matches the app's call against the principal registered here, so a URL
  // carrying any other key installs an app the link refuses.
  it("registers the key it puts in the URL", async () => {
    const url = await prepareInstallUrl({
      identityNumber: IDENTITY,
      replacingExisting: false,
    });

    const carried = await ECDSAKeyIdentity.fromKeyPair(await linkKeyOf(url!));
    const [request] = prepare.mock.calls[0];
    expect(request.anchor_number).toBe(IDENTITY);
    expect(request.link_principal.toText()).toBe(
      carried.getPrincipal().toText(),
    );
  });

  it("points at the app's own path", async () => {
    const url = await prepareInstallUrl({
      identityNumber: IDENTITY,
      replacingExisting: false,
    });

    expect(url).toMatch(new RegExp(`^${NOTIFICATION_APP_PATH}#anchor=10772&`));
  });

  it("mints a key of its own for every install", async () => {
    const one = await prepareInstallUrl({
      identityNumber: IDENTITY,
      replacingExisting: false,
    });
    const other = await prepareInstallUrl({
      identityNumber: IDENTITY,
      replacingExisting: false,
    });

    expect(one).not.toBe(other);
  });

  it("has no URL for a browser no sign-in has registered", async () => {
    actor.mockRejectedValue(new UnregisteredBrowserError());

    await expect(
      prepareInstallUrl({ identityNumber: IDENTITY, replacingExisting: false }),
    ).resolves.toBeUndefined();
  });

  it("raises where this browser's key could not be read", async () => {
    actor.mockRejectedValue(new Error("storage is closed"));

    await expect(
      prepareInstallUrl({ identityNumber: IDENTITY, replacingExisting: false }),
    ).rejects.toThrow("storage is closed");
  });

  // Handing out a URL the canister is not waiting for walks the user through an install
  // that cannot finish.
  it("raises where the canister refused the install", async () => {
    prepare.mockResolvedValue({ Err: { AppAlreadyDelivering: null } });

    await expect(
      prepareInstallUrl({ identityNumber: IDENTITY, replacingExisting: false }),
    ).rejects.toThrow();
  });
});

describe("openInstallTab", () => {
  it("points the tab it opened at a URL that arrives late", async () => {
    const opened = tab();
    vi.spyOn(window, "open").mockReturnValue(opened as unknown as Window);

    expect(openInstallTab(Promise.resolve("/ios-icp-alerts#token"))).toBe(true);
    await settled();

    expect(opened.location.href).toBe("/ios-icp-alerts#token");
    expect(opened.close).not.toHaveBeenCalled();
  });

  it("closes the tab where there is no URL to send it to", async () => {
    const opened = tab();
    vi.spyOn(window, "open").mockReturnValue(opened as unknown as Window);

    openInstallTab(Promise.resolve(undefined));
    await settled();

    expect(opened.close).toHaveBeenCalledOnce();
  });

  it("closes the tab and reports the reason where the URL never arrives", async () => {
    const opened = tab();
    vi.spyOn(window, "open").mockReturnValue(opened as unknown as Window);
    const refused = new Error("storage is closed");

    openInstallTab(Promise.reject(refused));
    await settled();

    expect(opened.close).toHaveBeenCalledOnce();
    expect(console.error).toHaveBeenCalledWith(refused);
  });

  it("reports a tab the browser refused to open", () => {
    vi.spyOn(window, "open").mockReturnValue(null);

    expect(openInstallTab(Promise.resolve("/ios-icp-alerts#token"))).toBe(
      false,
    );
  });
});

describe("installUrlOrNone", () => {
  it("points at the app's own path, with the token in the fragment", async () => {
    const url = await installUrlOrNone(IDENTITY);

    expect(url).toMatch(new RegExp(`^${NOTIFICATION_APP_PATH}#anchor=10772&`));
  });

  it("has nowhere to send a browser no sign-in has registered", async () => {
    actor.mockRejectedValue(new UnregisteredBrowserError());

    await expect(installUrlOrNone(IDENTITY)).resolves.toBeUndefined();
  });

  it("reports the reason and sends them nowhere where the key store is shut", async () => {
    const reason = new Error("the key store is shut");
    actor.mockRejectedValue(reason);

    await expect(installUrlOrNone(IDENTITY)).resolves.toBeUndefined();
    expect(console.error).toHaveBeenCalledWith(reason);
  });
});

describe("openPendingInstallTab", () => {
  it("opens the tab before it has anywhere to point it", () => {
    const opened = tab();
    vi.spyOn(window, "open").mockReturnValue(opened as unknown as Window);

    expect(openPendingInstallTab().opened).toBe(true);
    expect(opened.location.href).toBe("");
  });

  it("points the tab where the press says, once it knows", async () => {
    const opened = tab();
    vi.spyOn(window, "open").mockReturnValue(opened as unknown as Window);

    openPendingInstallTab().point("/ios-icp-alerts#token");
    await settled();

    expect(opened.location.href).toBe("/ios-icp-alerts#token");
  });

  it("closes the tab where the press had nothing to point it at", async () => {
    const opened = tab();
    vi.spyOn(window, "open").mockReturnValue(opened as unknown as Window);

    openPendingInstallTab().point(undefined);
    await settled();

    expect(opened.close).toHaveBeenCalledOnce();
  });

  it("reports a refused tab to the press that opened it", () => {
    vi.spyOn(window, "open").mockReturnValue(null);

    expect(openPendingInstallTab().opened).toBe(false);
  });
});
