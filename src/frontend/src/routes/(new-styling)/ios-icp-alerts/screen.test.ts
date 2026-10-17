import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

type App = {
  subscribed: boolean;
  enabled: boolean;
  last_seen_ns: bigint;
  pending_probe: [] | [bigint];
};

type Status =
  | { kind: "app"; app: App }
  | { kind: "awaiting-install"; expiresAtNs: bigint }
  | { kind: "no-app" }
  | { kind: "unanswered"; reason: string };

const browserAppStatus = vi.fn<() => Promise<Status>>();
const appAnchors = vi.fn<() => Promise<bigint[]>>();
const registerForAnyAnchor = vi.fn<(anchors: bigint[]) => Promise<boolean>>();
const setNotificationAppEnabled =
  vi.fn<
    (request: { anchor_number: bigint; enabled: boolean }) => Promise<unknown>
  >();
const linkWithToken =
  vi.fn<() => Promise<{ status: string; reason?: string }>>();
type Fragment =
  | { kind: "token"; token: { identityNumber: bigint } }
  | { kind: "no-token" }
  | { kind: "unreadable"; reason: string };

const decodeLinkToken = vi.fn<() => Fragment>();
const recordAlreadyInstalled = vi.fn<(n: bigint) => Promise<void>>();

vi.mock("$lib/utils/notifications/appStatus", () => ({
  browserAppStatus: () => browserAppStatus(),
}));
vi.mock("$lib/utils/notifications/appAnchors", () => ({
  appAnchors: () => appAnchors(),
}));
vi.mock("$lib/utils/notifications/linkToken", () => ({
  decodeLinkToken: () => decodeLinkToken(),
}));
vi.mock("$lib/utils/notifications/notificationAppKey", () => ({
  ensureAppKey: () => Promise.resolve({ keyPair: {} }),
}));
vi.mock("$lib/utils/notifications/alreadyInstalled", () => ({
  recordAlreadyInstalled: (identityNumber: bigint) =>
    recordAlreadyInstalled(identityNumber),
}));
vi.mock("$lib/utils/notifications/subscribeDevice", () => ({
  registerForAnyAnchor: (anchors: bigint[]) => registerForAnyAnchor(anchors),
}));
vi.mock("$lib/utils/notifications/browserActor", () => ({
  browserKeyActor: () =>
    Promise.resolve({
      set_notification_app_enabled: setNotificationAppEnabled,
    }),
}));
vi.mock("./appKeyActor", () => ({
  appKeyActor: () => Promise.resolve({}),
}));
vi.mock("./link", () => ({
  linkWithToken: () => linkWithToken(),
}));

const {
  afterAlreadyAdded,
  insideTheApp,
  insideTheBrowser,
  settleFromSettings,
} = await import("./screen");

const IDENTITY = BigInt(10_000);
const HASH = "#token";

const reporting = (app: Partial<App>): Status => ({
  kind: "app",
  app: {
    subscribed: true,
    enabled: true,
    last_seen_ns: BigInt(100),
    pending_probe: [],
    ...app,
  },
});

/** Nothing in these tests clears the URL, so the callback records rather than acts. */
const spent = vi.fn();

beforeEach(() => {
  vi.clearAllMocks();
  vi.unstubAllGlobals();
  decodeLinkToken.mockReturnValue({
    kind: "token",
    token: { identityNumber: IDENTITY },
  });
  browserAppStatus.mockResolvedValue({ kind: "no-app" });
  appAnchors.mockResolvedValue([IDENTITY]);
  linkWithToken.mockResolvedValue({ status: "linked" });
  registerForAnyAnchor.mockResolvedValue(true);
  setNotificationAppEnabled.mockResolvedValue({ Ok: null });
  recordAlreadyInstalled.mockResolvedValue(undefined);
});

/** What the app document holds, which is where `Notification.permission` is defined. */
const permissionIs = (permission: NotificationPermission): void => {
  vi.stubGlobal("Notification", { permission });
};

/** A browser that can take a subscription but cannot be asked what it holds. */
const pushStateUnreadable = (): void => {
  vi.stubGlobal("PushManager", class {});
  Object.defineProperty(navigator, "serviceWorker", {
    configurable: true,
    value: {
      getRegistration: () => Promise.reject(new Error("storage is closed")),
    },
  });
};

afterEach(() => {
  Reflect.deleteProperty(navigator, "serviceWorker");
});

describe("what a browser tab shows", () => {
  it("sends a tab with no link token back to where tokens are handed out", async () => {
    decodeLinkToken.mockReturnValue({ kind: "no-token" });

    await expect(insideTheBrowser(HASH)).resolves.toEqual({
      kind: "unlinked",
    });
  });

  it("sends a tab whose link cannot be read back to the host", async () => {
    decodeLinkToken.mockReturnValue({
      kind: "unreadable",
      reason: "the token is missing a field",
    });
    vi.spyOn(console, "error").mockImplementation(() => undefined);

    await expect(insideTheBrowser(HASH)).resolves.toEqual({
      kind: "unlinked",
    });
    expect(browserAppStatus).not.toHaveBeenCalled();
  });

  it("offers the install where this browser points at no app", async () => {
    await expect(insideTheBrowser(HASH)).resolves.toEqual({ kind: "install" });
  });

  it("says there is nothing left to do where the app delivers", async () => {
    browserAppStatus.mockResolvedValue(reporting({}));

    await expect(insideTheBrowser(HASH)).resolves.toEqual({
      kind: "ready",
      inTheApp: false,
    });
  });

  it("says the delivering screen is a tab, so it can carry the way out", async () => {
    browserAppStatus.mockResolvedValue(reporting({}));

    const screen = await insideTheBrowser(HASH);

    expect(screen).toHaveProperty("inTheApp", false);
  });

  it("points at the app where it is there but not subscribed", async () => {
    browserAppStatus.mockResolvedValue(reporting({ subscribed: false }));

    await expect(insideTheBrowser(HASH)).resolves.toEqual({ kind: "open-app" });
  });

  it("turns the app back on where this identity has it switched off", async () => {
    browserAppStatus.mockResolvedValue(reporting({ enabled: false }));

    await expect(insideTheBrowser(HASH)).resolves.toEqual({
      kind: "ready",
      inTheApp: false,
    });
    expect(setNotificationAppEnabled).toHaveBeenCalledWith({
      anchor_number: IDENTITY,
      enabled: true,
    });
  });

  it("leaves an app that is already on alone", async () => {
    browserAppStatus.mockResolvedValue(reporting({}));

    await insideTheBrowser(HASH);

    expect(setNotificationAppEnabled).not.toHaveBeenCalled();
  });

  it("leaves the user at the app where turning it back on failed", async () => {
    browserAppStatus.mockResolvedValue(reporting({ enabled: false }));
    setNotificationAppEnabled.mockRejectedValue(new Error("offline"));
    vi.spyOn(console, "error").mockImplementation(() => undefined);

    await expect(insideTheBrowser(HASH)).resolves.toEqual({
      kind: "open-app",
    });
  });

  it("points at an app switched back on that has never registered", async () => {
    browserAppStatus.mockResolvedValue(
      reporting({ enabled: false, subscribed: false }),
    );

    await expect(insideTheBrowser(HASH)).resolves.toEqual({ kind: "open-app" });
    expect(setNotificationAppEnabled).toHaveBeenCalledOnce();
  });

  it("does not offer the install on a query that could not be made", async () => {
    browserAppStatus.mockResolvedValue({
      kind: "unanswered",
      reason: "offline",
    });

    await expect(insideTheBrowser(HASH)).resolves.toEqual({
      kind: "failed",
      reason: "offline",
    });
  });
});

describe("what the installed app shows", () => {
  it("carries on through a token the canister has stopped accepting", async () => {
    linkWithToken.mockResolvedValue({
      status: "token-refused",
      reason: "NoPendingLink",
    });

    await expect(insideTheApp(HASH, "granted", spent)).resolves.toEqual({
      kind: "ready",
      inTheApp: true,
    });
    expect(registerForAnyAnchor).toHaveBeenCalledOnce();
  });

  it("says the delivering screen is the app, which carries no way out", async () => {
    const screen = await insideTheApp(HASH, "granted", spent);

    expect(screen).toHaveProperty("inTheApp", true);
  });

  it("asks for a fresh token where the refused one left nothing linked", async () => {
    linkWithToken.mockResolvedValue({
      status: "token-refused",
      reason: "NoPendingLink",
    });
    appAnchors.mockResolvedValue([]);

    await expect(insideTheApp(HASH, "granted", spent)).resolves.toEqual({
      kind: "stale-link",
    });
  });

  it("asks for a fresh app where the canister refused this app's key", async () => {
    linkWithToken.mockResolvedValue({
      status: "key-refused",
      reason: "KeyAlreadyInUse",
    });

    await expect(insideTheApp(HASH, "granted", spent)).resolves.toEqual({
      kind: "unusable-key",
    });
    expect(registerForAnyAnchor).not.toHaveBeenCalled();
  });

  it("carries on through a link it cannot read", async () => {
    decodeLinkToken.mockReturnValue({
      kind: "unreadable",
      reason: "the key is 32 bytes, not 96",
    });

    await expect(insideTheApp(HASH, "granted", spent)).resolves.toEqual({
      kind: "ready",
      inTheApp: true,
    });
    expect(registerForAnyAnchor).toHaveBeenCalledOnce();
  });

  it("asks for a fresh link where the unreadable one left nothing linked", async () => {
    decodeLinkToken.mockReturnValue({
      kind: "unreadable",
      reason: "the token is missing a field",
    });
    appAnchors.mockResolvedValue([]);

    await expect(insideTheApp(HASH, "granted", spent)).resolves.toEqual({
      kind: "stale-link",
    });
  });

  it("says the app is connected to nothing where it serves no identity", async () => {
    decodeLinkToken.mockReturnValue({ kind: "no-token" });
    appAnchors.mockResolvedValue([]);

    await expect(insideTheApp(HASH, "granted", spent)).resolves.toEqual({
      kind: "orphaned",
    });
  });

  /// Reaching the canister at all is the one failure a launch cannot carry on through.
  it("stops where the link call itself failed", async () => {
    linkWithToken.mockResolvedValue({ status: "failed", reason: "offline" });

    await expect(insideTheApp(HASH, "granted", spent)).resolves.toEqual({
      kind: "failed",
      reason: "offline",
    });
  });

  it("drops a token the canister took, so a reload does not present it again", async () => {
    await insideTheApp(HASH, "granted", spent);

    expect(spent).toHaveBeenCalledOnce();
  });

  it("leaves a refused token where it is", async () => {
    linkWithToken.mockResolvedValue({
      status: "token-refused",
      reason: "NoPendingLink",
    });

    await insideTheApp(HASH, "granted", spent);

    expect(spent).not.toHaveBeenCalled();
  });

  it("asks for the permission where it has not been answered", async () => {
    await expect(insideTheApp(HASH, "default", spent)).resolves.toEqual({
      kind: "ask",
    });
  });

  it("shows the unblock steps where the permission was refused", async () => {
    await expect(insideTheApp(HASH, "denied", spent)).resolves.toEqual({
      kind: "blocked",
    });
  });

  it("registers nothing where the app serves no identity", async () => {
    appAnchors.mockResolvedValue([]);

    await insideTheApp(HASH, "granted", spent);

    expect(registerForAnyAnchor).not.toHaveBeenCalled();
  });

  it("registers on an ordinary launch that links nothing", async () => {
    decodeLinkToken.mockReturnValue({ kind: "no-token" });

    await expect(insideTheApp(HASH, "granted", spent)).resolves.toEqual({
      kind: "ready",
      inTheApp: true,
    });
    expect(registerForAnyAnchor).toHaveBeenCalledOnce();
  });

  it("offers the canister every identity the app serves", async () => {
    const other = BigInt(20_000);
    appAnchors.mockResolvedValue([IDENTITY, other]);

    await insideTheApp(HASH, "granted", spent);

    expect(registerForAnyAnchor).toHaveBeenCalledWith([IDENTITY, other]);
  });

  it("sends the user to the host where no identity accepted the registration", async () => {
    registerForAnyAnchor.mockResolvedValue(false);

    await expect(insideTheApp(HASH, "granted", spent)).resolves.toEqual({
      kind: "unlinked",
    });
  });
});

describe("coming back from iOS Settings", () => {
  it("carries on where the permission has been granted", async () => {
    permissionIs("granted");

    await expect(settleFromSettings()).resolves.toEqual({
      kind: "ready",
      inTheApp: true,
    });
  });

  it("carries on where the permission is granted and the push state is unreadable", async () => {
    permissionIs("granted");
    pushStateUnreadable();

    await expect(settleFromSettings()).resolves.toEqual({
      kind: "ready",
      inTheApp: true,
    });
  });

  it("shows the unblock steps where the permission is still refused", async () => {
    permissionIs("denied");

    await expect(settleFromSettings()).resolves.toEqual({ kind: "blocked" });
  });
});

describe("saying the app is already there", () => {
  it("records the answer against the identity the token names", async () => {
    browserAppStatus.mockResolvedValue(reporting({}));

    await afterAlreadyAdded(HASH);

    expect(recordAlreadyInstalled).toHaveBeenCalledWith(IDENTITY);
  });

  it("ends the tab rather than promising a link it cannot make", async () => {
    browserAppStatus.mockResolvedValue(reporting({}));

    await expect(afterAlreadyAdded(HASH)).resolves.toEqual({ kind: "closed" });
  });

  /** The install the canister is still holding a key for: the app is there and has not been opened, which is the one thing left to do. */
  it("asks them to open an install that is still waiting for it", async () => {
    browserAppStatus.mockResolvedValue({
      kind: "awaiting-install",
      expiresAtNs: BigInt(5_000),
    });

    await expect(afterAlreadyAdded(HASH)).resolves.toEqual({
      kind: "open-to-finish",
    });
    expect(recordAlreadyInstalled).not.toHaveBeenCalled();
  });

  /** Recording this one would report a device as set up while the app on it holds a key that is spent and a URL that cannot change. */
  it("does not take their word where the canister is holding nothing", async () => {
    await expect(afterAlreadyAdded(HASH)).resolves.toEqual({
      kind: "link-expired",
    });
    expect(recordAlreadyInstalled).not.toHaveBeenCalled();
  });

  it("reports a query it could not make rather than recording it", async () => {
    browserAppStatus.mockResolvedValue({
      kind: "unanswered",
      reason: "offline",
    });

    await expect(afterAlreadyAdded(HASH)).resolves.toEqual({
      kind: "failed",
      reason: "offline",
    });
    expect(recordAlreadyInstalled).not.toHaveBeenCalled();
  });

  it.each([
    { kind: "no-token" } as const,
    { kind: "unreadable", reason: "the token is missing a field" } as const,
  ])(
    "ends the tab where $kind leaves nothing to record against",
    async (fragment) => {
      decodeLinkToken.mockReturnValue(fragment);

      await expect(afterAlreadyAdded(HASH)).resolves.toEqual({
        kind: "closed",
      });
      expect(recordAlreadyInstalled).not.toHaveBeenCalled();
    },
  );
});
