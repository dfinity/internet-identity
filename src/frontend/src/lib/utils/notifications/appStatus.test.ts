import { beforeEach, describe, expect, it, vi } from "vitest";

const probe_notification_app = vi.fn();
const get_notification_app_status = vi.fn();

class UnregisteredBrowserError extends Error {}

const browserKeyActor = vi.fn<() => Promise<unknown>>();

vi.mock("$lib/utils/notifications/browserActor", () => ({
  browserKeyActor: () => browserKeyActor(),
  UnregisteredBrowserError,
}));

const { browserAppStatus, probeApp } = await import("./appStatus");

const IDENTITY = BigInt(10_000);

beforeEach(() => {
  vi.clearAllMocks();
  browserKeyActor.mockResolvedValue({
    probe_notification_app,
    get_notification_app_status,
  });
  get_notification_app_status.mockResolvedValue({ NoApp: null });
});

/** What the canister holds for a browser that points at an app. */
const APP = {
  subscribed: true,
  enabled: true,
  last_seen_ns: BigInt(100),
  pending_probe: [],
};

describe("browserAppStatus", () => {
  it("carries back the entry the canister holds", async () => {
    get_notification_app_status.mockResolvedValue({ Linked: APP });

    await expect(browserAppStatus(IDENTITY)).resolves.toEqual({
      kind: "app",
      app: APP,
    });
  });

  it("says this browser points at no app where the canister holds none", async () => {
    await expect(browserAppStatus(IDENTITY)).resolves.toEqual({
      kind: "no-app",
    });
  });

  /** The install nobody has opened yet, which is neither an app nor nothing: the steps stay up and the user is told to open it. */
  it("carries back when an install this browser started stops being spendable", async () => {
    get_notification_app_status.mockResolvedValue({
      AwaitingInstall: BigInt(5_000),
    });

    await expect(browserAppStatus(IDENTITY)).resolves.toEqual({
      kind: "awaiting-install",
      expiresAtNs: BigInt(5_000),
    });
  });

  it("says there is no app for a browser that was never registered", async () => {
    browserKeyActor.mockRejectedValue(new UnregisteredBrowserError());

    await expect(browserAppStatus(IDENTITY)).resolves.toEqual({
      kind: "no-app",
    });
  });

  it("keeps a query that could not be made apart from having no app", async () => {
    get_notification_app_status.mockRejectedValue(new Error("no consensus"));
    const reported = vi
      .spyOn(console, "error")
      .mockImplementation(() => undefined);

    await expect(browserAppStatus(IDENTITY)).resolves.toEqual({
      kind: "unanswered",
      reason: "no consensus",
    });
    expect(reported).toHaveBeenCalledOnce();
  });
});

describe("probeApp", () => {
  it("carries back the id of a probe that was sent", async () => {
    probe_notification_app.mockResolvedValue({ Ok: BigInt(7) });

    await expect(probeApp(IDENTITY)).resolves.toEqual({
      kind: "sent",
      probeId: BigInt(7),
    });
  });

  it("says there is no app where the canister says so", async () => {
    probe_notification_app.mockResolvedValue({
      Err: { NoNotificationApp: null },
    });

    await expect(probeApp(IDENTITY)).resolves.toEqual({ kind: "no-app" });
  });

  it("says there is no app for a browser that was never registered", async () => {
    browserKeyActor.mockRejectedValue(new UnregisteredBrowserError());

    await expect(probeApp(IDENTITY)).resolves.toEqual({ kind: "no-app" });
  });

  it("keeps a probe that was not sent apart from having no app", async () => {
    probe_notification_app.mockResolvedValue({ Err: { NotReachable: null } });

    await expect(probeApp(IDENTITY)).resolves.toEqual({
      kind: "not-sent",
      reason: "NotReachable",
    });
  });

  it("reports a call that failed as nothing sent", async () => {
    probe_notification_app.mockRejectedValue(new Error("no consensus"));
    vi.spyOn(console, "error").mockImplementation(() => undefined);

    await expect(probeApp(IDENTITY)).resolves.toEqual({
      kind: "not-sent",
      reason: "no consensus",
    });
  });
});
