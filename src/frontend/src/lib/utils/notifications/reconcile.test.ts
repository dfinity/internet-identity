import { describe, expect, it, vi } from "vitest";
import {
  reconcile,
  type CanisterPull,
  type PulledNotification,
} from "./reconcile";

const ORIGIN = "https://app.example";
const A = "canister-a";
const B = "canister-b";

interface FakeNotification {
  tag: string;
  data: {
    origin?: string;
    canister?: string;
    id?: string;
    url?: string;
  } | null;
  close: ReturnType<typeof vi.fn>;
}

const shownNotification = (
  canister: string | undefined,
  id: string,
  origin: string | undefined,
): FakeNotification => ({
  tag: canister === undefined ? id : `${canister} ${id}`,
  data: origin === undefined ? null : { origin, canister, id },
  close: vi.fn(),
});

const pending = (id: string): PulledNotification => ({
  id,
  title: `title-${id}`,
  body: `body-${id}`,
});

const known = (canister: string, ...ids: string[]): CanisterPull => ({
  canister,
  pulled: ids.map(pending),
});

const unknown = (canister: string): CanisterPull => ({
  canister,
  pulled: undefined,
});

const fakeRegistration = (shown: FakeNotification[]) => {
  const showNotification = vi.fn(async () => {});
  const registration = {
    getNotifications: () => Promise.resolve(shown as unknown as Notification[]),
    showNotification,
  } as unknown as ServiceWorkerRegistration;
  return { registration, showNotification };
};

describe("reconcile", () => {
  it("closes a canister's notifications that are no longer pending", async () => {
    const stale = shownNotification(A, "a", ORIGIN);
    const kept = shownNotification(A, "b", ORIGIN);
    const { registration } = fakeRegistration([stale, kept]);

    await reconcile(registration, ORIGIN, [known(A, "b")], new Set());

    expect(stale.close).toHaveBeenCalledOnce();
    expect(kept.close).not.toHaveBeenCalled();
  });

  it("an empty (or removed) sender closes all its notifications and shows nothing", async () => {
    const one = shownNotification(A, "a", ORIGIN);
    const two = shownNotification(A, "b", ORIGIN);
    const { registration, showNotification } = fakeRegistration([one, two]);

    await reconcile(
      registration,
      ORIGIN,
      [{ canister: A, pulled: [] }],
      new Set(),
    );

    expect(one.close).toHaveBeenCalledOnce();
    expect(two.close).toHaveBeenCalledOnce();
    expect(showNotification).not.toHaveBeenCalled();
  });

  it("never touches another origin's notifications", async () => {
    const otherApp = shownNotification(A, "a", "https://other.example");
    const noOrigin = shownNotification(A, "b", undefined);
    const { registration } = fakeRegistration([otherApp, noOrigin]);

    await reconcile(
      registration,
      ORIGIN,
      [{ canister: A, pulled: [] }],
      new Set(),
    );

    expect(otherApp.close).not.toHaveBeenCalled();
    expect(noOrigin.close).not.toHaveBeenCalled();
  });

  it("never touches another canister's notifications on the same origin", async () => {
    const otherCanister = shownNotification(B, "a", ORIGIN);
    const { registration } = fakeRegistration([otherCanister]);

    // A is reconciled to empty; B is not in the results at all.
    await reconcile(
      registration,
      ORIGIN,
      [{ canister: A, pulled: [] }],
      new Set(),
    );

    expect(otherCanister.close).not.toHaveBeenCalled();
  });

  it("shows each pending notification tagged by canister and id", async () => {
    const { registration, showNotification } = fakeRegistration([]);

    await reconcile(registration, ORIGIN, [known(A, "x", "y")], new Set());

    expect(showNotification).toHaveBeenCalledTimes(2);
    expect(showNotification).toHaveBeenCalledWith(
      "title-x",
      expect.objectContaining({
        body: "body-x",
        tag: `${A} x`,
        data: { origin: ORIGIN, canister: A, id: "x", url: undefined },
      }),
    );
    expect(showNotification).toHaveBeenCalledWith(
      "title-y",
      expect.objectContaining({ tag: `${A} y` }),
    );
  });

  it("carries a url through to the notification data", async () => {
    const { registration, showNotification } = fakeRegistration([]);
    const withUrl: CanisterPull = {
      canister: A,
      pulled: [
        { id: "x", title: "t", body: "b", url: "https://app.example/deep" },
      ],
    };

    await reconcile(registration, ORIGIN, [withUrl], new Set());

    expect(showNotification).toHaveBeenCalledWith(
      "t",
      expect.objectContaining({
        data: {
          origin: ORIGIN,
          canister: A,
          id: "x",
          url: "https://app.example/deep",
        },
      }),
    );
  });

  it("re-showing the same canister and id replaces in place (same tag)", async () => {
    const existing = shownNotification(A, "x", ORIGIN);
    const { registration, showNotification } = fakeRegistration([existing]);

    await reconcile(registration, ORIGIN, [known(A, "x")], new Set());

    expect(existing.close).not.toHaveBeenCalled();
    expect(showNotification).toHaveBeenCalledOnce();
    expect(showNotification).toHaveBeenCalledWith(
      "title-x",
      expect.objectContaining({ tag: `${A} x` }),
    );
  });

  it("two canisters may use the same id without replacing each other", async () => {
    const { registration, showNotification } = fakeRegistration([]);

    await reconcile(
      registration,
      ORIGIN,
      [known(A, "dup"), known(B, "dup")],
      new Set(),
    );

    expect(showNotification).toHaveBeenCalledTimes(2);
    expect(showNotification).toHaveBeenCalledWith(
      "title-dup",
      expect.objectContaining({
        tag: `${A} dup`,
        data: { origin: ORIGIN, canister: A, id: "dup", url: undefined },
      }),
    );
    expect(showNotification).toHaveBeenCalledWith(
      "title-dup",
      expect.objectContaining({
        tag: `${B} dup`,
        data: { origin: ORIGIN, canister: B, id: "dup", url: undefined },
      }),
    );
  });

  it("a failed (unknown) sender keeps its notifications while a known one reconciles", async () => {
    const keptOnUnknown = shownNotification(A, "a", ORIGIN);
    const staleOnKnown = shownNotification(B, "b", ORIGIN);
    const { registration, showNotification } = fakeRegistration([
      keptOnUnknown,
      staleOnKnown,
    ]);

    // A could not be reached (unknown); B answered empty.
    await reconcile(
      registration,
      ORIGIN,
      [unknown(A), { canister: B, pulled: [] }],
      new Set(),
    );

    expect(keptOnUnknown.close).not.toHaveBeenCalled();
    expect(staleOnKnown.close).toHaveBeenCalledOnce();
    expect(showNotification).not.toHaveBeenCalled();
  });

  it("does not re-show a notification the user has dismissed", async () => {
    const { registration, showNotification } = fakeRegistration([]);

    await reconcile(
      registration,
      ORIGIN,
      [known(A, "x", "y")],
      new Set([`${A} x`]),
    );

    // x is dismissed, so only y is shown.
    expect(showNotification).toHaveBeenCalledOnce();
    expect(showNotification).toHaveBeenCalledWith(
      "title-y",
      expect.objectContaining({ tag: `${A} y` }),
    );
  });

  it("reports a dismissed tag to forget once its canister stops listing it", async () => {
    const { registration } = fakeRegistration([]);

    const forget = await reconcile(
      registration,
      ORIGIN,
      [known(A, "still")],
      new Set([`${A} gone`, `${A} still`]),
    );

    // The dismissed "gone" is no longer pending, so it is forgotten; "still" is
    // still pending, so it is kept.
    expect(forget).toEqual([`${A} gone`]);
  });

  it("does not forget a dismissal for a canister that did not answer", async () => {
    const { registration } = fakeRegistration([]);

    const forget = await reconcile(
      registration,
      ORIGIN,
      [unknown(A)],
      new Set([`${A} x`]),
    );

    expect(forget).toEqual([]);
  });
});
