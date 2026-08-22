import { describe, expect, it, vi } from "vitest";
import { reconcile, type PulledNotification } from "./reconcile";

const ORIGIN = "https://app.example";

interface FakeNotification {
  tag: string;
  data: { origin?: string } | null;
  close: ReturnType<typeof vi.fn>;
}

const shownNotification = (
  tag: string,
  origin: string | undefined,
): FakeNotification => ({
  tag,
  data: origin === undefined ? null : { origin },
  close: vi.fn(),
});

const pending = (id: string): PulledNotification => ({
  id,
  title: `title-${id}`,
  body: [`body-${id}`],
});

const fakeRegistration = (shown: FakeNotification[]) => {
  const showNotification = vi.fn(async () => {});
  const registration = {
    getNotifications: async () => shown as unknown as Notification[],
    showNotification,
  } as unknown as ServiceWorkerRegistration;
  return { registration, showNotification };
};

describe("reconcile", () => {
  it("closes this origin's notifications that are no longer pending", async () => {
    const stale = shownNotification("a", ORIGIN);
    const kept = shownNotification("b", ORIGIN);
    const { registration } = fakeRegistration([stale, kept]);

    await reconcile(registration, ORIGIN, [pending("b")]);

    expect(stale.close).toHaveBeenCalledOnce();
    expect(kept.close).not.toHaveBeenCalled();
  });

  it("empty pending closes all of this origin's notifications and shows nothing", async () => {
    const one = shownNotification("a", ORIGIN);
    const two = shownNotification("b", ORIGIN);
    const { registration, showNotification } = fakeRegistration([one, two]);

    await reconcile(registration, ORIGIN, []);

    expect(one.close).toHaveBeenCalledOnce();
    expect(two.close).toHaveBeenCalledOnce();
    expect(showNotification).not.toHaveBeenCalled();
  });

  it("never touches another origin's notifications", async () => {
    const otherApp = shownNotification("a", "https://other.example");
    const noOrigin = shownNotification("b", undefined);
    const { registration } = fakeRegistration([otherApp, noOrigin]);

    await reconcile(registration, ORIGIN, []);

    expect(otherApp.close).not.toHaveBeenCalled();
    expect(noOrigin.close).not.toHaveBeenCalled();
  });

  it("shows or replaces each pending notification keyed by its id", async () => {
    const { registration, showNotification } = fakeRegistration([]);

    await reconcile(registration, ORIGIN, [pending("x"), pending("y")]);

    expect(showNotification).toHaveBeenCalledTimes(2);
    expect(showNotification).toHaveBeenCalledWith(
      "title-x",
      expect.objectContaining({
        body: "body-x",
        tag: "x",
        data: { origin: ORIGIN, id: "x" },
      }),
    );
    expect(showNotification).toHaveBeenCalledWith(
      "title-y",
      expect.objectContaining({ tag: "y", data: { origin: ORIGIN, id: "y" } }),
    );
  });

  it("re-showing the same id replaces in place (same tag), not a duplicate", async () => {
    // A notification for id "x" is already on screen; a fresh pull of "x"
    // reuses its tag, so the browser updates rather than stacks.
    const existing = shownNotification("x", ORIGIN);
    const { registration, showNotification } = fakeRegistration([existing]);

    await reconcile(registration, ORIGIN, [pending("x")]);

    expect(existing.close).not.toHaveBeenCalled();
    expect(showNotification).toHaveBeenCalledOnce();
    expect(showNotification).toHaveBeenCalledWith(
      "title-x",
      expect.objectContaining({ tag: "x" }),
    );
  });
});
