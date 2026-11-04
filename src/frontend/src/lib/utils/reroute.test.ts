import { describe, it, expect, vi, beforeEach, afterEach } from "vitest";
import { goto, replaceState } from "$app/navigation";
import { DISCOVERABLE_PASSKEY_FLOW } from "$lib/state/featureFlags";
import { manuallyReroute } from "./reroute";

vi.mock("$app/environment", () => ({ browser: true, building: false }));
vi.mock("$app/navigation", () => ({
  goto: vi.fn(() => Promise.resolve()),
  replaceState: vi.fn(),
}));

/** Point `window.location` at `href`; reroute only reads href/origin. */
const setLocation = (href: string) => {
  Object.defineProperty(window, "location", {
    configurable: true,
    value: new URL(href),
    writable: true,
  });
};

describe("manuallyReroute", () => {
  beforeEach(() => {
    vi.mocked(goto).mockClear();
    vi.mocked(replaceState).mockClear();
    DISCOVERABLE_PASSKEY_FLOW.set(true);
  });

  afterEach(() => {
    DISCOVERABLE_PASSKEY_FLOW.getFeatureFlag()?.reset();
  });

  it("treats a same-route reroute as a no-op", async () => {
    // With the discoverable passkey flow on, "/" reroutes to "/": the route
    // we're already on. That must not count as a reroute, or the landing
    // page's onMount bails out before opening a resumed same-tab sign-in.
    setLocation("https://id.ai/");
    const rerouted = await manuallyReroute();
    expect(rerouted).toBe(false);
    expect(goto).not.toHaveBeenCalled();
    expect(replaceState).not.toHaveBeenCalled();
  });

  it("ignores the query string when comparing routes", async () => {
    setLocation("https://id.ai/?authSuccess=1");
    const rerouted = await manuallyReroute();
    expect(rerouted).toBe(false);
    expect(goto).not.toHaveBeenCalled();
  });

  it("reroutes to a different route and restores the visible URL", async () => {
    setLocation("https://id.ai/vc-flow");
    const rerouted = await manuallyReroute();
    expect(rerouted).toBe(true);
    expect(goto).toHaveBeenCalledTimes(1);
    const gotoTarget = vi.mocked(goto).mock.calls[0][0] as URL;
    expect(gotoTarget.pathname).toBe("/vc-flow/index");
    // The visible URL is put back to what the user expects to see.
    expect(replaceState).toHaveBeenCalledTimes(1);
    const restored = vi.mocked(replaceState).mock.calls[0][0] as URL;
    expect(restored.pathname).toBe("/vc-flow");
  });

  it("reports no reroute when none applies", async () => {
    setLocation("https://id.ai/some/other/page");
    const rerouted = await manuallyReroute();
    expect(rerouted).toBe(false);
    expect(goto).not.toHaveBeenCalled();
  });
});
