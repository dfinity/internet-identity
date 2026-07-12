import { beforeEach, describe, expect, it, vi } from "vitest";
import type { AnalyticsConfig } from "$lib/generated/internet_identity_types";

// Mock the Plausible tracker: `initAnalytics` calls the default export to build
// a tracker exposing `trackEvent` / `trackPageview`, which we spy on.
const { plausibleFactory, trackEvent, trackPageview } = vi.hoisted(() => ({
  plausibleFactory: vi.fn(),
  trackEvent: vi.fn(),
  trackPageview: vi.fn(),
}));

vi.mock("plausible-tracker", () => ({ default: plausibleFactory }));

// Keep `withAuthorizationOrigin` deterministic and free of app deps: an empty
// channel store means no `authorizationOrigin` is added to props.
vi.mock("$lib/stores/channelStore", async () => {
  const { writable } = await import("svelte/store");
  return { establishedChannelStore: writable(undefined) };
});

// A config that `convertToPlausibleConfig` accepts (opt fields are `[] | [v]`).
const ENABLED_CONFIG = {
  Plausible: {
    domain: ["identity.internetcomputer.org"],
    track_localhost: [],
    hash_mode: [],
    api_host: [],
  },
} as unknown as AnalyticsConfig;

let analytics: typeof import("./analytics").analytics;
let initAnalytics: typeof import("./analytics").initAnalytics;

beforeEach(async () => {
  // Fresh module state (the tracker / queue / initialized flag are module-level).
  vi.resetModules();
  plausibleFactory.mockReset().mockReturnValue({ trackEvent, trackPageview });
  trackEvent.mockReset();
  trackPageview.mockReset();
  ({ analytics, initAnalytics } = await import("./analytics"));
});

describe("analytics init ordering", () => {
  it("queues events fired before init and flushes them once initialized", () => {
    // Emitted before the tracker exists — as the /mcp page's onMount does,
    // before the client `init` hook has created the tracker.
    analytics.event("start-mcp-authorize");
    analytics.pageView();

    // Nothing sent yet: the tracker isn't built until init.
    expect(plausibleFactory).not.toHaveBeenCalled();
    expect(trackEvent).not.toHaveBeenCalled();
    expect(trackPageview).not.toHaveBeenCalled();

    initAnalytics(ENABLED_CONFIG);

    // The queued hits are flushed, in order, to the freshly-built tracker.
    expect(plausibleFactory).toHaveBeenCalledTimes(1);
    expect(trackEvent).toHaveBeenCalledWith("start-mcp-authorize", {
      props: undefined,
    });
    expect(trackPageview).toHaveBeenCalledTimes(1);
  });

  it("sends events directly once initialized", () => {
    initAnalytics(ENABLED_CONFIG);
    analytics.event("mcp-authorize--confirmed");

    expect(trackEvent).toHaveBeenCalledWith("mcp-authorize--confirmed", {
      props: undefined,
    });
  });

  it("drops pre-init events when analytics is disabled (no unbounded queue)", () => {
    analytics.event("start-mcp-authorize");

    // Init with no config = analytics disabled: no tracker is built, so the
    // queued event is discarded rather than sent or retained.
    initAnalytics(undefined);

    expect(plausibleFactory).not.toHaveBeenCalled();
    expect(trackEvent).not.toHaveBeenCalled();

    // Subsequent events are also dropped, not queued.
    analytics.event("mcp-authorize--confirmed");
    expect(trackEvent).not.toHaveBeenCalled();
  });
});
