import type { AnalyticsConfig } from "$lib/generated/internet_identity_types";
import { establishedChannelStore } from "$lib/stores/channelStore";
import Plausible from "plausible-tracker";
import { PlausibleInitOptions } from "plausible-tracker/build/main/lib/tracker";
import { get } from "svelte/store";

let tracker: undefined | ReturnType<typeof Plausible>;

// Whether `initAnalytics` has run yet — regardless of its outcome. Lets us tell
// "analytics not initialized yet" (queue events) apart from "initialized but
// disabled" (no tracker configured → drop events), so a deployment with
// analytics off doesn't accumulate a queue forever.
let initialized = false;

// Events emitted before the tracker exists are queued here and flushed on init.
// This matters because Svelte runs a *child* component's `onMount` before its
// parent's: a page's `onMount` (e.g. the `/mcp` funnel firing
// `start-mcp-authorize` / `mcp-authorize--request-received`) runs before the
// root layout — or the client `init` hook — has created the tracker. Without
// this queue those early events hit `tracker === undefined` and were silently
// dropped, so the top of the connect funnel never reached Plausible. Bounded so
// that if init never brings up a tracker (analytics disabled, or a stuck init)
// the queue can't grow without limit.
type QueuedHit =
  | { kind: "pageView" }
  | {
      kind: "event";
      name: string;
      props?: Record<string, string | number | boolean>;
    };
const MAX_QUEUED_HITS = 50;
let queuedHits: QueuedHit[] = [];

const convertToPlausibleConfig = (
  config: AnalyticsConfig | undefined,
): PlausibleInitOptions | undefined => {
  if (config === undefined) {
    return;
  }
  if ("Plausible" in config) {
    const plausibleConfig = config.Plausible;
    // `undefined` values in Plausible config are not the same as missing values.
    return removeUndefinedFields({
      hashMode: plausibleConfig.hash_mode[0],
      domain: plausibleConfig.domain[0],
      trackLocalhost: plausibleConfig.track_localhost[0],
      apiHost: plausibleConfig.api_host[0],
    });
  }
};

const removeUndefinedFields = (obj: Record<string, unknown | undefined>) => {
  return Object.fromEntries(
    Object.entries(obj).filter(([_, value]) => value !== undefined),
  );
};

export const initAnalytics = (config: AnalyticsConfig | undefined) => {
  const plausibleConfig =
    config === undefined ? undefined : convertToPlausibleConfig(config);
  if (plausibleConfig !== undefined) {
    tracker = Plausible(plausibleConfig);
  }
  initialized = true;
  // Flush anything emitted before the tracker existed. If analytics is disabled
  // (no tracker was configured) the queue is simply discarded.
  const pending = queuedHits;
  queuedHits = [];
  if (tracker !== undefined) {
    for (const hit of pending) {
      if (hit.kind === "pageView") {
        tracker.trackPageview();
      } else {
        tracker.trackEvent(hit.name, { props: hit.props });
      }
    }
  }
};

// The `authorizationOrigin` property is resolved at emit time (when it is
// meaningful), so it is captured here and carried through the queue if the
// event has to wait for init.
const withAuthorizationOrigin = (
  props?: Record<string, string | number | boolean>,
): Record<string, string | number | boolean> | undefined => {
  let authorizationOrigin: string | undefined;
  try {
    authorizationOrigin = get(establishedChannelStore).origin;
  } catch {
    // Context not available yet
  }
  return authorizationOrigin !== undefined
    ? { ...props, authorizationOrigin }
    : props;
};

// Queue a hit only while analytics is still coming up. Once initialized without
// a tracker (analytics disabled), hits are dropped rather than accumulated.
const enqueue = (hit: QueuedHit): void => {
  if (!initialized && queuedHits.length < MAX_QUEUED_HITS) {
    queuedHits.push(hit);
  }
};

export const analytics = {
  pageView: () => {
    if (tracker === undefined) {
      enqueue({ kind: "pageView" });
      return;
    }
    tracker.trackPageview();
  },
  event: (name: string, props?: Record<string, string | number | boolean>) => {
    const eventProps = withAuthorizationOrigin(props);
    if (tracker === undefined) {
      enqueue({ kind: "event", name, props: eventProps });
      return;
    }
    tracker.trackEvent(name, { props: eventProps });
  },
};
