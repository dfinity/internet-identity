import { POLL_INTERVAL_MS } from "$lib/constants/polling.constants";
import { browserAppStatus } from "$lib/utils/notifications/appStatus";

/** Returns the function that stops watching, which also runs before the callback.
 *
 * `replacedLastSeen` is how far the record the user is replacing had got, read by the
 * caller before it started the new install. A record outlives the app it points at, so
 * without it a deleted app that is still marked delivering reads as the new one. It is
 * read by the caller rather than taken from the first poll here: a tab Safari suspends
 * can come back to find the replacement already answering, and a baseline taken then is
 * the new app's own, which nothing can ever beat.
 */
export const watchForInstall = (
  identityNumber: bigint,
  onDelivering: () => void,
  { replacedLastSeen }: { replacedLastSeen?: bigint } = {},
): (() => void) => {
  let stopped = false;
  let timer: ReturnType<typeof setTimeout>;

  const stop = () => {
    stopped = true;
    clearTimeout(timer);
  };

  const poll = async () => {
    if (stopped) {
      return;
    }
    const status = await browserAppStatus(identityNumber);
    // Asked again on the way out: the caller may have stopped watching while the query was in flight.
    if (stopped) {
      return;
    }
    if (status.kind === "app") {
      // Subscribed is not delivering: a reference the identity turned off is skipped when the canister picks who to send to.
      const isTheNewApp =
        replacedLastSeen === undefined ||
        status.app.last_seen_ns > replacedLastSeen;
      if (isTheNewApp && status.app.subscribed && status.app.enabled) {
        stop();
        onDelivering();
        return;
      }
    }
    timer = setTimeout(() => void poll(), POLL_INTERVAL_MS);
  };

  timer = setTimeout(() => void poll(), POLL_INTERVAL_MS);
  return stop;
};

/** `onAnswered` where the probe has left this browser's queue, `onGone` where the record itself is gone.
 *  Both are read from this browser's own entry, so an app on another device can never stand in for this one. */
export const watchProbe = (
  identityNumber: bigint,
  {
    since,
    onAnswered,
    onGone,
  }: {
    /** What the app's stamp read before the probe, so only the app moving it counts. */
    since: bigint;
    onAnswered: () => void;
    onGone: () => void;
  },
): (() => void) => {
  let stopped = false;
  let timer: ReturnType<typeof setTimeout>;

  const stop = () => {
    stopped = true;
    clearTimeout(timer);
  };

  const poll = async () => {
    if (stopped) {
      return;
    }
    const status = await browserAppStatus(identityNumber);
    if (stopped) {
      return;
    }
    switch (status.kind) {
      // An install started here points this browser away from the app the probe went
      // to, which is the same as having none.
      case "awaiting-install":
      case "no-app":
        stop();
        onGone();
        return;
      case "app":
        // An empty `pending_probe` says only that the entry left the queue, which a rate
        // limit, a relay error or a rejected push also does. `last_seen_ns` moves on the
        // app's own update calls, so that is the one thing only the app can have done.
        if (
          status.app.pending_probe.length === 0 &&
          status.app.last_seen_ns > since
        ) {
          stop();
          onAnswered();
          return;
        }
        break;
      case "unanswered":
        // Neither answer, and the wrong one either way: a user whose app is working would be told to install it again.
        break;
    }
    timer = setTimeout(() => void poll(), POLL_INTERVAL_MS);
  };

  timer = setTimeout(() => void poll(), POLL_INTERVAL_MS);
  return stop;
};
