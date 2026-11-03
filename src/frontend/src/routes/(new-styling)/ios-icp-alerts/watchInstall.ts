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
