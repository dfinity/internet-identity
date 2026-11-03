import { POLL_INTERVAL_MS } from "$lib/constants/polling.constants";
import { browserAppStatus } from "$lib/utils/notifications/appStatus";
/** `onAnswered` where the app has moved its own stamp since the probe, `onGone` where
 *  the record itself is gone. Both are read from this browser's own entry, so an app on
 *  another device can never stand in for this one. */
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
        // `last_seen_ns` moves on the app's own update calls and on nothing else, so it
        // is the whole of the answer. An empty `pending_probe` says only that the ask
        // has had its window, which it gets whether or not anybody called.
        if (status.app.last_seen_ns > since) {
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
