/**
 * Watches for the Home Screen app claiming its entry, so the tab showing the install
 * steps can take itself away once they have been followed.
 *
 * The app runs in its own storage partition and nothing in this tab can see it, so the
 * canister is the only party that knows. It is asked rather than told: there is no
 * channel between the two documents.
 */
import { notificationAppDelivers } from "$lib/utils/notifications/notificationState";

/** Slow enough to be nothing on a query, often enough that the tab goes while the user
 *  is still looking at the app they just opened. */
const POLL_MS = 3000;

/** Returns the function that stops watching, which also runs before the callback. */
export const watchForInstall = (
  identityNumber: bigint,
  onInstalled: () => void,
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
    if (await notificationAppDelivers(identityNumber)) {
      stop();
      onInstalled();
      return;
    }
    if (!stopped) {
      timer = setTimeout(() => void poll(), POLL_MS);
    }
  };

  timer = setTimeout(() => void poll(), POLL_MS);
  return stop;
};
