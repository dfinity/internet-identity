/**
 * Watches for the Home Screen app registering, so the tab that showed the install steps
 * can say the thing is done.
 *
 * The app runs in its own storage partition and nothing in this tab can see it, so the
 * canister is the only party that knows. It is asked rather than told: there is no
 * channel between the two documents.
 */
import { browserAppStatus } from "./appStatus";

/** Slow enough to be nothing on a query, often enough that the tab answers while the
 *  user is still looking at the app they just opened. */
const POLL_MS = 3000;

/** Returns the function that stops watching, which also runs before the callback. */
export const watchForInstall = (
  identityNumber: bigint,
  onDelivering: () => void,
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
    // Asked again on the way out: the caller may have stopped watching while the query
    // was in flight, and what the callback does cannot be undone.
    if (stopped) {
      return;
    }
    if (status?.subscribed === true) {
      stop();
      onDelivering();
      return;
    }
    timer = setTimeout(() => void poll(), POLL_MS);
  };

  timer = setTimeout(() => void poll(), POLL_MS);
  return stop;
};
