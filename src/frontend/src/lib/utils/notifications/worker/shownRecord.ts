/**
 * What this worker last put on screen, kept by us rather than asked of the browser.
 *
 * `ServiceWorkerRegistration.getNotifications()` is how a worker finds what it has
 * shown, and it is what this prefers. It is not enough on its own: it answered an empty
 * array on iOS 16.4 and only began reporting notifications in 17.0, and `close()` was
 * broken there until 18.3, so on an older phone a wake-up asking the browser what is on
 * screen is told nothing and adds a placeholder beside what is already there.
 *
 * A stamp of our own is the fallback. It says only that this worker showed something
 * recently, never that it is still on screen, so it is asked beside the API rather than
 * instead of it.
 */
import { createStore, get as idbGet, set as idbSet } from "idb-keyval";

const SHOWN_STORE = createStore("ii-notification-shown", "shown");
const SHOWN_KEY = "last";

/**
 * How recently a notification must have been shown for a wake-up to take the screen as
 * already served.
 *
 * A notification and the wake-ups around it arrive within seconds of each other: the
 * mint earns one, and the canister queues one per notification. Longer than that and a
 * wake-up minutes later would stay silent when it owes the user something.
 */
const RECENT_MS = 30_000;

/** Records that something was shown, which is all the screen state we can keep. */
export const recordShown = async (): Promise<void> => {
  try {
    await idbSet(SHOWN_KEY, Date.now(), SHOWN_STORE);
  } catch {
    // Without the stamp a wake-up falls back to showing the placeholder, which is the
    // behaviour this replaces rather than something worse.
  }
};

/** Whether this worker put something on screen within the last {@link RECENT_MS}. */
export const shownRecently = async (
  nowMillis: number = Date.now(),
): Promise<boolean> => {
  try {
    const last = await idbGet<number>(SHOWN_KEY, SHOWN_STORE);
    return last !== undefined && nowMillis - last < RECENT_MS;
  } catch {
    return false;
  }
};
