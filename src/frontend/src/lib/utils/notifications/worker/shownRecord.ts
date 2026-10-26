/** `getNotifications()` answered an empty array on iOS 16.4, only began reporting in 17.0, and `close()` was broken until 18.3, so a stamp of our own is kept beside it.
 * The stamp says only that this worker showed something recently, never that it is still on screen. */
import { createStore, get as idbGet, set as idbSet } from "idb-keyval";

const SHOWN_STORE = createStore("ii-notification-shown", "shown");
const SHOWN_KEY = "last";

/** A notification and the wake-ups around it arrive within seconds of each other; longer than this and a wake-up minutes later would stay silent when it owes the user something. */
const RECENT_MS = 30_000;

/** Records that something was shown, which is all the screen state we can keep. */
export const recordShown = async (): Promise<void> => {
  try {
    await idbSet(SHOWN_KEY, Date.now(), SHOWN_STORE);
  } catch {
    // Without the stamp a wake-up falls back to showing the placeholder.
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
