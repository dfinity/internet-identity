/**
 * Requests that still owe the flow a screen.
 *
 * A consent request is accepted well before it knows what to put on screen: it has an
 * origin to validate, an identity to wait for, and state to read first. Authorizing is
 * what would otherwise take the screen the user is looking at away from them, so a
 * request claims the screen while it works out what it needs, and the flow keeps what
 * it already has rather than cutting to the redirect animation and back.
 *
 * Counted rather than flagged: an app may have several requests in flight, and the
 * screen is owed until the last of them has answered for itself.
 */
import { derived, type Readable, writable } from "svelte/store";

const claims = writable(0);

/** Claims the screen until the returned function is called. */
export const claimScreen = (): (() => void) => {
  claims.update((count) => count + 1);
  let released = false;
  return () => {
    if (released) {
      return;
    }
    released = true;
    claims.update((count) => count - 1);
  };
};

/** Whether any request is still working out what to show. */
export const pendingScreenStore: Readable<boolean> = derived(
  claims,
  (count) => count > 0,
);
