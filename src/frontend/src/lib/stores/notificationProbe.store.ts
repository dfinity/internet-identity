/**
 * Sign-ins that are probing the way this device receives an app's notifications.
 *
 * A path past its limit is probed before the sign-in answers the app, and the probe
 * waits a few seconds for the device to fetch it. The authorize page shows that wait
 * rather than the redirect animation, so the pause has a reason on screen.
 *
 * Counted rather than flagged: an app may have several requests in flight, and the
 * view stays up until the last of them has finished probing.
 */
import { derived, type Readable, writable } from "svelte/store";

const numProbing = writable(0);

/** Shows the probing view until the returned function is called. */
export const showNotificationProbe = (): (() => void) => {
  numProbing.update((count) => count + 1);
  let isHidden = false;
  return () => {
    if (isHidden) {
      return;
    }
    isHidden = true;
    numProbing.update((count) => count - 1);
  };
};

/** Whether any sign-in is probing this device's notification path. */
export const notificationProbeStore: Readable<boolean> = derived(
  numProbing,
  (count) => count > 0,
);
