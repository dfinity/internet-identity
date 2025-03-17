// Utility functions to track window visibility state changes

// Array to store all visibility change listeners
const visibilityChangeListeners: (() => void)[] = [];

/**
 * Adds an event listener to track when the window visibility state changes.
 * @param params - An object containing the functions to call when the window visibility state changes.
 * @param params.onLeaveSession - The function to call when the window is hidden.
 * @param params.onEnterSession - The function to call when the window becomes visible.
 * @returns A function that removes this specific listener when called
 */
export function trackWindowSession({
  onLeaveSession,
  onEnterSession,
}: {
  onLeaveSession: () => void;
  onEnterSession: () => void;
}): () => void {
  const listener = () => {
    if (document.visibilityState === "hidden") {
      onLeaveSession();
    } else if (document.visibilityState === "visible") {
      onEnterSession();
    }
  };

  // Add the listener to the array
  visibilityChangeListeners.push(listener);
  document.addEventListener("visibilitychange", listener);

  // Return cleanup function
  return () => {
    // Use indexOf to find and remove the listener
    const index = visibilityChangeListeners.indexOf(listener);
    if (index !== -1) {
      visibilityChangeListeners.splice(index, 1);
    }
    document.removeEventListener("visibilitychange", listener);
  };
}

/**
 * Removes all event listeners that track window visibility state changes.
 */
export function cleanUpWindowSessionTrackers(): void {
  visibilityChangeListeners.forEach((listener) => {
    document.removeEventListener("visibilitychange", listener);
  });
  visibilityChangeListeners.length = 0;
}
