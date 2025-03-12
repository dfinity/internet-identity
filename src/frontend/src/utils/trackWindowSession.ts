// Utility functions to track window visibility state changes

let visibilityChangeListener: (() => void) | null = null;

/**
 * Adds an event listener to track when the window visibility state changes.
 * @param params - An object containing the functions to call when the window visibility state changes.
 * @param params.onLeaveSession - The function to call when the window is hidden.
 * @param params.onEnterSession - The function to call when the window becomes visible.
 */
export function trackWindowSession({ onLeaveSession, onEnterSession }: { onLeaveSession: () => void, onEnterSession: () => void }): void {
  if (visibilityChangeListener) {
    document.removeEventListener("visibilitychange", visibilityChangeListener);
  }
  visibilityChangeListener = () => {
    if (document.visibilityState === "hidden") {
      onLeaveSession();
    } else if (document.visibilityState === "visible") {
      onEnterSession();
    }
  };
  document.addEventListener("visibilitychange", visibilityChangeListener);
}

/**
 * Removes the event listener that tracks window visibility state changes.
 */
export function removeWindowSessionTracker(): void {
  if (visibilityChangeListener) {
    document.removeEventListener("visibilitychange", visibilityChangeListener);
    visibilityChangeListener = null;
  }
}
