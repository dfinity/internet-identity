// Utility functions to track window close events

let windowCloseListener: (() => void) | null = null;

/**
 * Adds an event listener to track when the window is closed.
 * @param callback - The function to call when the window is closed.
 */
export function trackWindowClose(callback: () => void): void {
  if (windowCloseListener) {
    window.removeEventListener("beforeunload", windowCloseListener);
  }
  windowCloseListener = callback;
  window.addEventListener("beforeunload", windowCloseListener);
}

/**
 * Removes the event listener that tracks when the window is closed.
 */
export function removeWindowCloseTracker(): void {
  if (windowCloseListener) {
    window.removeEventListener("beforeunload", windowCloseListener);
    windowCloseListener = null;
  }
}
