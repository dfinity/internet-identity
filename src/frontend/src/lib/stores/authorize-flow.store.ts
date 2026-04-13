import { writable } from "svelte/store";

/**
 * Whether the authorize flow has completed all pending requests.
 *
 * - `false` — Requests are being handled or might still arrive. Layout renders children.
 * - `true` — No more requests expected. Layout shows the "you may close this page" screen.
 */
export const authorizeFlowComplete = writable<boolean>(false);

const IDLE_TIMEOUT_MS = 500;
let idleTimeoutId: ReturnType<typeof setTimeout> | undefined;

/**
 * Start the idle countdown. When it fires, `authorizeFlowComplete` becomes `true`.
 * Call after sending a response on the channel — if no new request arrives
 * within the timeout window, the flow is considered complete.
 */
export const startIdleTimeout = () => {
  clearTimeout(idleTimeoutId);
  idleTimeoutId = setTimeout(() => {
    authorizeFlowComplete.set(true);
  }, IDLE_TIMEOUT_MS);
};

/**
 * Cancel the idle countdown and mark the flow as active.
 * Called by the channel request listener when a new request arrives.
 */
export const resetIdleTimeout = () => {
  clearTimeout(idleTimeoutId);
  authorizeFlowComplete.set(false);
};
