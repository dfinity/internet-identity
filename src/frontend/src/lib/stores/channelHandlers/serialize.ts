import { promiseQueue } from "$lib/utils/promiseQueue";
import { derived, type Readable, writable } from "svelte/store";

/**
 * Runs authorization-bearing requests one at a time.
 *
 * Several handlers drive the same authorization state — the effective origin, the auth
 * flow, the authorized account — so a dapp sending requests in parallel could otherwise
 * race them against each other and have the user approve a screen naming one origin
 * while another is answered.
 */
export const serializeAuthorizationRequest = promiseQueue();

const signInsWaiting = writable(0);

/**
 * Queues a request that signs the user in to the app, counted while it waits.
 *
 * A request queued ahead of a sign-in can read the count to hand over its turn: the
 * sign-in is what carries the app's requested session duration and what registers this
 * browser, so a request that needs either must not hold the queue in front of it.
 */
export const serializeSignInRequest = <T>(
  run: () => Promise<T>,
): Promise<T> => {
  signInsWaiting.update((count) => count + 1);
  return serializeAuthorizationRequest(() => {
    signInsWaiting.update((count) => count - 1);
    return run();
  });
};

/** Whether a sign-in request is waiting for its turn in the queue. */
export const signInWaitingStore: Readable<boolean> = derived(
  signInsWaiting,
  (count) => count > 0,
);
