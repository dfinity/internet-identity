/**
 * Runs authorization-bearing requests one at a time.
 *
 * Several handlers drive the same authorization state — the effective origin, the auth
 * flow, the authorized account — so a dapp sending requests in parallel could otherwise
 * race them against each other and have the user approve a screen naming one origin
 * while another is answered.
 */
let queueTail: Promise<unknown> = Promise.resolve();

export const serializeAuthorizationRequest = <T>(
  run: () => Promise<T>,
): Promise<T> => {
  const next = queueTail.then(run);
  queueTail = next.catch(() => {});
  return next;
};
