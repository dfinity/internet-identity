/**
 * Polling helpers for the canister's on-demand SSO discovery and JWKS caches.
 *
 * An update call reports `Pending` (and `discover_sso` returns no value) while
 * a cold cache fills; the frontend retries until it warms. A configured
 * provider (Google / Microsoft / Apple) is always warm, so callers resolve on
 * the first attempt.
 */
export const POLL_INTERVAL_MS = 500;
export const MAX_POLL_ATTEMPTS = 60;

/**
 * Sleep one poll interval. Resolves early when `signal` aborts so the caller's
 * next abort check runs without waiting out the full delay (and, in turn,
 * without firing another canister call for an already-cancelled lookup).
 */
export const pollDelay = (signal?: AbortSignal): Promise<void> =>
  new Promise((resolve) => {
    if (signal?.aborted === true) {
      resolve();
      return;
    }
    const onAbort = (): void => {
      clearTimeout(timer);
      resolve();
    };
    const timer = setTimeout(() => {
      signal?.removeEventListener("abort", onAbort);
      resolve();
    }, POLL_INTERVAL_MS);
    signal?.addEventListener("abort", onAbort, { once: true });
  });

/**
 * Retry an update call that reports the top-level `Pending` arm while SSO
 * discovery / JWKS load, returning the first settled (`Ok` / `Err`) response.
 * Throws if the cache hasn't warmed within the attempt budget.
 */
export const retryWhilePending = async <T, E>(
  call: () => Promise<{ Ok: T } | { Pending: null } | { Err: E }>,
): Promise<{ Ok: T } | { Err: E }> => {
  for (let attempt = 0; attempt < MAX_POLL_ATTEMPTS; attempt++) {
    const response = await call();
    if ("Pending" in response) {
      await pollDelay();
      continue;
    }
    return response;
  }
  throw new Error("Timed out waiting for SSO discovery to complete");
};
