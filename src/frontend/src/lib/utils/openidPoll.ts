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

export const pollDelay = (): Promise<void> =>
  new Promise((resolve) => setTimeout(resolve, POLL_INTERVAL_MS));

/**
 * Retry an update call that returns a `{ Pending }` error variant while SSO
 * discovery / JWKS load, returning the first non-`Pending` response. Throws if
 * the cache hasn't warmed within the attempt budget.
 */
export const retryWhilePending = async <
  R extends { Ok: unknown } | { Err: Record<string, unknown> },
>(
  call: () => Promise<R>,
): Promise<R> => {
  for (let attempt = 0; attempt < MAX_POLL_ATTEMPTS; attempt++) {
    const response = await call();
    if ("Err" in response && "Pending" in response.Err) {
      await pollDelay();
      continue;
    }
    return response;
  }
  throw new Error("Timed out waiting for SSO discovery to complete");
};
