import type { LastUsedAccounts } from "$lib/stores/last-used-identities.store";

export interface App {
  /** The origin the app's identity is derived for, as sign-in recorded it. Its
   *  metadata and its notification consent are keyed by the same spelling. */
  origin: string;
  /** The latest sign-in to any of its accounts, or `undefined` when none carries a
   *  time: an account synced from the canister reads 0 when it was never used. */
  lastUsedMillis: number | undefined;
}

/**
 * The apps an identity has signed in to from this browser, most recent first.
 *
 * Read from this browser's own record because the canister keeps no list of an
 * identity's apps, so another browser lists the apps it signed in to itself.
 */
export const appsFrom = (accounts: LastUsedAccounts | undefined): App[] =>
  Object.entries(accounts ?? {})
    // A sync that came back with no accounts leaves an empty entry behind.
    .filter(([, byAccount]) => Object.keys(byAccount).length > 0)
    .map(([origin, byAccount]) => {
      const latest = Math.max(
        ...Object.values(byAccount).map(
          (account) => account.lastUsedTimestampMillis,
        ),
      );
      return { origin, lastUsedMillis: latest > 0 ? latest : undefined };
    })
    .sort((a, b) => {
      const byTime = (b.lastUsedMillis ?? 0) - (a.lastUsedMillis ?? 0);
      return byTime !== 0 ? byTime : a.origin.localeCompare(b.origin);
    });
