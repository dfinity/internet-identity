import type { VisitedApp } from "$lib/stores/last-used-identities.store";

export interface App {
  /** The origin the app's identity is derived for, as sign-in recorded it. Its
   *  metadata and its notification consent are keyed by the same spelling. */
  origin: string;
  /** Where the user last signed in to it from, which is what they know it by and
   *  where it opens. */
  displayOrigin: string;
  lastVisitedMillis: number;
}

/** A plain web origin, which is all a link to the app may point to. */
const isWebOrigin = (value: string): boolean => {
  try {
    const url = new URL(value);
    return (
      (url.protocol === "https:" || url.protocol === "http:") &&
      url.origin === value
    );
  } catch {
    return false;
  }
};

/**
 * The apps an identity has signed in to from this browser, most recent first.
 *
 * Read from this browser's own record because the canister keeps no list of an
 * identity's apps, so another browser lists the apps it signed in to itself.
 */
export const appsFrom = (
  visited: { [origin: string]: VisitedApp } | undefined,
): App[] =>
  Object.entries(visited ?? {})
    .map(([origin, { displayOrigin, lastVisitedMillis }]) => ({
      origin,
      displayOrigin: isWebOrigin(displayOrigin) ? displayOrigin : origin,
      lastVisitedMillis,
    }))
    .sort((a, b) => {
      const byTime = b.lastVisitedMillis - a.lastVisitedMillis;
      return byTime !== 0 ? byTime : a.origin.localeCompare(b.origin);
    });
