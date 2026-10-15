/**
 * What a tapped notification opens, which from the Home Screen app is not the link.
 *
 * iOS gives a standalone web app an in-app browser view for a window opened on another
 * origin, and that view carries its own storage, so the user lands at the app signed
 * out. Handing the link to Safari is possible, but only from a page: a scheme passed to
 * `clients.openWindow` is not honoured and opens the app itself, losing the link. So the
 * app opens a page of its own, in scope, and that page does the handing over.
 *
 * A browser's own worker opens the link directly: a window is a tab there, already
 * carrying the user's session.
 */
import { readAppKey } from "../notificationAppKey";

/** The page that hands the link over, inside the app's scope so it opens in the app. */
const DOORWAY = "/ios-icp-alerts/open";

/**
 * Whether this worker is the Home Screen app's.
 *
 * The registration is shared with the browser that installed it, but storage is not: an
 * app key exists only in the app's own partition.
 */
const isTheApp = async (): Promise<boolean> => {
  try {
    return (await readAppKey()) !== undefined;
  } catch {
    return false;
  }
};

export const openable = async (url: string): Promise<string> =>
  url.startsWith("https:") && (await isTheApp())
    ? `${DOORWAY}#${encodeURIComponent(url)}`
    : url;
