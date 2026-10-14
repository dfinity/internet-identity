/**
 * What a tapped notification opens, which is not always the app's own link.
 *
 * iOS gives a standalone web app an in-app browser view for a window opened on another
 * origin, with no API to ask for Safari instead. A standalone web app that navigates
 * itself out of its own scope is handed to Safari, so the Home Screen app opens a page
 * of its own and lets that page navigate.
 *
 * Only the installed app takes that road: a browser's own worker opens the link
 * directly, where a window is a tab and a doorway page would only be a redirect the
 * user can see.
 */
import { readAppKey } from "../notificationAppKey";

/** The page that does the navigating, in the app's scope so it opens inside it. */
const DOORWAY = "/notifications/open";

/**
 * Whether this worker is the Home Screen app's.
 *
 * The registration is shared with the browser that installed it, but storage is not:
 * an app key exists only in the app's own partition.
 */
const isTheApp = async (): Promise<boolean> => {
  try {
    return (await readAppKey()) !== undefined;
  } catch {
    return false;
  }
};

export const openable = async (url: string): Promise<string> =>
  (await isTheApp()) ? `${DOORWAY}#${encodeURIComponent(url)}` : url;
