/** iOS gives a standalone web app an in-app browser view, with storage of its own, for a window opened on another origin, so the user would land signed out.
 * A scheme passed to `clients.openWindow` is not honoured, so the app opens a page of its own, in scope, and that page hands the link to Safari. */
import { readAppKey } from "../notificationAppKey";
import { NOTIFICATION_APP_PATH } from "../appPath";

/** The page that hands the link over, inside the app's scope so it opens in the app. */
const DOORWAY = `${NOTIFICATION_APP_PATH}/open`;

/**
 * Whether this worker belongs to the installed iOS app, which is the one place the
 * doorway exists for: iOS gives a standalone app an in-app browser view for another
 * origin, and nowhere else does.
 *
 * Both halves come from the record the app's own page wrote, because neither can be read
 * here. A desktop browser shares a storage partition with an installed app of the same
 * origin, so the marker alone would make a tab read as the app; and the user agent alone
 * calls an iPad in desktop mode a Mac, so it would leave that app out.
 *
 * Asked while a notification is being shown rather than when one is clicked, because a
 * click cannot wait for a promise without spending its permission to open a window.
 */
export const isTheApp = async (): Promise<boolean> => {
  const stored = await readAppKey();
  switch (stored.kind) {
    case "key":
      return stored.record.installed === true && stored.record.ios === true;
    case "no-key":
      return false;
    case "unreadable":
      return false;
  }
};

/**
 * The doorway's own address for `url`, for the installed app alone: it has one window
 * and cannot open another origin without stranding the user in an in-app view, so it is
 * pointed at a page of its own that hands the link to Safari.
 *
 * `undefined` where there is nothing to hand over, which a browser opens directly.
 */
export const doorwayFor = (
  url: string,
  shown?: { title: string; body: string },
): string | undefined => {
  if (!url.startsWith("https:")) {
    return undefined;
  }
  const carried = new URLSearchParams({ url });
  if (shown !== undefined) {
    carried.set("title", shown.title);
    carried.set("body", shown.body);
  }
  return `${DOORWAY}#${carried.toString()}`;
};
