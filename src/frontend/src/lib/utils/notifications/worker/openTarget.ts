/** iOS gives a standalone web app an in-app browser view, with storage of its own, for a window opened on another origin, so the user would land signed out.
 * A scheme passed to `clients.openWindow` is not honoured, so the app opens a page of its own, in scope, and that page hands the link to Safari. */
import { readAppKey } from "../notificationAppKey";
import { NOTIFICATION_APP_PATH } from "../appPath";

/** The page that hands the link over, inside the app's scope so it opens in the app. */
const DOORWAY = `${NOTIFICATION_APP_PATH}/open`;

/** Only the installed app says so, and it says so from its own page: a desktop browser
 * holding an app key is still a browser, and handing it the doorway leaves it opening a
 * page whose whole job is to pass the link to Safari. Storage that will not answer
 * confirms nothing, so the link itself is used. */
const isTheApp = async (): Promise<boolean> => {
  const stored = await readAppKey();
  switch (stored.kind) {
    case "key":
      return stored.record.installed === true;
    case "no-key":
      return false;
    case "unreadable":
      return false;
  }
};

/** What the doorway shows and where it sends them. The fragment never leaves the device, so the notification's own words travel in it. */
export const openable = async (
  url: string,
  shown?: { title: string; body: string },
): Promise<string> => {
  if (!url.startsWith("https:") || !(await isTheApp())) {
    return url;
  }
  const carried = new URLSearchParams({ url });
  if (shown !== undefined) {
    carried.set("title", shown.title);
    carried.set("body", shown.body);
  }
  return `${DOORWAY}#${carried.toString()}`;
};
