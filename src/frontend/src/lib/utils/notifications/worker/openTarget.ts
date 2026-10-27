/** iOS gives a standalone web app an in-app browser view, with storage of its own, for a window opened on another origin, so the user would land signed out.
 * A scheme passed to `clients.openWindow` is not honoured, so the app opens a page of its own, in scope, and that page hands the link to Safari. */
import { readAppKey } from "../notificationAppKey";
import { NOTIFICATION_APP_PATH } from "../appPath";

/** The page that hands the link over, inside the app's scope so it opens in the app. */
const DOORWAY = `${NOTIFICATION_APP_PATH}/open`;

/**
 * The doorway exists for one platform. iOS gives a standalone app an in-app browser
 * view for another origin, and nowhere else does, so nowhere else should be sent
 * through a page whose only job is to hand a link to Safari.
 *
 * Checked first, because what is stored cannot answer this on its own: a desktop
 * browser shares a storage partition with an installed app of the same origin, so a
 * tab reads as the app there. On iOS the app has a partition of its own, which is
 * what makes the stored answer meaningful at all.
 */
export const onIos = (): boolean =>
  /iPhone|iPod|iPad/.test(navigator.userAgent);

const isTheApp = async (): Promise<boolean> => {
  if (!onIos()) {
    return false;
  }
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

/**
 * Where a click should go, and which of the two jobs opening it is.
 *
 * `inTheApp` is the installed iOS app, which has one window and cannot open another
 * origin without stranding the user in an in-app view: it is pointed at the doorway,
 * a page of its own that hands the link to Safari. Anything else is a browser, which
 * opens the app's link and needs none of that.
 */
export const openable = async (
  url: string,
  shown?: { title: string; body: string },
): Promise<{ url: string; inTheApp: boolean }> => {
  if (!url.startsWith("https:") || !(await isTheApp())) {
    return { url, inTheApp: false };
  }
  const carried = new URLSearchParams({ url });
  if (shown !== undefined) {
    carried.set("title", shown.title);
    carried.set("body", shown.body);
  }
  return { url: `${DOORWAY}#${carried.toString()}`, inTheApp: true };
};
