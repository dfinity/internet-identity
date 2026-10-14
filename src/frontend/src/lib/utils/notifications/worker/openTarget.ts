/**
 * Opening the link a tapped notification carries.
 *
 * A standalone web app on iOS gets an in-app browser view for any window opened on
 * another origin, and a worker has no API to ask for Safari. `x-safari-https:` is a
 * private scheme that hands a URL to Safari, reported to still work in several in-app
 * browsers in 2026 and to have stopped working in others, and not documented by Apple at
 * all. So it is tried and not relied on: an engine that refuses the scheme, or an iOS
 * that has closed it, leaves the plain link opening exactly as it does today.
 *
 * Only the installed app tries it. A browser's own worker opens a tab, where the scheme
 * would at best do the same thing and at worst be refused.
 */
import { readAppKey } from "../notificationAppKey";

/** The scheme that asks iOS to hand a link to Safari. */
const toSafariScheme = (url: string): string =>
  url.replace(/^https:/, "x-safari-https:");

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

/**
 * Opens `url`, in Safari where iOS allows it.
 *
 * Answers nothing: a tap has no caller to report to, and both roads end with the link
 * open or with nothing this worker can do about it.
 */
export const openLink = async (
  clients: Clients,
  url: string,
): Promise<void> => {
  if (url.startsWith("https:") && (await isTheApp())) {
    const escaped = await clients
      .openWindow(toSafariScheme(url))
      .then(() => true)
      .catch(() => false);
    if (escaped) {
      return;
    }
  }
  await clients.openWindow(url).catch(() => undefined);
};
