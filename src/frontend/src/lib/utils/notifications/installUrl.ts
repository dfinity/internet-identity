import { encodeLinkToken } from "./linkToken";
import { startInstall } from "./startInstall";
import { NOTIFICATION_APP_PATH } from "./appPath";
import { isStandalone } from "./standalone";
import { openInSafari } from "./openInSafari";
import {
  installSourceParam,
  type NotificationsSource,
} from "$lib/utils/analytics/icpAlertsFunnel";

/** `undefined` where this browser is not registered; rejects as {@link startInstall} does. */
export const prepareInstallUrl = async ({
  identityNumber,
  source,
}: {
  identityNumber: bigint;
  /** Where the install was offered, carried to the Home Screen app for analytics; `undefined` where a URL that never carried one is minted again. */
  source: NotificationsSource | undefined;
}): Promise<string | undefined> => {
  const linkKey = await startInstall(identityNumber);
  if (linkKey === undefined) {
    return undefined;
  }
  const token = encodeLinkToken({ identityNumber, linkKey });
  return source === undefined
    ? `${NOTIFICATION_APP_PATH}#${token}`
    : `${NOTIFICATION_APP_PATH}#${token}&${installSourceParam(source)}`;
};

/** The placeholder the new tab holds while the URL is still being minted: this page, told to wait rather than to decide. */
export const INSTALL_PLACEHOLDER_URL = `${NOTIFICATION_APP_PATH}?installing`;

/** Marks an install handed to Safari by another browser on this device, whose own entry is the one the install is for. */
export const MINTED_ELSEWHERE_PARAM = "minted-elsewhere";

/** `undefined` where this browser has nothing to send the user to; the reason is logged rather than raised.
 *  A record that still answers refuses an install, which is what keeps a second app off a device that already has one.
 *  That refusal is not worked around here: the caller that means to replace an app probes it first, and the screen says so where it is still there. */
export const installUrlOrNone = async (
  identityNumber: bigint,
  source: NotificationsSource,
): Promise<string | undefined> => {
  try {
    return await prepareInstallUrl({ identityNumber, source });
  } catch (error) {
    console.error(error);
    return undefined;
  }
};

/** Must be called straight from the click, which is the only time a browser allows it; the tab is opened on the placeholder and pointed once the URL resolves.
 *  A blocked tab is not an error: the screen shows the link instead.
 *  Inside a Home Screen web app, II runs in an in-app view that has no tabs and reports itself standalone, which the install page would take for the installed app, so the steps are handed to Safari instead. */
export const openInstallTab = (
  url: string | Promise<string | undefined>,
): boolean => {
  if (isStandalone()) {
    void Promise.resolve(url).then(
      (resolved) => {
        if (resolved === undefined) {
          return;
        }
        const handedOver = new URL(resolved, window.location.origin);
        handedOver.searchParams.set(MINTED_ELSEWHERE_PARAM, "");
        openInSafari(handedOver.toString());
      },
      (error: unknown) => console.error(error),
    );
    return true;
  }
  const opened = window.open(INSTALL_PLACEHOLDER_URL, "_blank");
  if (opened === null) {
    // The caller handed its only failure path to this promise, so a tab the browser
    // refused is no reason to leave that failure unhandled.
    if (typeof url !== "string") {
      void url.catch((error: unknown) => console.error(error));
    }
    return false;
  }
  if (typeof url === "string") {
    opened.location.href = url;
    return true;
  }
  void url.then(
    (resolved) => {
      if (resolved === undefined) {
        // Nothing to send them to, and an empty tab says less than none.
        opened.close();
        return;
      }
      opened.location.href = resolved;
    },
    (error: unknown) => {
      // A rejection left here would be an empty tab nothing ever closes.
      console.error(error);
      opened.close();
    },
  );
  return true;
};

/** A tab opened inside the press, waiting for somewhere to go. */
export interface PendingInstallTab {
  /** Whether the browser allowed the tab; a refused one is not a failure, the screen behind it carries the link. */
  opened: boolean;
  /** Points it at the install, or closes it where there is nothing to point it at. */
  point: (url: string | undefined) => void;
}

/** Must be called straight from the click: the tab has to open before anything is awaited. */
export const openPendingInstallTab = (): PendingInstallTab => {
  let point: (url: string | undefined) => void = () => {};
  const opened = openInstallTab(
    new Promise<string | undefined>((resolve) => {
      point = resolve;
    }),
  );
  return { opened, point };
};
