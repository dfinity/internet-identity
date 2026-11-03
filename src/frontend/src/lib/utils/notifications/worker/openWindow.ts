/// <reference lib="webworker" />

import { onIos, openable } from "./openTarget";
import { NOTIFICATION_APP_PATH } from "../appPath";

/** The app's own window, which is the only one reused: a page on this origin that is not
 *  the app belongs to the user and is left where they put it. */
const appWindow = async (
  worker: ServiceWorkerGlobalScope,
): Promise<WindowClient | undefined> => {
  const windows = await worker.clients.matchAll({
    type: "window",
    includeUncontrolled: true,
  });
  return windows.find((client) => {
    try {
      return new URL(client.url).pathname.startsWith(NOTIFICATION_APP_PATH);
    } catch {
      return false;
    }
  });
};

/** iOS gives a standalone app one window and surfaces it for a URL in its own scope
 *  rather than taking it there, so the running window is pointed by hand. */
const pointAt = async (
  worker: ServiceWorkerGlobalScope,
  url: string,
  inTheApp: boolean,
): Promise<void> => {
  const window = inTheApp ? await appWindow(worker) : undefined;
  if (window === undefined) {
    await worker.clients.openWindow(url);
    return;
  }
  // Refused in some app views, where the page's own `hashchange` is what is left.
  await window.navigate(url).catch(() => undefined);
  await window.focus();
};

/**
 * Opens what a click names.
 *
 * A browser's permission to open a window belongs to the synchronous run of the click
 * handler: anything awaited before `openWindow` spends it. So the ordinary path calls it
 * first and asks questions afterwards. Only the installed iOS app has to be told where
 * to go before it can go there, and iOS is also the one platform that honours the call
 * after the fact.
 */
export const openOnClick = (
  worker: ServiceWorkerGlobalScope,
  url: string,
  shown: { title: string; body: string },
): Promise<void> => {
  if (!onIos()) {
    return worker.clients.openWindow(url).then(
      () => undefined,
      () => undefined,
    );
  }
  return openable(url, shown).then(({ url, inTheApp }) =>
    pointAt(worker, url, inTheApp),
  );
};

/** Where a click with nothing to open goes: the app this worker belongs to. */
export const openTheApp = async (
  worker: ServiceWorkerGlobalScope,
): Promise<void> => {
  const windows = await worker.clients.matchAll({
    type: "window",
    includeUncontrolled: true,
  });
  const open = windows.find((client) => client.url.startsWith(worker.origin));
  if (open !== undefined) {
    await open.focus();
    return;
  }
  await worker.clients.openWindow("/");
};
