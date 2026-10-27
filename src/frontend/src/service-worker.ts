/// <reference types="@sveltejs/kit" />
/// <reference lib="webworker" />

// The Web Push worker. It handles pushes and nothing else: no `fetch` listener, so
// it never sits between the page and the network, and no precaching, so a stale
// worker cannot serve a stale app.
//
// A push carries no body, since the payload is not sealed to the browser's keys: what
// it is for is asked of the canister, and its content of the app that sent it. The
// subscription is `userVisibleOnly`, which obliges a visible notification per push.

import { reportOpened } from "$lib/utils/notifications/worker/notificationOpened";
import {
  onWakeUp,
  showPlaceholder,
} from "$lib/utils/notifications/worker/wakeUp";
import { initWorkerConfig } from "$lib/utils/notifications/workerConfig";
import { promiseQueue } from "$lib/utils/promiseQueue";
import { refOf } from "$lib/utils/notifications/worker/shownNotification";
import {
  openable,
  sameOriginWayTo,
} from "$lib/utils/notifications/worker/openTarget";
import { NOTIFICATION_APP_PATH } from "$lib/utils/notifications/appPath";

const worker = self as unknown as ServiceWorkerGlobalScope;

// What the page put on this worker's registration URL, read once. A woken worker has
// no page to ask and no document to read, and the registration keeps the URL it was
// made with, so this is the whole of what the worker is told.
const configured = initWorkerConfig(worker.location.search);

worker.addEventListener("install", () => {
  // Nothing is cached, so an older worker has nothing this one needs to inherit.
  void worker.skipWaiting();
});

worker.addEventListener("activate", (event) => {
  event.waitUntil(worker.clients.claim());
});

// One wake-up at a time, whatever the browser delivers. `browser_get_next_notification`
// is a query and claims nothing, so two pushes handled at once would both read the same
// head, show the same notification, and leave the one behind it with no wake-up coming.
// `waitUntil` extends this worker's life; it does not serialise its handlers.
const enqueue = promiseQueue();

/**
 * Diagnostic, on beta only: a worker's console is not reachable in every browser, so
 * what a click did is written where a page can read it back.
 *
 *   const c = await caches.open("ii-notification-trace");
 *   for (const k of await c.keys()) console.log(await (await c.match(k)).json());
 *   await caches.delete("ii-notification-trace");   // to clear
 *
 * Remove once the desktop behaviour is confirmed.
 */
const TRACE = "ii-notification-trace";

const trace = async (entry: Record<string, unknown>): Promise<void> => {
  try {
    const log = await caches.open(TRACE);
    await log.put(
      new Request(`https://trace.invalid/${Date.now()}-${Math.random()}`),
      new Response(
        JSON.stringify({ at: new Date().toISOString(), ...entry }, null, 2),
        { headers: { "content-type": "application/json" } },
      ),
    );
  } catch {
    // A trace must never be the reason a click fails.
  }
};

worker.addEventListener("push", (event) => {
  event.waitUntil(
    enqueue(async () => {
      void trace({
        step: "push",
        configured,
        scope: worker.registration.scope,
      });
      if (!configured) {
        // Nothing can be asked of anyone without knowing which canister to ask, and
        // the subscription still owes the user a notification.
        await showPlaceholder(worker.registration);
        void trace({ step: "placeholder-shown" });
        return;
      }
      await onWakeUp({ registration: worker.registration });
      const shown = await worker.registration.getNotifications();
      void trace({
        step: "woke",
        showing: shown.map((notification) => ({
          title: notification.title,
          hasData:
            notification.data !== undefined && notification.data !== null,
        })),
      });
    }),
  );
});

/** The app's own window, which is the only one reused: a page on this origin that is not
 *  the app belongs to the user and is left where they put it. */
const appWindow = async (): Promise<WindowClient | undefined> => {
  const clients = await worker.clients.matchAll({
    type: "window",
    includeUncontrolled: true,
  });
  return clients.find((client) => {
    try {
      return new URL(client.url).pathname.startsWith(NOTIFICATION_APP_PATH);
    } catch {
      return false;
    }
  });
};

/**
 * Takes a window to `url`.
 *
 * iOS gives a standalone app one window and surfaces it for a URL in its own scope
 * rather than taking it there, so the running window is pointed by hand. Only a
 * destination inside the app reuses one; anything else opens its own.
 */
const pointAt = async (url: string, inTheApp: boolean): Promise<void> => {
  // A browser opens the link the app named. `openWindow` is refused for another
  // origin in some browsers, and the app's link is on the app's origin, so a refusal
  // falls back to a page of this origin that redirects: a window that opens on the
  // wrong page for an instant beats a click that opens nothing.
  if (!inTheApp) {
    const opened = await worker.clients.openWindow(url).then(
      () => true,
      () => false,
    );
    if (!opened) {
      await worker.clients.openWindow(sameOriginWayTo(url));
    }
    return;
  }

  // The installed app has one window, and iOS surfaces it for a URL in its own scope
  // rather than taking it there, so the window is pointed by hand. With no window to
  // point, opening one is what is left.
  const window = await appWindow();
  if (window === undefined) {
    await worker.clients.openWindow(url);
    return;
  }
  // Refused in some app views, where the page's own `hashchange` is what is left.
  await window.navigate(url).catch(() => undefined);
  await window.focus();
};

worker.addEventListener("notificationclick", (event) => {
  // Diagnostic, on beta only: a click that opens nothing and dismisses nothing has
  // not reached this handler at all, and that is worth telling apart from one that
  // reached it and failed. Remove once the desktop behaviour is confirmed.
  const seen = {
    title: event.notification.title,
    data: event.notification.data as unknown,
  };
  // A click does not take the notification off the screen by itself.
  event.notification.close();
  const target = refOf(event.notification.data);
  const url =
    typeof event.notification.data?.url === "string"
      ? event.notification.data.url
      : undefined;
  event.waitUntil(
    (async () => {
      // Opening comes first: the click is what permits a worker to open a window,
      // and that permission does not survive an await on a canister call. The
      // app's window cannot be focused instead, because `matchAll` sees this origin
      // only and the link is the app's.
      void trace({
        step: "clicked",
        ...seen,
        hasRef: target !== undefined,
        urlType: typeof url,
      });
      if (target !== undefined && url !== undefined) {
        const destination = await openable(url, {
          title: event.notification.title,
          body: event.notification.body,
        });
        // A refused window must not cost the app its report, but it is reported:
        // a click that opens nothing looks to the user like a dismissal.
        void trace({
          step: "opening",
          destination: destination.url,
          inTheApp: destination.inTheApp,
          agent: navigator.userAgent,
        });
        await pointAt(destination.url, destination.inTheApp).then(
          () => trace({ step: "opened", destination: destination.url }),
          (error: unknown) =>
            trace({ step: "open-failed", error: String(error) }),
        );
      } else {
        const clients = await worker.clients.matchAll({
          type: "window",
          includeUncontrolled: true,
        });
        void trace({
          step: "no-link",
          why: "the notification carried no readable reference or url",
          clients: clients.map((client) => client.url),
        });
        const open = clients.find((client) =>
          client.url.startsWith(worker.origin),
        );
        if (open !== undefined) {
          await open.focus();
        } else {
          await worker.clients.openWindow("/");
        }
      }

      // Then the app learns its notification did its job. Nothing can act on a
      // failure here: a click has no caller to answer and nothing to come back for,
      // the window is open and the notification closed either way, and the queue
      // entry went when it was shown. It costs the app a count, not the user.
      if (target !== undefined && configured) {
        await reportOpened({ ref: target }).catch(() => undefined);
      }
    })(),
  );
});
