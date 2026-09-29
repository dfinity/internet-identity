/// <reference types="@sveltejs/kit" />
/// <reference lib="webworker" />

// The Web Push worker. It handles pushes and nothing else: no `fetch` listener, so
// it never sits between the page and the network, and no precaching, so a stale
// worker cannot serve a stale app.
//
// A push carries no body, since the payload is not sealed to the browser's keys: what
// it is for is asked of the canister, and its content of the app that sent it. The
// subscription is `userVisibleOnly`, which obliges a visible notification per push.

import { reportOpened } from "$lib/utils/notifications/notificationOpened";
import { onWakeUp, sequencer } from "$lib/utils/notifications/wakeUp";
import { refOf } from "$lib/utils/notifications/shownNotification";

const worker = self as unknown as ServiceWorkerGlobalScope;

worker.addEventListener("install", () => {
  // Nothing is cached, so an older worker has nothing this one needs to inherit.
  void worker.skipWaiting();
});

worker.addEventListener("activate", (event) => {
  event.waitUntil(worker.clients.claim());
});

// One wake-up at a time, whatever the browser delivers: see `sequencer`.
const next = sequencer();

worker.addEventListener("push", (event) => {
  event.waitUntil(
    next(() =>
      onWakeUp({
        registration: worker.registration,
        location: worker.location,
      }),
    ),
  );
});

worker.addEventListener("notificationclick", (event) => {
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
      // app's window cannot be focused instead — `matchAll` sees this origin only,
      // and the link is the app's.
      if (target !== undefined && url !== undefined) {
        // A refused window must not cost the app its report.
        await worker.clients.openWindow(url).catch(() => undefined);
      } else {
        const clients = await worker.clients.matchAll({
          type: "window",
          includeUncontrolled: true,
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

      // Then the app learns its notification did its job.
      if (target !== undefined) {
        await reportOpened({ ref: target, location: worker.location }).catch(
          () => undefined,
        );
      }
    })(),
  );
});
