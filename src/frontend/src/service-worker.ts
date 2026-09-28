/// <reference types="@sveltejs/kit" />
/// <reference lib="webworker" />

// The Web Push worker. It handles pushes and nothing else: no `fetch` listener, so
// it never sits between the page and the network, and no precaching, so a stale
// worker cannot serve a stale app.
//
// A push carries no body, since the payload is not sealed to the browser's keys: what
// it is for is asked of the canister, and its content of the app that sent it. The
// subscription is `userVisibleOnly`, which obliges a visible notification per push.

import { onWakeUp } from "$lib/utils/notifications/wakeUp";
import { refOf } from "$lib/utils/notifications/shownNotification";

const worker = self as unknown as ServiceWorkerGlobalScope;

worker.addEventListener("install", () => {
  // Nothing is cached, so an older worker has nothing this one needs to inherit.
  void worker.skipWaiting();
});

worker.addEventListener("activate", (event) => {
  event.waitUntil(worker.clients.claim());
});

worker.addEventListener("push", (event) => {
  event.waitUntil(
    onWakeUp({
      registration: worker.registration,
      location: worker.location,
    }),
  );
});

worker.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const target = refOf(event.notification.data);
  const url =
    typeof event.notification.data?.url === "string"
      ? event.notification.data.url
      : undefined;
  event.waitUntil(
    (async () => {
      // The app's own link, already checked against the origins it publishes as its
      // own. Anything else opens Internet Identity itself.
      if (target !== undefined && url !== undefined) {
        await worker.clients.openWindow(url);
        return;
      }
      const clients = await worker.clients.matchAll({
        type: "window",
        includeUncontrolled: true,
      });
      const open = clients.find((client) =>
        client.url.startsWith(worker.origin),
      );
      if (open !== undefined) {
        await open.focus();
        return;
      }
      await worker.clients.openWindow("/");
    })(),
  );
});
