/// <reference types="@sveltejs/kit" />
/// <reference lib="webworker" />

// The Web Push worker. It handles pushes and nothing else: no `fetch` listener, so
// it never sits between the page and the network, and no precaching, so a stale
// worker cannot serve a stale app.
//
// A push carries no body, since the payload is not sealed to the browser's keys. The
// subscription is `userVisibleOnly`, which obliges a visible notification per push, so
// there is one to show until the content-pull path can say what it is about.

const worker = self as unknown as ServiceWorkerGlobalScope;

const TITLE = "Internet Identity";
const BODY = "You have a new notification.";

worker.addEventListener("install", () => {
  // Nothing is cached, so an older worker has nothing this one needs to inherit.
  void worker.skipWaiting();
});

worker.addEventListener("activate", (event) => {
  event.waitUntil(worker.clients.claim());
});

worker.addEventListener("push", (event) => {
  event.waitUntil(
    worker.registration.showNotification(TITLE, {
      body: BODY,
      // One notification rather than a pile: the next push replaces this one until
      // there is per-notification content to tell them apart by.
      tag: "internet-identity",
    }),
  );
});

worker.addEventListener("notificationclick", (event) => {
  event.notification.close();
  event.waitUntil(
    (async () => {
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
