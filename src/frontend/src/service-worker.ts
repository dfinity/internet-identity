/// <reference types="@sveltejs/kit" />
/// <reference lib="webworker" />

// The Web Push worker. It handles pushes and nothing else: no `fetch` listener, so
// it never sits between the page and the network, and no precaching, so a stale
// worker cannot serve a stale app.
//
// A push names the identity, browser and inbox entry it was sent for, and carries
// nothing else: the content is fetched from the app that sent it, with a delegation
// minted for the purpose, so it never reaches the push service. The browser decrypts
// the payload before this worker is woken. The subscription is `userVisibleOnly`,
// which obliges a visible notification per push.

import { reportOpened } from "$lib/utils/notifications/worker/notificationOpened";
import {
  entryIdFrom,
  onWakeUp,
  showPlaceholder,
} from "$lib/utils/notifications/worker/wakeUp";
import { initWorkerConfig } from "$lib/utils/notifications/workerConfig";
import { promiseQueue } from "$lib/utils/promiseQueue";
import {
  refOf,
  urlOf,
} from "$lib/utils/notifications/worker/shownNotification";
import {
  openOnClick,
  openTheApp,
} from "$lib/utils/notifications/worker/openWindow";

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

// One wake-up at a time, whatever the browser delivers. Each handler sweeps the
// registration and then decides whether it still owes the user a notification, and two
// running at once would read each other's half-finished answer to that.
// `waitUntil` extends this worker's life; it does not serialise its handlers.
const enqueue = promiseQueue();

worker.addEventListener("push", (event) => {
  event.waitUntil(
    enqueue(async () => {
      if (!configured) {
        // Nothing can be asked of anyone without knowing which canister to ask, and
        // the subscription still owes the user a notification.
        await showPlaceholder(worker.registration);
        return;
      }
      await onWakeUp(
        { registration: worker.registration },
        entryIdFrom(event.data),
      );
    }),
  );
});

worker.addEventListener("notificationclick", (event) => {
  // A click does not take the notification off the screen by itself.
  event.notification.close();
  const ref = refOf(event.notification.data);
  const url = urlOf(event.notification.data);
  // Started here rather than inside `waitUntil`: the window has to be asked for while
  // the click still permits it, which is this synchronous run and no later.
  const opening =
    ref !== undefined && url !== undefined
      ? openOnClick(worker, url, {
          title: event.notification.title,
          body: event.notification.body,
        })
      : openTheApp(worker);
  event.waitUntil(
    (async () => {
      await opening.catch(() => undefined);

      // Nothing can act on a failure here: a click has no caller to answer, the window
      // is open and the notification closed either way, and the inbox entry went when
      // it was shown. It costs the app a count, not the user.
      if (ref !== undefined && configured) {
        await reportOpened({ ref }).catch(() => undefined);
      }
    })(),
  );
});
