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
import {
  dataOf,
  refOf,
  tagOf,
} from "$lib/utils/notifications/worker/shownNotification";
import { onIos, openable } from "$lib/utils/notifications/worker/openTarget";
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

// Diagnostic, on beta only: every start of this worker is recorded, so a click that
// reaches no handler can be told apart from one whose worker never started. A click
// on a terminated worker makes Chrome evaluate this script first, and an evaluation
// that throws loses the click with no sign of it. Remove with the tracing above.
void trace({ step: "evaluated", build: "no-enumerate-1" });

worker.addEventListener("push", (event) => {
  event.waitUntil(
    enqueue(async () => {
      void trace({
        step: "push",
        build: "no-enumerate-1",
        configured,
      });
      if (!configured) {
        // Nothing can be asked of anyone without knowing which canister to ask, and
        // the subscription still owes the user a notification.
        await showPlaceholder(worker.registration);
        void trace({ step: "placeholder-shown" });
        return;
      }
      await onWakeUp({ registration: worker.registration });
      void trace({ step: "woke" });
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
  if (!inTheApp) {
    await worker.clients.openWindow(url);
    return;
  }

  const window = await appWindow();
  if (window === undefined) {
    await worker.clients.openWindow(url);
    return;
  }
  // Refused in some app views, where the page's own `hashchange` is what is left.
  await window.navigate(url).catch(() => undefined);
  await window.focus();
};

/**
 * Opens what a click names, starting with what the click itself permits.
 *
 * A browser's permission to open a window belongs to the synchronous run of the
 * click handler: anything awaited before `openWindow` spends it, and the click then
 * opens nothing. So the ordinary path calls it first and asks questions afterwards.
 * Only the installed iOS app needs to be told where to go before it can go there,
 * and iOS is also the one platform that honours the call after the fact.
 */
const openOnClick = (
  url: string,
  shown: { title: string; body: string },
): Promise<void> => {
  if (!onIos()) {
    return worker.clients.openWindow(url).then(
      () => undefined,
      () => undefined,
    );
  }
  return (async () => {
    const destination = await openable(url, shown);
    await pointAt(destination.url, destination.inTheApp);
  })();
};

/** Where a click with nothing to open goes: the app this worker belongs to. */
const openTheApp = async (): Promise<void> => {
  const clients = await worker.clients.matchAll({
    type: "window",
    includeUncontrolled: true,
  });
  const open = clients.find((client) => client.url.startsWith(worker.origin));
  if (open !== undefined) {
    await open.focus();
    return;
  }
  await worker.clients.openWindow("/");
};

/** Diagnostic, on beta only: the trace page asks the worker to show a notification,
 *  so a worker-shown one can be told apart from a page-shown one. Remove with the
 *  tracing above. */
worker.addEventListener("message", (event) => {
  const ask = event.data as { type?: string; withIcon?: boolean } | undefined;
  if (ask?.type === "trace-show-real") {
    // The tag, data and title a real notification carries, without the canister that
    // would otherwise have to produce one.
    const ref = {
      identityNumber: BigInt(10772),
      origin: "https://oizmi-wyaaa-aaaam-ajo5q-cai.ic0.app",
      accountNumber: undefined,
      appCanisterId: "oizmi-wyaaa-aaaam-ajo5q-cai",
      id: BigInt("8893169870452928613"),
    };
    void worker.registration.showNotification("Test App \u00b7 hello", {
      body: "world",
      tag: tagOf(ref),
      data: dataOf(ref, "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io/#chat"),
    });
    return;
  }
  if (ask?.type !== "trace-show") {
    return;
  }
  const icon =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==";
  void worker.registration.showNotification("Worker test", {
    body:
      ask.withIcon === true
        ? "Shown by the worker, with an icon"
        : "Shown by the worker",
    icon: ask.withIcon === true ? icon : undefined,
    tag: "trace-worker-test",
    data: {
      identityNumber: "10000",
      origin: "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io",
      accountNumber: "",
      appCanisterId: "vt36r-2qaaa-aaaad-aad5a-cai",
      id: "1",
      url: "https://vt36r-2qaaa-aaaad-aad5a-cai.icp0.io/#chat",
    },
  });
});

worker.addEventListener("notificationclick", (event) => {
  // A click does not take the notification off the screen by itself.
  event.notification.close();
  const target = refOf(event.notification.data);
  const url =
    typeof event.notification.data?.url === "string"
      ? event.notification.data.url
      : undefined;
  // Started here rather than inside `waitUntil`: the window has to be asked for
  // while the click still permits it, which is this synchronous run and no later.
  const opening =
    target !== undefined && url !== undefined
      ? openOnClick(url, {
          title: event.notification.title,
          body: event.notification.body,
        })
      : openTheApp();
  event.waitUntil(
    (async () => {
      void trace({
        step: "clicked",
        title: event.notification.title,
        data: event.notification.data as unknown,
        hasRef: target !== undefined,
        urlType: typeof url,
        agent: navigator.userAgent,
      });
      await opening.then(
        () => trace({ step: "opened", destination: url }),
        (error: unknown) =>
          trace({ step: "open-failed", error: String(error) }),
      );

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
