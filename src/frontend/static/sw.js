// Internet Identity push notification service worker.
//
// The canister currently sends empty-body pushes. To still show the sender
// and subject in the notification we run a PoC side-channel: the postbox
// page posts a `LATEST_EMAIL` message to this worker whenever it polls, we
// cache the metadata in memory, and the `push` handler reads from that
// cache. When the SW is cold-started by a push (no open page), the cache is
// empty and we fall back to a generic notification.
//
// The proper fix is RFC 8291 payload encryption in the canister — once that
// lands the `push` handler will just read from `event.data` and the
// side-channel code can go. The message handler + cache in this file
// preserve the UX until that ships.

const POSTBOX_URL = "/manage/postbox";
// Fragment on the postbox page that marks the email detail pane — lets the
// browser auto-scroll to the email contents on mobile after a click.
const POSTBOX_DETAIL_HASH = "#email-detail";
// Shared tag for every new-email notification so the browser coalesces
// rapid-fire pushes into a single entry in the notification tray.
const NEW_EMAIL_TAG = "ii-new-email";

// PoC: in-memory cache of the most recent email metadata, populated by
// `LATEST_EMAIL` messages from open postbox pages. Cleared on worker
// termination, which is fine — the `push` handler falls back to a generic
// notification when it's null.
let latestEmail = null;

self.addEventListener("install", (event) => {
  // Activate the new worker immediately rather than waiting for all clients
  // to close.
  event.waitUntil(self.skipWaiting());
});

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});

self.addEventListener("message", (event) => {
  // Only accept messages from same-origin clients. A compromised iframe or
  // hostile popup registered against a different origin must not be able to
  // poison the notification cache.
  if (event.origin !== self.location.origin) {
    return;
  }
  if (event.data?.type === "LATEST_EMAIL") {
    latestEmail = {
      from: event.data.from,
      subject: event.data.subject,
    };
  }
});

self.addEventListener("push", (event) => {
  // Prefer the sender/subject posted by an open postbox page. Fall back to
  // a generic message when the cache is empty (e.g. the worker was
  // cold-started by this push event with no page open).
  const title =
    latestEmail?.from && latestEmail.from.length > 0
      ? latestEmail.from
      : "Internet Identity";
  const body =
    latestEmail?.subject && latestEmail.subject.length > 0
      ? latestEmail.subject
      : "You have a new email in your postbox.";
  const options = {
    body,
    icon: "/favicon.svg",
    badge: "/favicon.svg",
    // Same tag across all new-email notifications → the platform silently
    // replaces any earlier one rather than stacking. `renotify` defaults to
    // false, which is what we want: no repeated alert sounds when a backlog
    // of queued pushes flushes at once (e.g. after the device comes online).
    tag: NEW_EMAIL_TAG,
    data: { url: POSTBOX_URL + POSTBOX_DETAIL_HASH },
  };
  event.waitUntil(self.registration.showNotification(title, options));
});

self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const targetUrl = event.notification.data?.url ?? POSTBOX_URL;

  event.waitUntil(
    (async () => {
      const allClients = await self.clients.matchAll({
        type: "window",
        includeUncontrolled: true,
      });

      // Prefer an existing /manage tab so we don't duplicate.
      for (const client of allClients) {
        const url = new URL(client.url);
        if (url.pathname.startsWith("/manage")) {
          // Tell the page to jump to the newest email (index 0) and scroll
          // the detail pane into view. The page already defaults to index 0
          // on mount, but an already-open tab may be parked on a different
          // selection.
          client.postMessage({ type: "SHOW_LATEST_EMAIL" });
          try {
            await client.navigate(targetUrl);
          } catch {
            // navigate() can fail if the client is cross-origin; the
            // postMessage above is the fallback.
          }
          return client.focus();
        }
      }

      // No existing tab — open a new one.
      return self.clients.openWindow(targetUrl);
    })(),
  );
});
