// Service worker for II-hosted Web Push (Option A): II receives push
// traffic for every dApp the anchor has granted consent to, and this
// worker renders each notification with the source dApp attributed inline
// so a single install covers all of the user's dApps.
//
// Web Push relays deliver `aes128gcm` ciphertext; the browser decrypts it
// natively using the private half of the P-256 keypair generated at
// subscription time, so `event.data.json()` below is already plaintext.

self.addEventListener("install", () => {
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});

// Ids of messages already shown, so the same notification is never rendered
// twice. Held in the Cache API rather than a variable because a service worker
// is killed between pushes — an in-memory set would forget everything it needs
// to remember. Bounded, and pruned oldest-first.
const SEEN_CACHE = "ii-push-seen-v1";
const SEEN_CAP = 200;

/**
 * Whether `msgId` has already been shown; records it when it hasn't.
 *
 * II sends one id per message, identical for every device in a fan-out, so two
 * subscription rows pointing at this same browser — an endpoint rotation that
 * left a stale row, a second registration, a retried send — arrive with the
 * same id and only the first is rendered.
 *
 * A payload with no id (an older canister) is always shown: failing open is
 * right here, since dropping a real notification is worse than a duplicate.
 */
const alreadyShown = async (msgId) => {
  if (!msgId) {
    return false;
  }
  try {
    const cache = await caches.open(SEEN_CACHE);
    // A synthetic key — nothing is ever fetched from this path.
    const key = new URL(
      `/__ii-push-seen/${encodeURIComponent(msgId)}`,
      self.location.origin,
    );
    if (await cache.match(key)) {
      return true;
    }
    await cache.put(key, new Response(""));
    const keys = await cache.keys();
    if (keys.length > SEEN_CAP) {
      await Promise.all(
        keys
          .slice(0, keys.length - SEEN_CAP)
          .map((stale) => cache.delete(stale)),
      );
    }
    return false;
  } catch (err) {
    // Storage unavailable — show rather than swallow.
    console.warn("[ii-sw] dedup unavailable:", err);
    return false;
  }
};

self.addEventListener("push", (event) => {
  if (!event.data) {
    // Empty pushes are valid per spec (used to just wake the worker), but
    // II's payload is always JSON — log so a protocol break is noticed.
    console.warn("[ii-sw] push event with no data");
    return;
  }

  let alert;
  try {
    alert = event.data.json();
  } catch (err) {
    console.error("[ii-sw] push body was not JSON:", err);
    return;
  }

  const hostname = alert.hostname || "Internet Identity";
  const title = alert.title || "";
  const body = alert.body || "";

  event.waitUntil(
    (async () => {
      if (await alreadyShown(alert.msg_id)) {
        // Showing nothing can make the browser render its own "site updated in
        // background" notice, and at volume that costs the permission. Accepted
        // here: a suppressed duplicate is rare and always preceded by the real
        // notification, so the user has already been told.
        console.warn("[ii-sw] suppressed a duplicate notification");
        return;
      }
      await self.registration.showNotification(hostname, {
        body:
          title !== "" && body !== "" ? `${title} — ${body}` : title || body,
        // Deliberately no `tag`: tagging by hostname made every notification
        // from an app replace that app's previous one, silently destroying an
        // unread notification. Distinct notifications must stack; collapsing
        // is something a sender opts into per message, never automatic.
        data: { origin: alert.hostname || null, url: alert.url || null },
      });
    })(),
  );
});

self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const data = event.notification.data || {};

  // A deep link goes straight to the app. II validated at send time that it is
  // on the sender's own origin, so there is nothing left to check here — and
  // routing through /notify would only add a second visit to II in the middle
  // of a journey the user expects to be one step. The scheme is re-checked
  // anyway, cheaply, so a regression on the send side cannot turn into a
  // navigation to something that isn't a web page.
  if (data.url) {
    let target;
    try {
      target = new URL(data.url);
    } catch {
      target = null;
    }
    if (
      target !== null &&
      (target.protocol === "https:" || target.protocol === "http:")
    ) {
      event.waitUntil(self.clients.openWindow(target.href));
      return;
    }
    console.warn("[ii-sw] ignoring a notification url that is not http(s)");
  }

  const origin = data.origin;
  if (!origin) {
    return;
  }
  // No deep link: /notify resolves the sender's own origin, shows which app is
  // being opened, and fails closed if it cannot verify the sender.
  const dest = new URL("/notify", self.location.origin);
  dest.searchParams.set("origin", origin);
  event.waitUntil(self.clients.openWindow(dest.href));
});
