// Service worker for II-hosted Web Push (Option A): II receives push
// traffic for every dApp the anchor has granted consent to, and this
// worker renders each notification with the source dApp attributed inline
// so a single install covers all of the user's dApps.
//
// Web Push relays deliver `aes128gcm` ciphertext; the browser decrypts it
// natively using the private half of the P-256 keypair generated at
// subscription time, so `event.data.json()` below is already plaintext.

// Identifies our own action button in `notificationclick`. Must match the
// `action` given to showNotification.
const ACTION_MANAGE = "ii-manage";

// The notification title is derived from the sender's origin and never from
// anything the sender supplies, so one dApp cannot publish a notification wearing
// another's name. Two consequences:
//
//  - the bare host reads better than the full URL and is just as unspoofable, so
//    that is what gets shown;
//  - a friendly name has to reach II over a channel tied to the origin. The right
//    home is a `name` field in the sender's `/.well-known/ii-push-senders`, which
//    only whoever controls the origin can publish. That field does not exist yet,
//    so DEMO_NAMES stands in for it — demo scaffolding, not a design, and it must
//    not grow into a sender-supplied display name.
const DEMO_NAMES = {
  "enlargement-their-dayton-aus.trycloudflare.com": "MULTI/DEX",
  "frontend.local.localhost:8000": "MULTI/DEX",
  "multidex.ai": "MULTI/DEX",
};

const senderLabel = (origin) => {
  let host;
  try {
    const u = new URL(origin);
    host = u.port === "" ? u.hostname : `${u.hostname}:${u.port}`;
  } catch {
    host = origin;
  }
  return DEMO_NAMES[host] ?? host;
};
// Where that button goes: the settings section listing every consented app, each
// with a remove control, plus the per-device toggle.
const SETTINGS_PATH = "/manage/settings";

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
  const sender = senderLabel(hostname);
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
      await self.registration.showNotification(sender, {
        body:
          title !== "" && body !== "" ? `${title} — ${body}` : title || body,
        // Deliberately no `tag`: tagging by hostname made every notification
        // from an app replace that app's previous one, silently destroying an
        // unread notification. Distinct notifications must stack; collapsing
        // is something a sender opts into per message, never automatic.
        data: { origin: alert.hostname || null, url: alert.url || null },
        // A route to the controls on every notification, owned by II rather than
        // by the sender — the one user protection a per-dApp push setup cannot
        // offer, since an app that controlled its own notifications would never
        // add this. Platforms render about two buttons and ignore the field
        // entirely where unsupported (Safari, iOS), so this degrades to a plain
        // notification rather than breaking it.
        //
        // "Manage" rather than "Unsubscribe" on purpose, for two reasons: Chrome
        // already puts its own unsubscribe affordance on a notification, so a
        // second one competing with it is confusing; and this button navigates
        // to settings rather than revoking anything, so promising otherwise
        // would be a lie.
        actions: [{ action: ACTION_MANAGE, title: "Manage" }],
      });
    })(),
  );
});

self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const data = event.notification.data || {};

  // Navigates rather than acting: revoking a consent is authorized by the anchor,
  // and this worker holds no authority for it. Landing on the settings page is the
  // better shape anyway — the user confirms, and sees every other app they have
  // granted while they are there. `app` selects which row to surface; the page
  // falls back to the plain list without it.
  if (event.action === ACTION_MANAGE) {
    const dest = new URL(SETTINGS_PATH, self.location.origin);
    if (data.origin) {
      dest.searchParams.set("app", data.origin);
    }
    event.waitUntil(self.clients.openWindow(dest.href));
    return;
  }

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
