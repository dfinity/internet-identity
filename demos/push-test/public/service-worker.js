// Service Worker for the push-notifications smoke test.
//
// Web Push relays deliver `aes128gcm` ciphertext; the browser decrypts
// natively using the private half of the P-256 keypair generated at
// subscription time. `event.data.json()` gives us the plaintext PushAlert
// JSON produced by the II canister.

self.addEventListener("install", () => {
  // Take over immediately so the first tab doesn't need a reload.
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});

self.addEventListener("push", (event) => {
  if (!event.data) {
    // Empty pushes are valid per spec (client just wakes the SW), but
    // this PoC always sends a JSON body — log so we notice if that
    // invariant breaks.
    console.warn("[push-sw] push event with no data");
    return;
  }
  let alert;
  try {
    alert = event.data.json();
  } catch (err) {
    console.error("[push-sw] push body was not JSON:", err);
    return;
  }
  event.waitUntil(
    self.registration.showNotification(alert.title || "Notification", {
      body: alert.body || "",
      // The `tag` collapses successive notifications from the same
      // dApp origin into one banner slot on desktop.
      tag: alert.hostname || "push-test",
      data: { url: alert.url || null },
    }),
  );
});

self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const url = event.notification.data && event.notification.data.url;
  if (url) {
    event.waitUntil(self.clients.openWindow(url));
  }
});
