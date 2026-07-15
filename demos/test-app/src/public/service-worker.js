// Service Worker for the push-notifications smoke test.
//
// The browser natively decrypts the aes128gcm payload with the SW's
// private half of the P-256 subscription key; `event.data.json()` gives
// us the plaintext `PushAlert` JSON produced by the II canister's
// `notify_user`.

self.addEventListener("install", () => {
  self.skipWaiting();
});

self.addEventListener("activate", (event) => {
  event.waitUntil(self.clients.claim());
});

self.addEventListener("push", (event) => {
  if (!event.data) {
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
      tag: alert.hostname || "test-app",
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
