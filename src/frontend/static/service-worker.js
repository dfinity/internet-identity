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
    self.registration.showNotification(hostname, {
      body: title !== "" && body !== "" ? `${title} — ${body}` : title || body,
      tag: hostname,
      data: { origin: alert.hostname || null, url: alert.url || null },
    }),
  );
});

self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  const data = event.notification.data || {};
  const origin = data.origin;
  if (!origin) {
    return;
  }
  const dest = new URL("/notify", self.location.origin);
  dest.searchParams.set("origin", origin);
  if (data.url) {
    dest.searchParams.set("to", data.url);
  }
  event.waitUntil(self.clients.openWindow(dest.href));
});
