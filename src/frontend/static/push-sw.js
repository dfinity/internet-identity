// Push service worker. Deliberately minimal: it has no `fetch` handler, so it
// never intercepts requests on the auth origin. The browser decrypts the
// payload before `push` fires, so `event.data` is the routing payload
// (`{"o":"<origin>"}`) sealed by II. This interim version shows a generic
// notification; pulling the real content via the stored credential is a later
// step.

self.addEventListener("push", (event) => {
  event.waitUntil(
    self.registration.showNotification("Internet Identity", {
      body: "You have a new notification.",
      icon: "/favicon.svg",
      tag: "ii-notification",
    }),
  );
});

self.addEventListener("notificationclick", (event) => {
  event.notification.close();
  event.waitUntil(self.clients.openWindow("/"));
});
