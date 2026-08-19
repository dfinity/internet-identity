// Brings one origin's on-screen notifications in line with the set the dApp
// reports as pending. Each pending notification is shown or updated in place
// (keyed by its `id` via `tag`), and any notification the dApp no longer lists
// is closed. Closing is how a dismissal reaches other devices: the app drops
// the id from its pending set, and every device removes it on the next pull.
//
// Extracted from the service worker so it can be tested without the worker's
// top-level `self`/event-listener side effects; the worker passes its own
// `registration`.

// Provisional pull interface: the dApp returns the notifications currently
// pending for the authenticated caller. Finalized alongside the client crate.
// `id` is the dApp's stable notification id, used as the notification `tag`.
export interface PulledNotification {
  id: string;
  title: string;
  body: [] | [string];
}

export const reconcile = async (
  registration: ServiceWorkerRegistration,
  origin: string,
  pulled: PulledNotification[],
): Promise<void> => {
  const pending = new Set(pulled.map((notification) => notification.id));
  const shown = await registration.getNotifications();
  for (const notification of shown) {
    const forThisOrigin =
      (notification.data as { origin?: string } | null)?.origin === origin;
    if (forThisOrigin && !pending.has(notification.tag)) {
      notification.close();
    }
  }
  await Promise.all(
    pulled.map((notification) =>
      registration.showNotification(notification.title, {
        body: notification.body[0],
        icon: "/favicon.svg",
        tag: notification.id,
        data: { origin, id: notification.id },
      }),
    ),
  );
};
