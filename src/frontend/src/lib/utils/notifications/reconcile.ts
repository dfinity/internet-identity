// Brings one origin's on-screen notifications in line with what its sender
// canisters report as pending. The origin's well-known can authorize several
// canisters, and any of them may own notification content, so the worker pulls
// each one and reconciles per canister: a notification is shown or updated in
// place (keyed by canister + `id` via `tag`), and any the owning canister no
// longer lists is closed. Closing is how a dismissal reaches other devices: the
// app drops the id from its pending set, and every device removes it on the next
// pull.
//
// A canister's result is either the set it reports (authoritative, empty closes
// everything shown for it) or `undefined`, meaning the pull could not be made
// (unreachable, timed out). Unknown is not empty: the canister's notifications
// are left exactly as they are rather than closed on a failure to reach it.
//
// Extracted from the service worker so it can be tested without the worker's
// top-level `self`/event-listener side effects; the worker passes its own
// `registration`.

// Provisional pull interface: the dApp returns the notifications currently
// pending for the authenticated caller. Finalized alongside the client crate.
// `id` is the dApp's stable notification id, unique only within its canister.
export interface PulledNotification {
  id: string;
  title: string;
  body: [] | [string];
}

// One sender canister's pull result. `canister` is its principal text; `pulled`
// is the reported set, or `undefined` when the pull could not be made.
export interface CanisterPull {
  canister: string;
  pulled: PulledNotification[] | undefined;
}

// The tag namespaces a notification by its owning canister, so two canisters
// authorized for the same origin can use the same `id` without one replacing the
// other. It is also how re-showing the same (canister, id) updates in place.
const tagFor = (canister: string, id: string): string => `${canister} ${id}`;

interface NotificationData {
  origin?: string;
  canister?: string;
  id?: string;
}

export const reconcile = async (
  registration: ServiceWorkerRegistration,
  origin: string,
  results: CanisterPull[],
): Promise<void> => {
  // Only canisters that actually answered touch the screen; an unknown result
  // leaves its notifications untouched.
  const known = results.filter(
    (result): result is { canister: string; pulled: PulledNotification[] } =>
      result.pulled !== undefined,
  );
  const shown = await registration.getNotifications();

  for (const { canister, pulled } of known) {
    const pending = new Set(pulled.map((notification) => notification.id));
    for (const notification of shown) {
      const data = notification.data as NotificationData | null;
      if (
        data?.origin === origin &&
        data?.canister === canister &&
        !pending.has(data?.id ?? "")
      ) {
        notification.close();
      }
    }
  }

  await Promise.all(
    known.flatMap(({ canister, pulled }) =>
      pulled.map((notification) =>
        registration.showNotification(notification.title, {
          body: notification.body[0],
          icon: "/favicon.svg",
          tag: tagFor(canister, notification.id),
          data: { origin, canister, id: notification.id },
        }),
      ),
    ),
  );
};
