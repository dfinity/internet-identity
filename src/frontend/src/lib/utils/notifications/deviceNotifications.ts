// Keeps this browser's push registration healthy. The device toggle owns the
// subscription; per-app consent is a separate list and is never touched here.

import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import {
  registerStoredDevice,
  subscribeAndRegisterDevice,
} from "./subscribeDevice";
import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";
import { notificationsTurnedOff } from "./turnedOff";

export { currentDeviceSubscription };

/**
 * Run on authenticated boot. Re-subscribes when the browser rotated or dropped its
 * subscription, so the canister stops knowing a dead endpoint. No-op for a browser
 * that never turned notifications on.
 *
 */
export const reconcileDeviceNotifications = async (
  identityNumber: bigint,
): Promise<void> => {
  if (!isPushSupported() || Notification.permission !== "granted") {
    return;
  }
  // The switch in settings takes this identity's registration away; registering it again here is what would make turning notifications off not stay off.
  if (await notificationsTurnedOff(identityNumber)) {
    return;
  }
  const subscription = await currentDeviceSubscription();

  // What the canister holds is what says whether this identity ever turned them on: a
  // browser with neither a live subscription nor a registration never did, and
  // subscribing for it here would turn them on by itself.
  // Signed as the browser, which is what the canister reads the registration off, so
  // a browser no sign-in has registered has nothing to ask about.
  let actor;
  try {
    actor = await browserKeyActor(identityNumber);
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return;
    }
    throw error;
  }
  const [status] = await actor.get_webpush_subscription_status({
    anchor_number: identityNumber,
  });
  if (status === undefined && subscription === undefined) {
    return;
  }
  // Nothing registered is what a second identity on this browser looks like, and a
  // different endpoint is what another identity's re-subscribe left behind. Either way,
  // register what the browser already holds rather than rotating it away.
  if (
    subscription !== undefined &&
    status !== undefined &&
    status.endpoint === subscription.endpoint
  ) {
    return;
  }
  // Read again, because the switch in settings can have been pressed while the status
  // query was in flight, and registering here would quietly undo an Off the user has
  // already been told succeeded.
  if (await notificationsTurnedOff(identityNumber)) {
    return;
  }
  // A browser that dropped its subscription makes a new one; one that still holds it
  // registers what it holds, since re-subscribing would take the endpoint every other
  // identity here is registered with.
  if (subscription === undefined) {
    await subscribeAndRegisterDevice(identityNumber);
    return;
  }
  await registerStoredDevice(identityNumber, subscription);
};
