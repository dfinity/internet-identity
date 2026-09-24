// Keeps this browser's push registration healthy. The device toggle owns the
// subscription; per-app consent is a separate list and is never touched here.

import { windowsRemaining } from "./vapidPool";
import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import {
  registerStoredDevice,
  subscribeAndRegisterDevice,
} from "./subscribeDevice";
import { loadVapidKey } from "./vapidKeyStore";
import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";

export { currentDeviceSubscription };

// Windows left in the pool below which the reconcile tops it up. The pool covers 30
// days, so this leaves a week of headroom.
const JWT_POOL_REFRESH_THRESHOLD = 10;

/**
 * Run on authenticated boot. Re-subscribes when the browser rotated or dropped
 * its subscription, so the canister stops knowing a dead endpoint, and tops up
 * the JWT pool before it runs out. No-op for a browser that never turned
 * notifications on.
 */
export const reconcileDeviceNotifications = async (
  identityNumber: bigint,
): Promise<void> => {
  if (!isPushSupported() || Notification.permission !== "granted") {
    return;
  }
  const subscription = await currentDeviceSubscription();
  const stored = await loadVapidKey();
  if (subscription === undefined && stored === undefined) {
    return;
  }

  // The canister knows this browser only if the live subscription matches the key we
  // still hold. Anything else means subscribe and register afresh.
  if (
    subscription === undefined ||
    stored === undefined ||
    stored.endpoint !== subscription.endpoint
  ) {
    // Re-subscribing overwrites the same row, so a rotated endpoint needs no removal.
    await subscribeAndRegisterDevice(identityNumber);
    return;
  }

  // The browser's subscription is sound, so what is left is this identity's own row.
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
  // Nothing registered is what a second identity on this browser looks like, and a
  // different endpoint is what another identity's re-subscribe left behind. A pool
  // running out wants the same call, which signs a fresh one. Either way, register
  // what the browser already holds rather than rotating it away.
  const needsRegistering =
    status === undefined ||
    status.endpoint !== stored.endpoint ||
    windowsRemaining({
      poolLen: status.pool_len,
      issuedAtNs: status.issued_at_ns,
      nowNs: BigInt(Date.now()) * BigInt(1_000_000),
    }) < JWT_POOL_REFRESH_THRESHOLD;
  if (needsRegistering) {
    await registerStoredDevice(identityNumber, stored);
  }
};
