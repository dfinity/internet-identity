// Keeps this browser's push registration healthy. The device toggle owns the
// subscription; per-app consent is a separate list and is never touched here.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
import { signJwtPool, windowsRemaining } from "./vapidPool";
import {
  currentDeviceSubscription,
  isPushSupported,
  relayOriginOf,
} from "./pushSubscription";
import { subscribeAndRegisterDevice } from "./subscribeDevice";
import { loadVapidKey } from "./vapidKeyStore";

export { currentDeviceSubscription };

// Windows left in the pool below which the reconcile tops it up. The pool covers
// 30 days; refreshing with a week-plus of headroom keeps a device that opens II
// at least monthly covered without a call on every visit.
const JWT_POOL_REFRESH_THRESHOLD = 10;

/**
 * Run on authenticated boot. Re-subscribes when the browser rotated or dropped
 * its subscription, so the canister stops knowing a dead endpoint, and tops up
 * the JWT pool before it runs out. No-op for a browser that never turned
 * notifications on.
 */
export const reconcileDeviceNotifications = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<void> => {
  if (!isPushSupported() || Notification.permission !== "granted") {
    return;
  }
  const subscription = await currentDeviceSubscription();
  const stored = await loadVapidKey();
  if (subscription === undefined && stored === undefined) {
    return;
  }

  // The canister knows this browser only if the live subscription matches the
  // key we still hold. Anything else — rotated endpoint, dropped subscription,
  // lost key — means subscribe and register afresh.
  if (
    subscription === undefined ||
    stored === undefined ||
    stored.endpoint !== subscription.endpoint
  ) {
    if (stored !== undefined) {
      await actor
        .webpush_unsubscribe_device(identityNumber, stored.endpoint)
        .catch(() => {});
    }
    await subscribeAndRegisterDevice(identityNumber, actor);
    return;
  }

  // Registered: top up the pool before it runs out. A missing status means the
  // canister no longer knows the endpoint, so re-register instead.
  const [status] = await actor.webpush_jwt_pool_status(
    identityNumber,
    subscription.endpoint,
  );
  if (status === undefined) {
    await subscribeAndRegisterDevice(identityNumber, actor);
    return;
  }
  const remaining = windowsRemaining({
    poolLen: status.pool_len,
    issuedAtNs: status.issued_at_ns,
    nowNs: BigInt(Date.now()) * BigInt(1_000_000),
  });
  if (remaining >= JWT_POOL_REFRESH_THRESHOLD) {
    return;
  }
  const issuedAtNs = BigInt(Date.now()) * BigInt(1_000_000);
  const signatures = await signJwtPool(
    stored.privateKey,
    relayOriginOf(subscription.endpoint),
    issuedAtNs,
  );
  await actor
    .webpush_refresh_jwts(
      identityNumber,
      subscription.endpoint,
      signatures,
      issuedAtNs,
    )
    .then(throwCanisterError);
};
