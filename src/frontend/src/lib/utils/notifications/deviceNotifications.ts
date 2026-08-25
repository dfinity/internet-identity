// Device-level push helpers for the settings screen and the boot-time reconcile.
// The device toggle owns this browser's push subscription; per-app consent is a
// separate list. Enable/disable stays idempotent so the toggle can't fail for
// the many "already in that state" reasons.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwTextCanisterError } from "$lib/utils/utils";
import { signJwtPool } from "./vapidPool";
import {
  currentDeviceSubscription,
  isPushSupported,
  relayOriginOf,
  requestNotificationPermission,
} from "./pushSubscription";
import { subscribeAndRegisterDevice } from "./subscribeDevice";
import { loadVapidKey, purgeVapidKey } from "./vapidKeyStore";

export { currentDeviceSubscription };

export type EnableDeviceResult =
  { status: "enabled" } | { status: "permission-denied" };

// Windows left in the pool below which the reconcile tops it up. The pool covers
// 30 days; refreshing with a week-plus of headroom keeps a device that opens II
// at least monthly covered without a call on every visit.
const JWT_POOL_REFRESH_THRESHOLD = 10;

/**
 * Subscribes this browser to push and registers it with the canister. No app
 * consent is granted here; consent for a given app is recorded during sign-in.
 * Idempotent: `subscribeToPush` drops any stale subscription first.
 */
export const enableDeviceNotifications = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<EnableDeviceResult> => {
  if (!(await requestNotificationPermission())) {
    return { status: "permission-denied" };
  }
  await subscribeAndRegisterDevice(identityNumber, actor);
  return { status: "enabled" };
};

/** Drops this browser's subscription and its stored key, canister-side then
 * locally. Consent for apps is left untouched: it is per identity, and other
 * devices may still use it. */
export const unsubscribeDevice = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<void> => {
  const subscription = await currentDeviceSubscription();
  if (subscription !== undefined) {
    await actor
      .webpush_unsubscribe_device(identityNumber, subscription.endpoint)
      .then(throwTextCanisterError);
    await subscription.unsubscribe();
  }
  await purgeVapidKey();
};

/** Revokes one app's consent and drops the service worker's credential for it.
 * The pullCredential import is lazy so its IndexedDB open stays off page load. */
export const revokeApp = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
  origin: string,
): Promise<void> => {
  await actor
    .notification_revoke_consent(identityNumber, origin)
    .then(throwTextCanisterError);
  await import("./pullCredential")
    .then(({ purgeNotificationCredential }) =>
      purgeNotificationCredential(origin),
    )
    .catch(() => {});
};

/**
 * Keeps this browser's push registration healthy, run on authenticated boot.
 * Re-subscribes when the browser rotated or dropped its subscription, so the
 * canister stops knowing a dead endpoint, and tops up the JWT pool before it
 * runs out. No-op for a browser that never turned notifications on.
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
  // canister dropped the endpoint (e.g. 410-pruned), so re-register instead.
  const [status] = await actor.webpush_jwt_pool_status(
    identityNumber,
    subscription.endpoint,
  );
  if (status === undefined) {
    await subscribeAndRegisterDevice(identityNumber, actor);
    return;
  }
  if (status.remaining >= JWT_POOL_REFRESH_THRESHOLD) {
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
    .then(throwTextCanisterError);
};
