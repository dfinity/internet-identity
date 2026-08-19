// Device-level push enablement for the settings screen. "Notifications on this
// device" is a combined state: the browser holds a push subscription AND at
// least one app is allowed. These helpers keep enable/disable idempotent so the
// toggle can't fail for the many "already in that state" reasons.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwTextCanisterError } from "$lib/utils/utils";
import { generateVapidKeypair, signJwtPool } from "./vapidPool";
import {
  relayOriginOf,
  requestNotificationPermission,
  subscribeToPush,
} from "./pushSubscription";

export type EnableDeviceResult =
  | { status: "enabled" }
  | { status: "permission-denied" };

/** This browser's current push subscription, or `undefined` if not subscribed. */
export const currentDeviceSubscription = async (): Promise<
  PushSubscription | undefined
> => {
  const registration = await navigator.serviceWorker.getRegistration();
  return (await registration?.pushManager.getSubscription()) ?? undefined;
};

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
  const { publicKeyRaw, privateKey } = await generateVapidKeypair();
  const { endpoint, p256dh, auth } = await subscribeToPush(publicKeyRaw);
  const issuedAtNs = BigInt(Date.now()) * BigInt(1_000_000);
  const signatures = await signJwtPool(
    privateKey,
    relayOriginOf(endpoint),
    issuedAtNs,
  );
  await actor
    .webpush_subscribe_device(
      identityNumber,
      endpoint,
      p256dh,
      auth,
      publicKeyRaw,
      signatures,
      issuedAtNs,
    )
    .then(throwTextCanisterError);
  return { status: "enabled" };
};

/** Drops this browser's subscription, canister-side and then locally. No-op if
 * this browser isn't subscribed. */
export const unsubscribeDevice = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<void> => {
  const subscription = await currentDeviceSubscription();
  if (subscription === undefined) {
    return;
  }
  await actor
    .webpush_unsubscribe_device(identityNumber, subscription.endpoint)
    .then(throwTextCanisterError);
  await subscription.unsubscribe();
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

/** Turns notifications off entirely: revokes every allowed app, then
 * unsubscribes this browser. */
export const disableAllNotifications = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<void> => {
  const origins = await actor
    .notification_consented_origins(identityNumber)
    .catch(() => [] as string[]);
  for (const origin of origins) {
    await revokeApp(identityNumber, actor, origin);
  }
  await unsubscribeDevice(identityNumber, actor);
};
