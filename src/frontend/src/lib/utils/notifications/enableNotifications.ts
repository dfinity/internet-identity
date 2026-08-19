// Opt-in orchestration: ask for permission, subscribe the device with a fresh
// VAPID key + signed JWT pool, record consent, and mint the service worker's
// pull credential. The device is subscribed before consent so the canister's
// consent-time sealing covers it in one pass.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwTextCanisterError } from "$lib/utils/utils";
import { generateVapidKeypair, signJwtPool } from "./vapidPool";
import {
  relayOriginOf,
  requestNotificationPermission,
  subscribeToPush,
} from "./pushSubscription";
import {
  mintNotificationCredential,
  storeNotificationCredential,
} from "./pullCredential";

export type EnableNotificationsResult =
  { status: "enabled" } | { status: "permission-denied" };

export const enableNotifications = async ({
  identityNumber,
  accountNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<EnableNotificationsResult> => {
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
  await actor
    .notification_grant_consent(identityNumber, origin)
    .then(throwTextCanisterError);

  await storeNotificationCredential(
    await mintNotificationCredential({
      identityNumber,
      accountNumber,
      origin,
      actor,
    }),
  );

  return { status: "enabled" };
};
