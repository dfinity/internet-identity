// Opt-in orchestration: ask for permission, subscribe the device with a fresh
// VAPID key + signed JWT pool, record consent, and mint the service worker's
// pull credential. The device is subscribed before consent so the canister's
// consent-time sealing covers it in one pass.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwTextCanisterError } from "$lib/utils/utils";
import { requestNotificationPermission } from "./pushSubscription";
import { subscribeAndRegisterDevice } from "./subscribeDevice";
import {
  mintNotificationCredential,
  storeNotificationCredential,
} from "./pullCredential";

export type EnableNotificationsResult =
  | { status: "enabled" }
  | { status: "permission-denied" };

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

  await subscribeAndRegisterDevice(identityNumber, actor);
  await grantAndMint({ identityNumber, accountNumber, origin, actor });

  return { status: "enabled" };
};

/**
 * Records consent for an app and mints the pull credential, without touching the
 * subscription. For a browser that is already subscribed and only needs to allow
 * one more app, so there is no permission prompt and no new endpoint.
 */
export const allowApp = ({
  identityNumber,
  accountNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> =>
  grantAndMint({ identityNumber, accountNumber, origin, actor });

const grantAndMint = async ({
  identityNumber,
  accountNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  accountNumber?: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> => {
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
};
