// Opt-in orchestration: ask for permission, subscribe the device with a fresh
// VAPID key + signed JWT pool, then record consent for the app. Subscribing
// first so a refusal at the browser prompt leaves no consent row behind for a
// browser that cannot receive anything.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
import { requestNotificationPermission } from "./pushSubscription";
import { subscribeAndRegisterDevice } from "./subscribeDevice";

export type EnableNotificationsResult =
  { status: "enabled" } | { status: "permission-denied" };

export const enableNotifications = async ({
  identityNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<EnableNotificationsResult> => {
  if (!(await requestNotificationPermission())) {
    return { status: "permission-denied" };
  }

  await subscribeAndRegisterDevice(identityNumber, actor);
  await grantConsent({ identityNumber, origin, actor });

  return { status: "enabled" };
};

/**
 * Records consent for an app without touching the subscription. For a browser
 * that is already subscribed and only needs to allow one more app, so there is
 * no permission prompt and no new endpoint.
 */
export const allowApp = ({
  identityNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> => grantConsent({ identityNumber, origin, actor });

const grantConsent = async ({
  identityNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> => {
  await actor
    .notification_grant_consent(identityNumber, origin)
    .then(throwCanisterError);
};
