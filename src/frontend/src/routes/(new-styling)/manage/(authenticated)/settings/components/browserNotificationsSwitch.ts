import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
import { requestNotificationPermission } from "$lib/utils/notifications/pushSubscription";
import { ensureRegisteredDevice } from "$lib/utils/notifications/subscribeDevice";
import {
  clearNotificationsOff,
  recordNotificationsOff,
} from "$lib/utils/notifications/turnedOff";

/** Notifications reach this browser now, or the permission is what stands in the way and only the user can lift it. */
export type BrowserSwitchOutcome = "delivering" | "permission-refused";

/** Taking back the word that notifications are off goes first, so a storage that refuses it is reported with nothing registered yet. */
export const turnOnInBrowser = async (
  identityNumber: bigint,
): Promise<BrowserSwitchOutcome> => {
  const permission = await requestNotificationPermission();
  if (permission !== "granted") {
    return "permission-refused";
  }
  await clearNotificationsOff(identityNumber);
  await ensureRegisteredDevice(identityNumber);
  return "delivering";
};

/** Drops this browser's registration and leaves this identity's word that it wants none, which keeps the next boot's reconcile from registering it again.
 * The canister goes first: the other order leaves a stored word, a subscription that still delivers, and every later reconcile refusing to touch it. */
export const turnOffInBrowser = async ({
  identityNumber,
  browserId,
  actor,
}: {
  identityNumber: bigint;
  /** Which of the identity's browsers this one is, as its registration recorded it. */
  browserId: number;
  /** Signed as the identity: the subscription hangs off its own entry. */
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> => {
  await actor
    .remove_webpush_subscription({
      anchor_number: identityNumber,
      browser_id: browserId,
    })
    .then(throwCanisterError);
  await recordNotificationsOff(identityNumber);
};
