import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
import { requestNotificationPermission } from "$lib/utils/notifications/pushSubscription";
import { ensureRegisteredDevice } from "$lib/utils/notifications/subscribeDevice";

/** Notifications reach this browser now, or the permission is what stands in the way and only the user can lift it. */
export type BrowserSwitchOutcome = "delivering" | "permission-refused";

export const turnOnInBrowser = async (
  identityNumber: bigint,
): Promise<BrowserSwitchOutcome> => {
  const permission = await requestNotificationPermission();
  if (permission !== "granted") {
    return "permission-refused";
  }
  await ensureRegisteredDevice(identityNumber);
  return "delivering";
};

/** Drops this browser's registration. */
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
};
