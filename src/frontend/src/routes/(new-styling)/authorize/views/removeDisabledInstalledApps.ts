import type { RemoveUnansweredPwaWebPushSubscriptionsRequest } from "$lib/generated/internet_identity_types";
import { browserKeyActor } from "$lib/utils/notifications/browserActor";
import { throwCanisterError } from "$lib/utils/utils";

/**
 * Removes the installed web apps the app disabled that mute this device, as this
 * browser, so turning notifications on here reaches it again. The removal `request`
 * names the identity, the app, its frontend and the account; `undefined` where no
 * disabled installed app mutes this device, which removes nothing.
 */
export const removeDisabledInstalledApps = async (
  request: RemoveUnansweredPwaWebPushSubscriptionsRequest | undefined,
): Promise<void> => {
  if (request === undefined) {
    return;
  }
  const browser = await browserKeyActor(request.anchor_number);
  await browser
    .remove_unanswered_pwa_webpush_subscriptions(request)
    .then(throwCanisterError);
};
