/** The subscription is the app's own and every other identity pointing at it delivers through the same one, so this flag is the whole of whether the app delivers for this identity.
 * Signed as the browser: the canister reads which app this is off its caller, and a session delegation is not the key the app was registered under. */
import { throwCanisterError } from "$lib/utils/utils";
import { browserKeyActor } from "./browserActor";

export const setAppEnabled = async ({
  identityNumber,
  enabled,
}: {
  identityNumber: bigint;
  enabled: boolean;
}): Promise<void> => {
  const actor = await browserKeyActor(identityNumber);
  await actor
    .set_notification_app_enabled({ anchor_number: identityNumber, enabled })
    .then(throwCanisterError);
};
