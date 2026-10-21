/** The subscription is the app's own and every other identity pointing at it delivers through the same one, so this flag is the whole of whether the app delivers for this identity.
 * Signed as the browser: the canister reads which app this is off its caller, and a session delegation is not the key the app was registered under.
 * Switching on is `enableApp` in `appStatus.ts`, which is also the probe: asking the app to deliver is what asks it to prove it is there. */
import { throwCanisterError } from "$lib/utils/utils";
import { browserKeyActor } from "./browserActor";

export const disableApp = async (identityNumber: bigint): Promise<void> => {
  const actor = await browserKeyActor(identityNumber);
  await actor
    .disable_notification_app({ anchor_number: identityNumber })
    .then(throwCanisterError);
};
