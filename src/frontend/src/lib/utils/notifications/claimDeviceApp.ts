/**
 * Turning notifications on for an identity through the app this device already has.
 *
 * A browser that cannot hold a subscription of its own, which on iOS is every browser,
 * has the installed app deliver instead. The app is not running and does not have to be:
 * its key says which entry to create and its subscription is what makes that entry
 * reachable, and both are read from the canister through an identity already signed in
 * to this browser.
 */
import { throwCanisterError } from "$lib/utils/utils";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { browserKeyActor } from "./browserActor";
import { recordClaimedIdentity } from "./deviceRecord";
import { findDeviceApp } from "./deviceApp";

/**
 * Claims this device's app for `identityNumber`, answering whether it was claimed.
 *
 * `false` where the device has no app to claim, which is the case the install steps
 * are for. A claim that the canister refuses throws, because the user pressed a button
 * and the screen has somewhere to say so.
 */
export const claimDeviceApp = async ({
  identityNumber,
  caller,
}: {
  identityNumber: bigint;
  /** Who to call as. Defaults to this browser's key, which is what authorizes it. */
  caller?: ActorSubclass<_SERVICE>;
}): Promise<boolean> => {
  const app = await findDeviceApp(identityNumber);
  if (app === undefined) {
    return false;
  }

  const actor = caller ?? (await browserKeyActor(identityNumber));
  await actor
    .claim_notification_app({
      anchor_number: identityNumber,
      app_key: app.appKey,
      endpoint: app.endpoint,
      vapid_public_key: app.vapidPublicKey,
      jwt_signatures: app.jwtSignatures,
      jwt_issued_at_ns: app.jwtIssuedAtNs,
    })
    .then(throwCanisterError);

  // The app is not running and cannot be told. It reads this on its next wake-up,
  // which is how it learns there is another queue to ask about.
  await recordClaimedIdentity(identityNumber);
  return true;
};
