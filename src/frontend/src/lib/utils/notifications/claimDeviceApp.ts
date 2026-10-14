/**
 * Turning notifications on for an identity through the app this device already has.
 *
 * A browser that cannot hold a subscription of its own, which on iOS is every browser,
 * has the installed app deliver instead. What the app published is enough to do that
 * without the app running: its key says which entry to create, and its subscription is
 * what makes the entry reachable, since only the app holds the key a pool is signed
 * with.
 */
import { throwCanisterError } from "$lib/utils/utils";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { browserKeyActor } from "./browserActor";
import { readDeviceRecord, recordClaimedIdentity } from "./deviceRecord";

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
  const record = await readDeviceRecord();
  if (record === undefined) {
    return false;
  }

  const actor = caller ?? (await browserKeyActor(identityNumber));
  await actor
    .claim_notification_app({
      anchor_number: identityNumber,
      app_key: record.appKey,
      endpoint: record.endpoint,
      vapid_public_key: record.vapidPublicKey,
      jwt_signatures: record.jwtSignatures,
      jwt_issued_at_ns: record.jwtIssuedAtNs,
    })
    .then(throwCanisterError);

  // The app is not running and cannot be told. It reads this on its next wake-up,
  // which is how it learns there is another queue to ask about.
  await recordClaimedIdentity(identityNumber);
  return true;
};
