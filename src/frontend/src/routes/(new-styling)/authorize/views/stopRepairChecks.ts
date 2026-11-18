import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { currentBrowserId } from "$lib/stores/browser-key.store";
import { throwCanisterError } from "$lib/utils/utils";

/**
 * Declines a repair on this device only: drops this browser's registration, the
 * subscription the check found dead and the Home Screen app it may route through, so
 * nothing is left on its path and no later sign-in probes it. The identity's consent,
 * which covers every device, is left as it is. Notifications reach this device again
 * once the user turns them on here.
 *
 * `identityNumber` is the identity signed in, and `actor` signs as it. A browser with
 * no registration of its own has no path to check and nothing to drop.
 */
export const stopRepairChecks = async ({
  identityNumber,
  actor,
}: {
  identityNumber: bigint;
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> => {
  const browserId = await currentBrowserId(identityNumber);
  if (browserId === undefined) {
    return;
  }
  await actor
    .remove_webpush_subscription({
      anchor_number: identityNumber,
      browser_id: browserId,
    })
    .then(throwCanisterError);
};
