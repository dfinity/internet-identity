/**
 * Telling an app that somebody acted on one of its notifications.
 *
 * The click carries everything needed: which identity the notification was for, which
 * app sent it and which notification it was, all of it in the data the worker attached
 * when it showed it. The call is signed with the same delegation the content was
 * pulled with, so the app sees it as the pull it already trusts.
 */

import { Principal } from "@icp-sdk/core/principal";
import type { WorkerRegistration } from "./registrationUrl";
import { reportNotificationOpened } from "./appNotifications";
import { loadPullIdentity } from "./pullDelegation";
import type { NotificationRef } from "./shownNotification";

/** Answers whether the app was told, which is what a test has to go on. */
export const reportOpened = async ({
  ref,
  worker,
}: {
  ref: NotificationRef;
  worker: WorkerRegistration;
}): Promise<boolean> => {
  const identity = await loadPullIdentity({
    identityNumber: ref.identityNumber,
    target: { origin: ref.origin, accountNumber: ref.accountNumber },
    internetIdentityCanisterId: worker.canisterId,
    nowMillis: Date.now(),
  });
  if (identity === undefined) {
    // The delegation has expired since the notification was shown. Nothing to
    // do about it here: the click has already opened the app.
    return false;
  }

  await reportNotificationOpened({
    canisterId: Principal.fromText(ref.canisterId),
    id: ref.id,
    identity,
    ...worker.agentOptions,
  });
  return true;
};
