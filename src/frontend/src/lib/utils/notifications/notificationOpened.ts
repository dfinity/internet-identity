/**
 * Telling an app that somebody acted on one of its notifications.
 *
 * The click carries everything needed: which identity the notification was for, which
 * app sent it and which notification it was, all of it in the data the worker attached
 * when it showed it. The call is signed with the same delegation the content was
 * pulled with, so the app sees it as the pull it already trusts.
 */

import { Principal } from "@icp-sdk/core/principal";
import { agentHost, type AgentLocation } from "$lib/utils/agentHost";
import { reportNotificationOpened } from "./appNotifications";
import { loadPullIdentity } from "./pullDelegation";
import type { NotificationRef } from "./shownNotification";
import { deploymentOf } from "./wakeUp";

/** Answers whether the app was told, which is what a test has to go on. */
export const reportOpened = async ({
  ref,
  location,
}: {
  ref: NotificationRef;
  location: AgentLocation & { search: string };
}): Promise<boolean> => {
  const deployment = deploymentOf(location.search);
  if (deployment === undefined) {
    return false;
  }
  const identity = await loadPullIdentity({
    identityNumber: ref.identityNumber,
    target: { origin: ref.origin, accountNumber: ref.accountNumber },
    internetIdentityCanisterId: deployment.canisterId,
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
    host: agentHost(location),
    shouldFetchRootKey: deployment.shouldFetchRootKey,
  });
  return true;
};
