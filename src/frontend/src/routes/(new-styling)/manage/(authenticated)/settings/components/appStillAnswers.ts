import { POLL_INTERVAL_MS } from "$lib/constants/polling.constants";
import { browserAppStatus, probeApp } from "$lib/utils/notifications/appStatus";
import { watchProbe } from "../../../../ios-icp-alerts/watchInstall";

/** Two polls. Nothing tells a canister that a Home Screen app was deleted, so the only
 *  answer is the app's own: one interval would call a live app dead whenever the worker
 *  is slow to wake, and a cold mint on a phone takes about as long as one. An app that
 *  answers inside the window ends the wait there, so this is a ceiling, not a delay. */
const PROBE_WAIT_MS = 2 * POLL_INTERVAL_MS;

/**
 * Whether the app this browser points at is still on the device.
 *
 * `subscribed` outlives the app: deleting it from the Home Screen tells the canister
 * nothing, so an entry can say it delivers for ever while nothing is there to deliver
 * to. Only the app moving its own stamp answers.
 *
 * Says yes wherever nothing was learned. A probe that never left the canister is no
 * evidence either way, and the cost of guessing wrong is handing a second app to a
 * device that already has one.
 */
export const appStillAnswers = async (
  identityNumber: bigint,
): Promise<boolean> => {
  const before = await browserAppStatus(identityNumber);
  if (before.kind !== "app") {
    return false;
  }
  const since = before.app.last_seen_ns;

  const probe = await probeApp(identityNumber);
  switch (probe.kind) {
    case "no-app":
      return false;
    case "not-sent":
      console.error(`the app could not be probed: ${probe.reason}`);
      return true;
    case "sent":
      break;
  }

  return new Promise<boolean>((resolve) => {
    const settle = (answer: boolean) => {
      clearTimeout(timer);
      stop();
      resolve(answer);
    };
    const stop = watchProbe(identityNumber, {
      since,
      onAnswered: () => settle(true),
      onGone: () => settle(false),
    });
    const timer = setTimeout(() => settle(false), PROBE_WAIT_MS);
  });
};
