import { POLL_INTERVAL_MS } from "$lib/constants/polling.constants";
import {
  browserAppStatus,
  turnOnApp,
} from "$lib/utils/notifications/appStatus";
import { setAppEnabled } from "./appEnabled";
import { watchProbe } from "../../../routes/(new-styling)/ios-icp-alerts/watchInstall";

/** Two polls. Nothing tells a canister that a Home Screen app was deleted, so the only
 *  answer is the app's own: one interval would call a live app dead whenever the worker
 *  is slow to wake, and a cold mint on a phone takes about as long as one. An app that
 *  answers inside the window ends the wait there, so this is a ceiling, not a delay. */
const PROBE_WAIT_MS = 2 * POLL_INTERVAL_MS;

/**
 * Switches delivery on, and answers whether the app this browser points at took it.
 *
 * `subscribed` outlives the app: deleting it from the Home Screen tells the canister
 * nothing, so an entry can say it delivers for ever while nothing is there to deliver
 * to. Only the app moving its own stamp answers. The switch and the probe go in one
 * call, which is also the one notification the device gets for the press.
 *
 * A probe the canister could not send says the entry has no wake-up address left, which
 * is the app being gone rather than a network that might come back: queuing one makes no
 * outcall, so there is nothing transient to wait out, and the canister leaves the switch
 * off. A probe it did send and nothing answered is the other way an app turns out to be
 * gone, and there the switch did go on: it comes back off here, because an entry that
 * delivers for nobody must not read as delivering.
 */
export const turnOnAndProve = async (
  identityNumber: bigint,
): Promise<boolean> => {
  const before = await browserAppStatus(identityNumber);
  if (before.kind !== "app") {
    return false;
  }
  const since = before.app.last_seen_ns;

  const probe = await turnOnApp(identityNumber);
  switch (probe.kind) {
    case "no-app":
      return false;
    case "not-sent":
      // Nowhere to send it: the record names a subscription the push service no longer
      // has, or none at all. Either way nothing on this device can answer.
      console.error(`the app could not be probed: ${probe.reason}`);
      return false;
    case "sent":
      break;
  }

  const answered = await new Promise<boolean>((resolve) => {
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

  if (!answered) {
    await setAppEnabled({ identityNumber, enabled: false });
  }
  return answered;
};
