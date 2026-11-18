/**
 * After a sign-in to an app, checks that the way this device receives the app's
 * notifications still works, and repairs what can be repaired without asking.
 *
 * Nothing tells the canister that a Home Screen app was deleted or a subscription broke,
 * so each subscription counts the pushes nobody fetched. Only a path past the limit is
 * probed: the probe is Internet Identity's own notification, and a fetch through the
 * subscription answers it. An installed web app that does not answer is removed, so the
 * app's notifications come back here; this browser's own path is then probed in turn.
 */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  NotificationPath,
  NotificationPathStatus,
  ProbeNotificationPathError,
} from "$lib/generated/internet_identity_types";
import { POLL_INTERVAL_MS } from "$lib/constants/polling.constants";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import {
  browserKeyActor,
  UnregisteredBrowserError,
} from "$lib/utils/notifications/browserActor";
import { PROBE_WAIT_MS } from "$lib/utils/notifications/turnOnAndProve";
import { notificationsNeedInstallHere } from "$lib/utils/notifications/notificationState";
import { currentDeviceSubscription } from "$lib/utils/notifications/pushSubscription";
import { ensureRegisteredDevice } from "$lib/utils/notifications/subscribeDevice";

/** What the sign-in does once the check is over. */
export type PathCheckOutcome =
  /** Nothing to do, or repaired without the user. */
  | { kind: "working" }
  /** Ask the user on the allow screen. `installFirst` opens it on the install steps,
   *  for a Home Screen app that stopped answering. */
  | { kind: "ask"; installFirst: boolean };

/** Whether a probe was answered, or the path had nothing on it to probe. */
type ProbeAnswer = "answered" | "unanswered" | "nothing-on-path";

/** The outcome where nothing is left for the user to do. */
const working: PathCheckOutcome = { kind: "working" };

/** Checks the path that delivers `origin`'s notifications to this device for the identity
 *  `identityNumber` names, signing as it with `actor`. Calls `onPastLimit` once a path is
 *  past its limit, before anything is probed, and stops with `signal`'s reason once it
 *  is aborted. */
export const checkNotificationPath = async ({
  identityNumber,
  origin,
  actor,
  onPastLimit,
  signal,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
  onPastLimit: () => void;
  signal: AbortSignal;
}): Promise<PathCheckOutcome> => {
  const consented = await actor.notification_consent_granted({
    anchor_number: identityNumber,
    origin,
  });
  if (!consented) {
    return working;
  }
  let browser: ActorSubclass<_SERVICE>;
  try {
    browser = await browserKeyActor(identityNumber);
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return working;
    }
    throw error;
  }
  const installedApps: NotificationPath = { InstalledApps: origin };
  const thisBrowser: NotificationPath = { ThisBrowser: null };

  const installedAppsStatus = await pathStatus(
    browser,
    identityNumber,
    installedApps,
  );
  if (isPastLimit(installedAppsStatus)) {
    onPastLimit();
    const answer = await probe(browser, identityNumber, installedApps, signal);
    if (answer === "answered") {
      return working;
    }
    await browser
      .remove_unanswered_pwa_webpush_subscriptions({
        anchor_number: identityNumber,
        origin,
      })
      .then(throwCanisterError);
    // This browser's own path carries the app's notifications from here on, whatever its count says.
    return afterOwnPath(
      await probe(browser, identityNumber, thisBrowser, signal),
      identityNumber,
    );
  }

  const thisBrowserStatus = await pathStatus(
    browser,
    identityNumber,
    thisBrowser,
  );
  if (!isPastLimit(thisBrowserStatus)) {
    return working;
  }
  onPastLimit();
  return afterOwnPath(
    await probe(browser, identityNumber, thisBrowser, signal),
    identityNumber,
  );
};

/** What is left once this browser's own path was probed. */
const afterOwnPath = async (
  answer: ProbeAnswer,
  identityNumber: bigint,
): Promise<PathCheckOutcome> => {
  switch (answer) {
    case "answered":
      return working;
    case "nothing-on-path":
      return { kind: "ask", installFirst: false };
    case "unanswered":
      break;
  }
  // The Home Screen app this browser routes through is gone, and only installing it again brings it back.
  if (notificationsNeedInstallHere()) {
    return { kind: "ask", installFirst: true };
  }
  if (
    typeof Notification === "undefined" ||
    Notification.permission !== "granted"
  ) {
    return { kind: "ask", installFirst: false };
  }
  await resubscribe(identityNumber);
  return working;
};

/** Drops the subscription that stopped answering and registers a fresh one, which needs
 *  no prompt where the permission is held. */
const resubscribe = async (identityNumber: bigint): Promise<void> => {
  await (await currentDeviceSubscription())?.unsubscribe();
  await ensureRegisteredDevice(identityNumber);
};

const pathStatus = (
  browser: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  path: NotificationPath,
): Promise<NotificationPathStatus> =>
  browser
    .get_notification_path_status({ anchor_number: identityNumber, path })
    .then(throwCanisterError);

const isPastLimit = (status: NotificationPathStatus): boolean =>
  status.subscriptions.some(
    ({ unanswered_pushes: [unanswered] }) => unanswered?.is_past_limit === true,
  );

/** Every subscription on the path fetched what was sent through it since. */
const isAnswered = (status: NotificationPathStatus): boolean =>
  status.subscriptions.every(
    ({ unanswered_pushes }) => unanswered_pushes.length === 0,
  );

/** Sends the probe and waits up to {@link PROBE_WAIT_MS} for every subscription on the
 *  path to fetch it, or until `signal` is aborted. */
const probe = async (
  browser: ActorSubclass<_SERVICE>,
  identityNumber: bigint,
  path: NotificationPath,
  signal: AbortSignal,
): Promise<ProbeAnswer> => {
  try {
    await browser
      .probe_notification_path({ anchor_number: identityNumber, path })
      .then(throwCanisterError);
  } catch (error) {
    if (
      isCanisterError<ProbeNotificationPathError>(error) &&
      error.type === "NotReachable"
    ) {
      return "nothing-on-path";
    }
    throw error;
  }
  const deadline = Date.now() + PROBE_WAIT_MS;
  while (Date.now() < deadline) {
    await new Promise((resolve) => setTimeout(resolve, POLL_INTERVAL_MS));
    signal.throwIfAborted();
    if (isAnswered(await pathStatus(browser, identityNumber, path))) {
      return "answered";
    }
  }
  return "unanswered";
};
