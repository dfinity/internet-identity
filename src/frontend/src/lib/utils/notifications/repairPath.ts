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
  DisableNotificationAppError,
  NotificationPath,
  NotificationPathStatus,
  ProbeNotificationPathError,
} from "$lib/generated/internet_identity_types";
import { POLL_INTERVAL_MS } from "$lib/constants/polling.constants";
import type { Principal } from "@icp-sdk/core/principal";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
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

/** The calls the check makes as the identity: only its consent is read that way. */
export type IdentityCalls = {
  notification_consent_granted: (
    ...args: Parameters<ActorSubclass<_SERVICE>["notification_consent_granted"]>
  ) => ReturnType<ActorSubclass<_SERVICE>["notification_consent_granted"]>;
};

/** The calls the check makes as this browser. */
export type BrowserCalls = {
  [
    Method in
      | "get_notification_path_status"
      | "probe_notification_path"
      | "remove_unanswered_pwa_webpush_subscriptions"
      | "disable_notification_app"
  ]: (
    ...args: Parameters<ActorSubclass<_SERVICE>[Method]>
  ) => ReturnType<ActorSubclass<_SERVICE>[Method]>;
};

/** The outcome where nothing is left for the user to do. */
const working: PathCheckOutcome = { kind: "working" };

/** Checks the path that delivers `origin`'s notifications to this device for the account
 *  `accountPrincipal` names, of the identity `identityNumber` names: `actor` signs as the
 *  identity and `browser` as this browser. Calls `onPastLimit` once a path is past its
 *  limit, before anything is probed, and stops with `signal`'s reason once it is
 *  aborted.
 *
 *  An installed web app that delivers for the account is the path, and this browser's
 *  own only takes over where none is left: one that is not past its limit is working,
 *  and whatever this browser's own path holds is beside the point. */
export const checkNotificationPath = async ({
  identityNumber,
  origin,
  accountPrincipal,
  actor,
  browser,
  onPastLimit,
  signal,
  renewSubscription = resubscribe,
}: {
  identityNumber: bigint;
  origin: string;
  accountPrincipal: Principal;
  actor: IdentityCalls;
  browser: BrowserCalls;
  onPastLimit: () => void;
  signal: AbortSignal;
  /** Replaces this browser's own subscription without a prompt, for the identity named. */
  renewSubscription?: (
    identityNumber: bigint,
    signal: AbortSignal,
  ) => Promise<void>;
}): Promise<PathCheckOutcome> => {
  const consented = await actor.notification_consent_granted({
    anchor_number: identityNumber,
    origin,
  });
  if (!consented) {
    return working;
  }
  const installedApps: NotificationPath = {
    InstalledApps: { origin, account_principal: accountPrincipal },
  };
  const thisBrowser: NotificationPath = { ThisBrowser: null };

  const installedAppsStatus = await pathStatus(
    browser,
    identityNumber,
    installedApps,
  );
  if (installedAppsStatus.subscriptions.length > 0) {
    if (!isPastLimit(installedAppsStatus)) {
      return working;
    }
    onPastLimit();
    const answer = await probe(browser, identityNumber, installedApps, signal);
    if (answer === "answered") {
      return working;
    }
    signal.throwIfAborted();
    await browser
      .remove_unanswered_pwa_webpush_subscriptions({
        anchor_number: identityNumber,
        origin,
        account_principal: accountPrincipal,
      })
      .then(throwCanisterError);
    const remaining = await pathStatus(browser, identityNumber, installedApps);
    if (remaining.subscriptions.length > 0) {
      return working;
    }
    // This browser's own path carries the app's notifications from here on, whatever its count says.
    return afterOwnPath(
      await probe(browser, identityNumber, thisBrowser, signal),
      browser,
      identityNumber,
      renewSubscription,
      signal,
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
    browser,
    identityNumber,
    renewSubscription,
    signal,
  );
};

/** What is left once this browser's own path was probed. */
const afterOwnPath = async (
  answer: ProbeAnswer,
  browser: BrowserCalls,
  identityNumber: bigint,
  renewSubscription: (
    identityNumber: bigint,
    signal: AbortSignal,
  ) => Promise<void>,
  signal: AbortSignal,
): Promise<PathCheckOutcome> => {
  switch (answer) {
    case "answered":
      return working;
    // A push service that called the subscription gone left it as broken as one that never
    // answered, and the same repair applies.
    case "nothing-on-path":
    case "unanswered":
      break;
  }
  // The Home Screen app this browser routes through is gone, and only installing it again brings it back.
  if (notificationsNeedInstallHere()) {
    // Switched off before the install is offered, as an unanswered enable probe is: the
    // canister refuses to replace an app that is on until the probe's whole window has
    // passed, which is longer than the check waits.
    await switchOffApp(browser, identityNumber);
    return { kind: "ask", installFirst: true };
  }
  if (
    typeof Notification === "undefined" ||
    Notification.permission !== "granted"
  ) {
    return { kind: "ask", installFirst: false };
  }
  await renewSubscription(identityNumber, signal);
  return working;
};

/** Switches off delivery through the app this browser points at. A browser that points
 *  at none has nothing to switch off. */
const switchOffApp = async (
  browser: BrowserCalls,
  identityNumber: bigint,
): Promise<void> => {
  try {
    await browser
      .disable_notification_app({ anchor_number: identityNumber })
      .then(throwCanisterError);
  } catch (error) {
    if (
      isCanisterError<DisableNotificationAppError>(error) &&
      error.type === "NoNotificationApp"
    ) {
      return;
    }
    throw error;
  }
};

/** Drops the subscription that stopped answering and registers a fresh one, which needs
 *  no prompt where the permission is held. */
const resubscribe = async (
  identityNumber: bigint,
  signal: AbortSignal,
): Promise<void> => {
  const subscription = await currentDeviceSubscription();
  // The subscription is the whole browser's, so nothing is dropped or registered for an
  // identity the user switched away from while it was looked up.
  signal.throwIfAborted();
  await subscription?.unsubscribe();
  signal.throwIfAborted();
  await ensureRegisteredDevice(identityNumber);
};

const pathStatus = (
  browser: BrowserCalls,
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

/** Every subscription on the path had its probe fetched: a remove clears the probe
 *  record, and nothing else does. */
const isAnswered = (status: NotificationPathStatus): boolean =>
  status.subscriptions.every(
    ({ probe_sent_at_ns }) => probe_sent_at_ns.length === 0,
  );

/** The least time a probe the push service accepted has to be answered before its
 *  silence counts: the canister's `MIN_PROBE_ANSWER_TIME_NS` in `webpush/unanswered.rs`,
 *  which refuses to remove an installed app whose probe is younger. */
const MIN_PROBE_ANSWER_TIME_MS = 10_000;

/** Whether every probe on `status`'s path has had {@link MIN_PROBE_ANSWER_TIME_MS} by
 *  now. */
const hasEveryProbeHadItsAnswerTime = (
  status: NotificationPathStatus,
): boolean =>
  status.subscriptions.every(
    ({ probe_sent_at_ns: [sentAtNs] }) =>
      sentAtNs === undefined ||
      Date.now() - Number(sentAtNs / BigInt(1_000_000)) >=
        MIN_PROBE_ANSWER_TIME_MS,
  );

/** Sends the probe and waits for every subscription on the path to fetch it: at least
 *  {@link PROBE_WAIT_MS}, and past it until every probe has had
 *  {@link MIN_PROBE_ANSWER_TIME_MS}, or until `signal` is aborted. A probe still too
 *  young when that runs out is no evidence either way, so it reads as answered and
 *  nothing is repaired for it. */
const probe = async (
  browser: BrowserCalls,
  identityNumber: bigint,
  path: NotificationPath,
  signal: AbortSignal,
): Promise<ProbeAnswer> => {
  // A probe is a notification the device shows, so none goes out for an identity the
  // user has left, however late the lookup before it answered.
  signal.throwIfAborted();
  try {
    await browser
      .probe_notification_path({ anchor_number: identityNumber, path })
      .then(throwCanisterError);
  } catch (error) {
    if (
      isCanisterError<ProbeNotificationPathError>(error) &&
      error.type === "NotReachable"
    ) {
      // Asked after the call, as after every poll: the answer may belong to the identity left behind.
      signal.throwIfAborted();
      return "nothing-on-path";
    }
    throw error;
  }
  const deadline = Date.now() + PROBE_WAIT_MS;
  const latest = deadline + MIN_PROBE_ANSWER_TIME_MS;
  while (Date.now() < latest) {
    await new Promise((resolve) => setTimeout(resolve, POLL_INTERVAL_MS));
    signal.throwIfAborted();
    const status = await pathStatus(browser, identityNumber, path);
    // Asked again after the query, so an answer that arrived after a switch is not
    // acted on for the identity left behind.
    signal.throwIfAborted();
    // A push service that called the subscription gone took it off the path.
    if (status.subscriptions.length === 0) {
      return "nothing-on-path";
    }
    if (isAnswered(status)) {
      return "answered";
    }
    if (Date.now() >= deadline && hasEveryProbeHadItsAnswerTime(status)) {
      return "unanswered";
    }
  }
  return "answered";
};
