/**
 * What Internet Identity's own notifications say.
 *
 * The canister sends the event rather than the sentence: the words belong here, where
 * the translations are, and rewording one costs no canister upgrade. It carries no text
 * for the same reason an app's notification carries none, which is that nothing of the
 * user's goes through the push service.
 */
import type { NotificationEvent } from "$lib/generated/internet_identity_types";

/** Where one of these opens, which is the app itself: what it is about is here, and
 *  there is no app of the user's behind it to send them to. */
export const NOTIFICATION_APP_PATH = "/ios-icp-alerts";

export interface IdentityNotificationText {
  title: string;
  body: string;
}

export const identityNotificationText = (
  event: NotificationEvent,
): IdentityNotificationText => {
  if ("NotificationsOn" in event) {
    return {
      title: "Notifications are on",
      // Which identity, because this device may carry several and the user turned this
      // one on somewhere else.
      body: `This device now receives notifications for identity ${event.NotificationsOn}.`,
    };
  }
  // Every variant has an answer, and a build that has not caught up with the canister
  // still owes the user something: the notification was woken for, and a worker that
  // shows nothing is one the browser answers for.
  return {
    title: "Internet Identity",
    body: "Something about your identity changed.",
  };
};
