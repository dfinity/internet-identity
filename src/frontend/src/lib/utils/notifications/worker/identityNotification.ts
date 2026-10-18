/** The canister sends the event rather than the sentence: the words belong here, where the translations are, and nothing of the user's goes through the push service. */
import type { NotificationEvent } from "$lib/generated/internet_identity_types";

/** Fails this build where the canister's type gains a variant, so the words for it are written here rather than discovered on a phone. */
const noWordsWrittenFor = (event: never): void => void event;

export const identityNotificationText = (
  event: NotificationEvent,
): { title: string; body: string } => {
  if ("NotificationsOn" in event) {
    const { anchor_number, name } = event.NotificationsOn;
    // Named, because nobody recognises an identity by its number.
    const identity = name[0] ?? `identity ${anchor_number}`;
    return {
      title: "Notifications are on",
      body: `This device now receives notifications for ${identity}.`,
    };
  }
  // The assertion and the sentence both: the sentence is what a build older than the canister owes the user, since a worker that shows nothing is one the browser answers for.
  noWordsWrittenFor(event);
  return {
    title: "Internet Identity",
    body: "Something about your identity changed.",
  };
};
