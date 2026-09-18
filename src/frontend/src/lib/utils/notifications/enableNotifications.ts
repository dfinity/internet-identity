// Opt-in orchestration: ask for permission, subscribe the device with a fresh
// VAPID key + signed JWT pool, then record consent for the app. Subscribing
// first so a refusal at the browser prompt leaves no consent row behind for a
// browser that cannot receive anything.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  NotificationError,
  _SERVICE,
} from "$lib/generated/internet_identity_types";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
import { awaitSessionCreation } from "$lib/stores/sessionCreation.store";
import { mintApplicationSession } from "./mintApplicationSession";
import { requestNotificationPermission } from "./pushSubscription";
import { ensureRegisteredDevice } from "./subscribeDevice";

type GrantArgs = {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
};

export type EnableNotificationsResult = {
  /** `dismissed` is a prompt closed without an answer, which leaves the permission
   * at `default` and can be asked again; `denied` cannot, and needs unblock steps. */
  status: "enabled" | "denied" | "dismissed";
};

export const enableNotifications = async ({
  identityNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<EnableNotificationsResult> => {
  const permission = await requestNotificationPermission();
  if (permission !== "granted") {
    return { status: permission === "denied" ? "denied" : "dismissed" };
  }

  await ensureRegisteredDevice(identityNumber, actor);
  await grantConsent({ identityNumber, origin, actor });

  return { status: "enabled" };
};

/**
 * Records consent for an app without touching the subscription. For a browser
 * that is already subscribed and only needs to allow one more app, so there is
 * no permission prompt and no new endpoint.
 */
export const allowApp = ({
  identityNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> => grantConsent({ identityNumber, origin, actor });

/**
 * Records consent, signing in at the app first where the identity has never reached it.
 *
 * Asks and reacts to `SessionMissing` rather than ensuring a session up front, so it
 * never touches an app the identity already holds a session at: a second session there
 * would drop the one the app is still using.
 */
const grantConsent = async (args: GrantArgs): Promise<void> => {
  if (await granted(args)) {
    return;
  }

  // A sign-in already under way is about to mint the application, so waiting for it
  // leaves the session the app asked for in place. Asked again only where there was
  // something to wait for.
  if (await awaitSessionCreation(args.origin)) {
    if (await granted(args)) {
      return;
    }
  }

  // Nothing else is going to mint it: a consent asked for on its own, or one that came in
  // alongside a delegation method that creates no session.
  await mintApplicationSession(args);
  await args.actor
    .notification_grant_consent(args.identityNumber, args.origin)
    .then(throwCanisterError);
};

/**
 * Whether the grant landed. `false` only where the identity has never reached the app,
 * which is the one refusal a sign-in fixes; every other error belongs to the caller.
 */
const granted = ({
  identityNumber,
  origin,
  actor,
}: GrantArgs): Promise<boolean> =>
  actor
    .notification_grant_consent(identityNumber, origin)
    .then(throwCanisterError)
    .then(() => true)
    .catch((error: unknown) => {
      if (
        isCanisterError<NotificationError>(error) &&
        error.type === "SessionMissing"
      ) {
        return false;
      }
      throw error;
    });
