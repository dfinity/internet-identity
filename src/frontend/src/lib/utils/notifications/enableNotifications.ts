// Opt-in orchestration: ask for permission, subscribe the device with a fresh
// VAPID key + signed JWT pool, then record consent for the app. Subscribing
// first so a refusal at the browser prompt leaves no consent row behind for a
// browser that cannot receive anything.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  NotificationGrantConsentError,
  _SERVICE,
} from "$lib/generated/internet_identity_types";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";
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

  await ensureRegisteredDevice(identityNumber);
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
 * Asks and reacts to `NoSuchSession` rather than ensuring a session up front, so it
 * never touches an app the identity already holds a session at: a second session there
 * would drop the one the app is still using.
 *
 * Needs no coordination with the sign-in that a delegation request performs, because
 * `serializeAuthorizationRequest` runs those one at a time and the consent handler holds
 * that queue for the whole ceremony, this call included. So a sign-in has either already
 * finished, in which case the app is reached and the grant above succeeds, or it is
 * queued behind us and replaces what we mint with the session the app asked for.
 */
const grantConsent = async (args: GrantArgs): Promise<void> => {
  if (await granted(args)) {
    return;
  }

  await mintApplicationSession(args);
  await args.actor
    .notification_grant_consent({
      anchor_number: args.identityNumber,
      origin: args.origin,
    })
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
    .notification_grant_consent({ anchor_number: identityNumber, origin })
    .then(throwCanisterError)
    .then(() => true)
    .catch((error: unknown) => {
      if (
        isCanisterError<NotificationGrantConsentError>(error) &&
        error.type === "NoSuchSession"
      ) {
        return false;
      }
      throw error;
    });
