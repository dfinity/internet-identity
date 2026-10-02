// Opt-in orchestration: ask for permission, subscribe the device with a fresh
// VAPID key + signed JWT pool, then record consent for the app. Subscribing
// first so a refusal at the browser prompt leaves no consent row behind for a
// browser that cannot receive anything.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
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
 * Records consent for an app without touching any subscription, so there is no
 * permission prompt and no new endpoint. Consent belongs to the identity, so it
 * reaches every browser already registered for notifications.
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
 * Withdraws an app's consent, which also drops what it has queued for every browser.
 * The subscription stays: it belongs to the browser, and the identity's other apps
 * still reach it.
 */
export const disallowApp = ({
  identityNumber,
  origin,
  actor,
}: GrantArgs): Promise<void> =>
  actor
    .notification_revoke_consent({ anchor_number: identityNumber, origin })
    .then(throwCanisterError)
    .then(() => undefined);

/** Records consent for the app. The canister mints the application it hangs off. */
const grantConsent = ({
  identityNumber,
  origin,
  actor,
}: GrantArgs): Promise<void> =>
  actor
    .notification_grant_consent({ anchor_number: identityNumber, origin })
    .then(throwCanisterError)
    .then(() => undefined);
