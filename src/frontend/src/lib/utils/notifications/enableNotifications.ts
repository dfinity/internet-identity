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
