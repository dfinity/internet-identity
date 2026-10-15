// Opt-in orchestration: ask for permission where it is not granted, subscribe and
// register the device where it is not registered, then record consent where the app
// does not have it. Each step is skipped when the resolved state says it is already
// done, so an app allowed elsewhere only costs this browser its registration and a
// browser already set up only costs the consent.
//
// Subscribing before recording consent, so a refusal at the browser prompt leaves no
// consent row behind for a browser that cannot receive anything.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
import { requestNotificationPermission } from "./pushSubscription";
import { ensureRegisteredDevice } from "./subscribeDevice";
import { claimDeviceApp } from "./deviceApp";
import { notificationsNeedInstallHere } from "./notificationState";
import { browserAppStatus } from "../../../routes/(new-styling)/ios-icp-alerts/appStatus";
import type { DeviceNotificationState } from "./notificationState";

type GrantArgs = {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
};

export type EnableNotificationsResult = {
  /** `dismissed` is a prompt closed without an answer, which leaves the permission at
   * `default` and can be asked again; `denied` cannot, and needs unblock steps.
   * `install-required` is a device whose notifications arrive in a Home Screen app that
   * is not there, which is the install steps rather than anything this can do. */
  status: "enabled" | "denied" | "dismissed" | "install-required";
};

export const turnOnNotifications = async ({
  identityNumber,
  origin,
  actor,
  device,
  consented,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
  /** This browser as the opt-in resolved it, which is what each step reads to know
   *  whether it has anything to do. */
  device: DeviceNotificationState;
  consented: boolean;
}): Promise<EnableNotificationsResult> => {
  // A browser that cannot hold a subscription has nothing to prompt for: what reaches
  // the user is the app this device has, and pointing this identity at it is the whole
  // of turning notifications on here.
  if (notificationsNeedInstallHere()) {
    const app =
      (await browserAppStatus(identityNumber)) ??
      ((await claimDeviceApp(identityNumber))
        ? await browserAppStatus(identityNumber)
        : undefined);
    if (app?.subscribed !== true) {
      return { status: "install-required" };
    }
    if (!consented) {
      await grantConsent({ identityNumber, origin, actor });
    }
    return { status: "enabled" };
  }

  if (device.permission !== "granted") {
    const permission = await requestNotificationPermission();
    if (permission !== "granted") {
      return { status: permission === "denied" ? "denied" : "dismissed" };
    }
  }

  if (!device.registered) {
    // Subscribing drops the endpoint every other identity here is registered with, so
    // this is the one call that must not run for a browser already registered.
    await ensureRegisteredDevice(identityNumber);
  }

  if (!consented) {
    await grantConsent({ identityNumber, origin, actor });
  }

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
