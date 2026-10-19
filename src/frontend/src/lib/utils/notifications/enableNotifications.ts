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
import { claimDeviceApp, type ClaimOutcome } from "./deviceApp";
import { setAppEnabled } from "./appEnabled";
import { clearNotificationsOff } from "./turnedOff";
import { browserAppStatus } from "$lib/utils/notifications/appStatus";
import { turnOnAndProve } from "./turnOnAndProve";
import type { DeviceNotificationState } from "./notificationState";

type GrantArgs = {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
};

export type EnableNotificationsResult =
  | {
      /** `dismissed` is a prompt closed without an answer, which leaves the permission at `default` and can be asked again; `denied` cannot.
       * `install-required` is a device whose notifications arrive in a Home Screen app that is not there. */
      status: "enabled" | "denied" | "dismissed" | "install-required";
    }
  | {
      /** Nothing could be read about the app this device's notifications arrive in, so this is neither a yes nor a no. */
      status: "unanswered";
      reason: string;
    };

export const turnOnNotifications = async ({
  identityNumber,
  origin,
  actor,
  device,
  consented,
  justClaimed = false,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
  /** Whether the resolution pointed this browser at the device's app. That is evidence
   *  the app is there, so this press has nothing left to prove. */
  justClaimed?: boolean;
  /** This browser as the opt-in resolved it, which is what each step reads to know
   *  whether it has anything to do. */
  device: DeviceNotificationState;
  consented: boolean;
}): Promise<EnableNotificationsResult> => {
  // A browser that cannot hold a subscription has nothing to prompt for: subscribing would register an endpoint iOS never delivers to.
  if (device.source === "app") {
    return turnOnInApp({
      identityNumber,
      origin,
      actor,
      consented,
      justClaimed,
    });
  }

  if (device.permission !== "granted") {
    const permission = await requestNotificationPermission();
    if (permission !== "granted") {
      return { status: permission === "denied" ? "denied" : "dismissed" };
    }
  }

  // Whatever the switch in settings said, the user is asking for notifications here.
  await clearNotificationsOff(identityNumber);

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

/** An app another identity on this browser has is pointed at rather than installed again, but only where the canister says this browser points at no app. */
const turnOnInApp = async ({
  identityNumber,
  origin,
  actor,
  consented,
  justClaimed,
}: GrantArgs & {
  consented: boolean;
  justClaimed: boolean;
}): Promise<EnableNotificationsResult> => {
  let status = await browserAppStatus(identityNumber);
  let claim: ClaimOutcome = "no-donor";
  if (status.kind === "no-app") {
    // The claim is written before its reply arrives, so a lost reply must not deny an app this browser already points at.
    claim = await claimDeviceApp(identityNumber);
    status = await browserAppStatus(identityNumber);
  }

  switch (status.kind) {
    case "unanswered":
      return { status: "unanswered", reason: status.reason };
    // An install this browser handed a key to and nobody has opened leaves the same thing
    // to do as no install at all: walk the steps, which say to open it.
    case "awaiting-install":
    case "no-app":
      return { status: "install-required" };
    case "app": {
      const { subscribed, enabled } = status.app;
      // A record without a subscription is an app that has not been opened and allowed yet.
      if (!subscribed) {
        if (!enabled) {
          // Written here, where the user asked, and not left to whatever happens next: an
          // install walked from this press finishes by carrying the flag this entry holds,
          // so a flag still saying off would quietly outlive the answer they just gave.
          await setAppEnabled({ identityNumber, enabled: true });
        }
        return { status: "install-required" };
      }
      if (!enabled) {
        if (justClaimed || claim === "claimed") {
          // Another identity on this device has just handed the app over, here or in
          // the resolution that put this question on screen. That is this browser's own
          // evidence the app is there, and proving it again would stall the ceremony on
          // a probe whose answer is already known.
          await setAppEnabled({ identityNumber, enabled: true });
        } else if (!(await turnOnAndProve(identityNumber))) {
          // `subscribed` outlives the app it names: deleting it from the Home Screen
          // tells the canister nothing, so an entry says it delivers while nothing is
          // there to deliver to. The switch goes on only where the app answers for
          // itself, and the steps are what is left to offer where it does not.
          return { status: "install-required" };
        }
      }
      if (!consented) {
        await grantConsent({ identityNumber, origin, actor });
      }
      return { status: "enabled" };
    }
  }
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
