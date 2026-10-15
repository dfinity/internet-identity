/**
 * What the canister says about this device's Home Screen app, asked by whoever is asking.
 *
 * The browser asks about its own entry, signing with its browser key, because the install
 * page is a tab of its own with no session. The app asks which identities point at it,
 * signing as itself, because nothing in its storage knows: a browser can point another
 * identity at it while it is not running.
 */
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  NotificationAppInfo,
} from "$lib/generated/internet_identity_types";
import {
  browserKeyActor,
  UnregisteredBrowserError,
} from "$lib/utils/notifications/browserActor";

/**
 * What this browser's entry says about the app it points at, or `undefined` where it
 * points at nothing and where the canister could not be asked.
 *
 * Both answers mean the same thing to the page: there is an install to offer.
 */
export const browserAppStatus = async (
  identityNumber: bigint,
): Promise<NotificationAppInfo | undefined> => {
  try {
    const actor = await browserKeyActor(identityNumber);
    const [status] = await actor.get_notification_app_status({
      anchor_number: identityNumber,
    });
    return status;
  } catch (error) {
    if (!(error instanceof UnregisteredBrowserError)) {
      console.error(error);
    }
    return undefined;
  }
};

/**
 * The identities this app delivers for.
 *
 * Empty where its record is gone and where it never had one, which the app answers the
 * same way: there is nothing here, and the user is sent back to their browser.
 */
export const appAnchors = async (
  actor: ActorSubclass<_SERVICE>,
): Promise<bigint[]> => Array.from(await actor.notification_app_anchors());
