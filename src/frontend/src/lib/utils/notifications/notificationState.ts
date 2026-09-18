// Reads this browser's notification state and picks which opt-in screen to show.
// Consent is per identity, the subscription is per browser, so the screen turns
// on the combination: a first-timer gets the full pitch, an already-set-up
// browser only needs the app's consent, a blocked browser gets guidance.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import { loadVapidKey } from "./vapidKeyStore";
import { wasDeclinedRecently } from "./notificationDiagnostics";
import { currentBrowserId } from "$lib/stores/browser-key.store";

export interface DeviceNotificationState {
  supported: boolean;
  permission: NotificationPermission;
  /** This browser holds a live subscription for the signing key we still keep. */
  subscribed: boolean;
  /** The canister holds that same subscription under this identity. A second identity
   * on a subscribed browser is not registered until it registers for itself, and one
   * another identity's re-subscribe left behind is registered on a dead endpoint. */
  registered: boolean;
}

export const readDeviceState = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<DeviceNotificationState> => {
  const supported = isPushSupported();
  const permission =
    typeof Notification !== "undefined" ? Notification.permission : "denied";
  if (!supported) {
    return { supported, permission, subscribed: false, registered: false };
  }
  const subscription = await currentDeviceSubscription();
  const stored = await loadVapidKey();
  const subscribed =
    subscription !== undefined &&
    stored !== undefined &&
    stored.endpoint === subscription.endpoint;
  return {
    supported,
    permission,
    subscribed,
    registered:
      subscribed &&
      (await isRegisteredHere(identityNumber, actor, stored.endpoint)),
  };
};

/**
 * Whether the canister holds this identity's registration on the endpoint the browser
 * is actually subscribed with. A registration naming any other endpoint is one another
 * identity's re-subscribe left behind, and reaches nothing.
 */
const isRegisteredHere = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
  endpoint: string,
): Promise<boolean> => {
  const browserId = await currentBrowserId(identityNumber);
  if (browserId === undefined) {
    return false;
  }
  const [status] = await actor.get_webpush_subscription_status({
    anchor_number: identityNumber,
    browser_id: browserId,
  });
  return status?.endpoint === endpoint;
};

export type OptInScreen =
  "first-time" | "allow-app" | "new-device" | "blocked" | "skip";

/** Picks the opt-in screen for `origin` from device state and existing consent. */
export const resolveOptInScreen = (
  state: DeviceNotificationState,
  origin: string,
  allowed: boolean,
): OptInScreen => {
  if (!state.supported) {
    return "skip";
  }
  if (state.permission === "granted" && state.registered && allowed) {
    return "skip";
  }
  if (wasDeclinedRecently(origin)) {
    return "skip";
  }
  if (state.permission === "denied") {
    return "blocked";
  }
  // Only where the browser can already deliver: this screen asks for the app's
  // consent and nothing else, so a permission that was reset to "default" has to
  // fall through to one that asks for it back.
  if (state.permission === "granted" && state.registered && !allowed) {
    return "allow-app";
  }
  if (!state.registered && allowed) {
    return "new-device";
  }
  return "first-time";
};
