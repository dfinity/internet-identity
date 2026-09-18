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
  /** The canister holds that subscription under this identity. A second identity
   * on a subscribed browser is not registered until it registers for itself. */
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
    registered: subscribed && (await hasCanisterRow(identityNumber, actor)),
  };
};

const hasCanisterRow = async (
  identityNumber: bigint,
  actor: ActorSubclass<_SERVICE>,
): Promise<boolean> => {
  const browserId = await currentBrowserId(identityNumber);
  if (browserId === undefined) {
    return false;
  }
  const [status] = await actor.webpush_jwt_pool_status(
    identityNumber,
    browserId,
  );
  return status !== undefined;
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
  if (state.registered && !allowed) {
    return "allow-app";
  }
  if (!state.registered && allowed) {
    return "new-device";
  }
  return "first-time";
};
