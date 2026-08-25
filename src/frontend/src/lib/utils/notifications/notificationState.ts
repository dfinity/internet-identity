// Reads this browser's notification state and picks which opt-in screen to show.
// Consent is per identity, the subscription is per browser, so the screen turns
// on the combination: a first-timer gets the full pitch, an already-set-up
// browser only needs the app's consent, a blocked browser gets guidance.

import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import { loadVapidKey } from "./vapidKeyStore";
import { wasDeclinedRecently } from "./notificationDiagnostics";

export interface DeviceNotificationState {
  supported: boolean;
  permission: NotificationPermission;
  /** This browser holds a subscription the canister knows: the live endpoint
   * matches the signing key we still hold. */
  subscribed: boolean;
}

export const readDeviceState = async (): Promise<DeviceNotificationState> => {
  const supported = isPushSupported();
  const permission =
    typeof Notification !== "undefined" ? Notification.permission : "denied";
  if (!supported) {
    return { supported, permission, subscribed: false };
  }
  const subscription = await currentDeviceSubscription();
  const stored = await loadVapidKey();
  const subscribed =
    subscription !== undefined &&
    stored !== undefined &&
    stored.endpoint === subscription.endpoint;
  return { supported, permission, subscribed };
};

export type OptInScreen =
  | "first-time"
  | "allow-app"
  | "new-device"
  | "blocked"
  | "skip";

/** Picks the opt-in screen for `origin` from device state and existing consent. */
export const resolveOptInScreen = (
  state: DeviceNotificationState,
  origin: string,
  consentedOrigins: string[],
): OptInScreen => {
  if (!state.supported) {
    return "skip";
  }
  const allowed = consentedOrigins.includes(origin);
  if (state.permission === "granted" && state.subscribed && allowed) {
    return "skip";
  }
  if (wasDeclinedRecently(origin)) {
    return "skip";
  }
  if (state.permission === "denied") {
    return "blocked";
  }
  if (state.subscribed && !allowed) {
    return "allow-app";
  }
  if (!state.subscribed && allowed) {
    return "new-device";
  }
  return "first-time";
};
