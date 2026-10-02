// Reads this browser's notification state and picks which opt-in screen to show.
// Consent is per identity, the subscription is per browser, so the screen turns
// on the combination: a first-timer gets the full pitch, an already-set-up
// browser only needs the app's consent, a blocked browser gets guidance.
//
// Split in two around what each half needs: the browser's own capability and
// subscription need no identity and are read as soon as a request arrives, while
// the registration and the app's consent are read once an identity is chosen.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { currentDeviceSubscription, isPushSupported } from "./pushSubscription";
import { loadVapidKey } from "./vapidKeyStore";
import {
  recordFailure,
  recordPermission,
  wasDeclinedRecently,
} from "./notificationDiagnostics";
import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";

/** What this browser can do, and what it is subscribed with. */
export interface BrowserPushState {
  supported: boolean;
  permission: NotificationPermission;
  /** The endpoint the browser and the signing key we kept agree on, or `undefined`
   * where there is no live subscription to the key we hold. */
  endpoint?: string;
}

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

/**
 * This browser's push state, or `undefined` where it could not be read.
 *
 * Answers rather than throws, because the caller starts this before it has anything
 * to show and a rejection there has no screen to land on. A state that reads as
 * unsupported would quietly skip the opt-in instead, so a failure keeps its own value.
 */
export const readBrowserPushState = async (): Promise<
  BrowserPushState | undefined
> => {
  try {
    const supported = isPushSupported();
    const permission =
      typeof Notification !== "undefined" ? Notification.permission : "denied";
    if (!supported) {
      return { supported, permission };
    }
    const subscription = await currentDeviceSubscription();
    const stored = await loadVapidKey();
    const live =
      subscription !== undefined &&
      stored !== undefined &&
      stored.endpoint === subscription.endpoint;
    return {
      supported,
      permission,
      endpoint: live ? stored.endpoint : undefined,
    };
  } catch (error) {
    console.error(error);
    return undefined;
  }
};

export const readDeviceState = async (
  identityNumber: bigint,
  browser: BrowserPushState,
): Promise<DeviceNotificationState> => {
  const { supported, permission, endpoint } = browser;
  if (!supported || endpoint === undefined) {
    return { supported, permission, subscribed: false, registered: false };
  }
  return {
    supported,
    permission,
    subscribed: true,
    registered: await isRegisteredHere(identityNumber, endpoint),
  };
};

/**
 * Whether the canister holds this identity's registration on the endpoint the browser
 * is actually subscribed with. A registration naming any other endpoint is one another
 * identity's re-subscribe left behind, and reaches nothing.
 */
const isRegisteredHere = async (
  identityNumber: bigint,
  endpoint: string,
): Promise<boolean> => {
  // Signed as the browser, which is what the canister reads the registration off, so
  // a browser no sign-in has registered holds nothing to find.
  let actor;
  try {
    actor = await browserKeyActor(identityNumber);
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return false;
    }
    throw error;
  }
  const [status] = await actor.get_webpush_subscription_status({
    anchor_number: identityNumber,
  });
  return status?.endpoint === endpoint;
};

export type OptInScreen =
  "first-time" | "allow-app" | "new-device" | "blocked" | "skip";

/** What the opt-in screen asks, where there is anything to ask at all. */
export type OptInQuestion = Exclude<OptInScreen, "skip"> | "failed";

/** Nothing to ask, so the answer is already known, or a question to put on screen. */
export type OptInResolution =
  { screen: "skip"; consented: boolean } | { screen: OptInQuestion };

/**
 * Which screen this app and identity need, resolved before anything renders.
 *
 * The app's consent and this browser's state are independent, so they are read at
 * once; only the registration has to wait, since it is read against the endpoint the
 * browser turns out to hold.
 */
export const resolveOptIn = async ({
  identityNumber,
  origin,
  actor,
  browser,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
  browser: Promise<BrowserPushState | undefined>;
}): Promise<OptInResolution> => {
  try {
    const [consented, browserState] = await Promise.all([
      actor
        .notification_consent_granted({ anchor_number: identityNumber, origin })
        .catch(() => false),
      browser,
    ]);
    if (browserState === undefined) {
      recordFailure("subscribe-failed", "could not read this browser's state");
      return { screen: "failed" };
    }
    const state = await readDeviceState(identityNumber, browserState);
    recordPermission(state.permission);
    const screen = resolveOptInScreen(state, origin, consented);
    return screen === "skip" ? { screen, consented } : { screen };
  } catch (error) {
    // The request is waiting on this window, so a rejection here has to land on a
    // screen the user can answer rather than on nothing at all.
    recordFailure(
      "subscribe-failed",
      error instanceof Error ? error.message : String(error),
    );
    return { screen: "failed" };
  }
};

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
