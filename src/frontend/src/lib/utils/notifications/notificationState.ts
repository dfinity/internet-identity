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
import { recordPermission } from "./notificationDiagnostics";
import { browserAndSystem } from "$lib/utils/describeBrowser";
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

/**
 * Whether notifications reach this identity in this browser right now.
 *
 * The permission as well as the registration: a permission reset to "default" leaves
 * the rows in place while the browser shows nothing, so a registration alone is not
 * delivery. `registered` already implies supported and subscribed.
 */
const deliversHere = (state: DeviceNotificationState): boolean =>
  state.permission === "granted" && state.registered;

/** iOS has no notifications yet: Safari delivers them only to a Home Screen app, and
 *  that is not built. Nothing is offered there and no app is told otherwise. */
export const notificationsUnavailableHere = (): boolean => {
  const { os } = browserAndSystem();
  return "Ios" in os || "Ipados" in os;
};

export type OptInScreen = "enable" | "blocked" | "skip";

/** What the opt-in asks, where there is anything to ask. */
export type OptInQuestion = Exclude<OptInScreen, "skip">;

/** Nothing to ask, so the answer is already known, or a question and what answering
 *  it still has to do. */
export type OptInResolution =
  | { screen: "skip"; granted: boolean }
  | {
      screen: OptInQuestion;
      state: DeviceNotificationState;
      consented: boolean;
    };

/**
 * Which screen this app and identity need.
 *
 * The app's consent and this browser's state are independent, so they are read at
 * once; only the registration has to wait, since it is read against the endpoint the
 * browser turns out to hold. A failure is left to the caller: it shows the same toast
 * whenever it happens, and the screen it lands on is the one that asks.
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
  const [consented, browserState] = await Promise.all([
    actor
      .notification_consent_granted({ anchor_number: identityNumber, origin })
      .catch(() => false),
    browser,
  ]);
  // A browser we could not read is not one with nothing to ask. Offer the question
  // and let answering it read everything again.
  const state =
    browserState === undefined
      ? {
          supported: true,
          permission: "default" as NotificationPermission,
          subscribed: false,
          registered: false,
        }
      : await readDeviceState(identityNumber, browserState);
  recordPermission(state.permission);
  const screen = resolveOptInScreen(state, consented);
  return screen === "skip"
    ? { screen, granted: consented && deliversHere(state) }
    : { screen, state, consented };
};

/**
 * What an app is told: this identity allowed it, and this browser delivers.
 *
 * Read again rather than carried over from the resolution, because the user has
 * acted on the screen since.
 */
export const readGranted = async ({
  identityNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<boolean> => {
  const browserState = await readBrowserPushState();
  if (browserState === undefined) {
    return false;
  }
  const [consented, state] = await Promise.all([
    actor
      .notification_consent_granted({ anchor_number: identityNumber, origin })
      .catch(() => false),
    readDeviceState(identityNumber, browserState),
  ]);
  return consented && deliversHere(state);
};

/** Picks the opt-in screen from device state and existing consent. */
export const resolveOptInScreen = (
  state: DeviceNotificationState,
  allowed: boolean,
): OptInScreen => {
  if (!state.supported) {
    return "skip";
  }
  if (allowed && deliversHere(state)) {
    return "skip";
  }
  // A refusal stands until the user changes it in browser settings, which no prompt
  // can do, so this asks for that instead of for a permission it cannot get.
  if (state.permission === "denied") {
    return "blocked";
  }
  return "enable";
};
