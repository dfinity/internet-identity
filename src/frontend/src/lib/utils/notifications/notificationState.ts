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

/**
 * Whether a Home Screen app this browser installed can be delivered to.
 *
 * The app has an entry of its own, so `get_webpush_subscription_status` cannot see it:
 * that answers for the caller, and the caller here is the browser. This is the read that
 * tells an install that finished from one that was never done.
 *
 * `false` where the canister could not be asked, which is the same answer a browser that
 * has linked nothing gets: either way there is an install to offer.
 */
export const notificationAppDelivers = async (
  identityNumber: bigint,
): Promise<boolean> => {
  try {
    const actor = await browserKeyActor(identityNumber);
    const [status] = await actor.get_notification_app_status({
      anchor_number: identityNumber,
    });
    return status?.subscribed === true;
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return false;
    }
    console.error(error);
    return false;
  }
};

/** iOS has no notifications yet: Safari delivers them only to a Home Screen app, and
 *  that is not built. Nothing is offered there and no app is told otherwise. */
export const notificationsUnavailableHere = (): boolean => {
  const { os } = browserAndSystem();
  return "Ios" in os || "Ipados" in os;
};

/**
 * Whether notifications here have to go through a Home Screen app first.
 *
 * iOS delivers web push only to an installed app, so Safari cannot subscribe and
 * answering here means sending the user through the install instead of raising a
 * prompt. The Home Screen app itself runs standalone and is served the same page, so
 * it is excluded: inside it, notifications are ordinary.
 */
export const notificationsNeedInstallHere = (): boolean => {
  if (!notificationsUnavailableHere()) {
    return false;
  }
  const legacy = (navigator as { standalone?: boolean }).standalone;
  if (legacy === true) {
    return false;
  }
  // Asked of the document, which not every context this module loads in has: the
  // service worker imports from here and has no `window` at all.
  const display =
    typeof window !== "undefined" && typeof window.matchMedia === "function"
      ? window.matchMedia("(display-mode: standalone)").matches
      : false;
  return !display;
};

export type OptInScreen = "enable" | "skip";

/** Nothing to ask, so the answer is already known, or the question and what answering
 *  it still has to do. */
export type OptInResolution =
  | { screen: "skip"; granted: boolean }
  | {
      screen: "enable";
      state: DeviceNotificationState;
      consented: boolean;
      /** Allow sends the user through the Home Screen install rather than raising a
       *  prompt, because this browser has no way to subscribe. The screen is the same
       *  one either way; only what answering it does differs. */
      installFirst: boolean;
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
  // Where the answer is a Home Screen app, delivery is the app's and not this
  // browser's, so it is read from the entry this browser linked. Both halves still
  // have to hold: the identity allowed the app, and an app exists that can be
  // delivered to. Consent alone would tell an app yes while nothing reached the user,
  // and would never offer the install to an identity that allowed notifications before
  // any of this existed.
  const installFirst = notificationsNeedInstallHere();
  if (installFirst) {
    const delivers = await notificationAppDelivers(identityNumber);
    return consented && delivers
      ? { screen: "skip", granted: true }
      : { screen: "enable", state, consented, installFirst };
  }

  const screen = resolveOptInScreen(state, consented);
  return screen === "skip"
    ? { screen, granted: consented && deliversHere(state) }
    : { screen, state, consented, installFirst };
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
  // A refused permission is asked for here too. No prompt can lift a refusal, so
  // answering leads to the unblock guidance rather than to a prompt, and the user
  // reaches that guidance from the screen that explains why they are being asked.
  return "enable";
};

/**
 * Calls back once this browser's notification permission stops being refused.
 *
 * The unblock steps send the user into browser settings, and nothing in the page can
 * raise the prompt again, so the screen waits for the setting itself to change rather
 * than for the user to come back and press something.
 *
 * `PermissionStatus` reports the change where the browser delivers it, which is
 * immediate and covers a toggle thrown in a site-settings bubble over the page. Not
 * every browser delivers it for notifications, and a settings window on another
 * screen may never return focus here, so the permission is also read on a timer.
 *
 * Returns the function that stops watching, which also runs before the callback.
 */
export const watchNotificationPermission = (
  onAllowed: () => void,
): (() => void) => {
  if (typeof Notification === "undefined") {
    return () => {};
  }
  let stopped = false;
  let detach = () => {};

  const check = () => {
    if (stopped || Notification.permission === "denied") {
      return;
    }
    stop();
    onAllowed();
  };

  const timer = setInterval(check, PERMISSION_POLL_MS);

  const stop = () => {
    stopped = true;
    clearInterval(timer);
    detach();
  };

  void navigator.permissions
    ?.query({ name: "notifications" as PermissionName })
    .then((status) => {
      if (stopped) {
        return;
      }
      status.addEventListener("change", check);
      detach = () => status.removeEventListener("change", check);
    })
    // Not every browser knows the `notifications` permission name, and the ones that
    // do not reject the query. The timer covers them.
    .catch(() => undefined);

  return stop;
};

const PERMISSION_POLL_MS = 1000;
