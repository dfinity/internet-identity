// Reads this browser's notification state and picks which opt-in screen to show.
// Consent is per identity, the subscription is per browser, so the screen turns
// on the combination: a first-timer gets the full pitch, an already-set-up
// browser only needs the app's consent, a blocked browser gets guidance.
//
// Split in two around what each half needs: the browser's own capability and
// subscription need no identity and are read as soon as a request arrives, while
// the registration and the app's consent are read once an identity is chosen.

import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  NotificationAppInfo,
} from "$lib/generated/internet_identity_types";
import { browserAppStatus } from "$lib/utils/notifications/appStatus";
import { claimDeviceApp, type ClaimOutcome } from "./deviceApp";
import { saidAlreadyInstalled } from "./alreadyInstalled";
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

/** `permission` is Safari's own word for what the user answered here, so it says something only on this path. */
export interface BrowserDeviceState {
  source: "browser";
  supported: boolean;
  permission: NotificationPermission;
  /** This browser holds a live subscription for the signing key we still keep. */
  subscribed: boolean;
  /** The canister holds that same subscription under this identity. A second identity
   * on a subscribed browser is not registered until it registers for itself, and one
   * another identity's re-subscribe left behind is registered on a dead endpoint. */
  registered: boolean;
}

/** Carries no permission: the one that decides delivery was given in a storage partition nothing here can read. */
export interface AppDeviceState {
  source: "app";
  /** The app holds a push subscription, which being opened and allowed there leaves behind. */
  subscribed: boolean;
  /** This identity has the app switched on. */
  enabled: boolean;
}

export type DeviceNotificationState = BrowserDeviceState | AppDeviceState;

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
): Promise<BrowserDeviceState> => {
  const { supported, permission, endpoint } = browser;
  if (!supported || endpoint === undefined) {
    return {
      source: "browser",
      supported,
      permission,
      subscribed: false,
      registered: false,
    };
  }
  return {
    source: "browser",
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

/** A permission reset to "default" leaves the rows in place, so a registration alone is not delivery. */
const deliversHere = (state: DeviceNotificationState): boolean =>
  state.source === "app"
    ? state.subscribed && state.enabled
    : state.permission === "granted" && state.registered;

/** iOS delivers web push only to a Home Screen app, so the browser's own permission and subscription say nothing. */
export const notificationsNeedInstallHere = (): boolean => {
  const { os } = browserAndSystem();
  return "Ios" in os || "Ipados" in os;
};

/** `undefined` is both a device with no app and one the canister could not be asked about; neither delivers. */
const asAppState = (app: NotificationAppInfo | undefined): AppDeviceState => ({
  source: "app",
  subscribed: app?.subscribed === true,
  enabled: app?.enabled === true,
});

/** Nothing is known, so nothing is decided: the question is put to the user. */
const unreadBrowserState: BrowserDeviceState = {
  source: "browser",
  supported: true,
  permission: "default",
  subscribed: false,
  registered: false,
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
      /** Whether answering installs the Home Screen app rather than asking this browser. */
      installFirst: boolean;
    };

/**
 * Which screen this app and identity need.
 *
 * The app's consent and this device's state are independent, so the consent goes out
 * first and is awaited alongside whichever state this device's delivery turns on; only
 * the browser's registration has to wait, since it is read against the endpoint the
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
  // Started rather than awaited: the consent and this device's state need nothing from each other.
  const consent = actor
    .notification_consent_granted({ anchor_number: identityNumber, origin })
    .catch(() => false);

  // Asked first: where delivery is the app's, this browser's own permission and subscription are beside the point.
  if (notificationsNeedInstallHere()) {
    return resolveAppOptIn(identityNumber, await consent);
  }

  const [consented, browserState] = await Promise.all([consent, browser]);
  const state =
    browserState === undefined
      ? unreadBrowserState
      : await readDeviceState(identityNumber, browserState);
  // This browser's own permission, the one the unblock guidance is written about; the app's path records none.
  recordPermission(state.permission);
  const screen = resolveOptInScreen(state, consented);
  return screen === "skip"
    ? { screen, granted: consented && deliversHere(state) }
    : { screen, state, consented, installFirst: false };
};

/** An app another identity here already has is pointed at rather than installed again, but only where the canister says there is no app. */
const resolveAppOptIn = async (
  identityNumber: bigint,
  consented: boolean,
): Promise<OptInResolution> => {
  let status = await browserAppStatus(identityNumber);
  let claim: ClaimOutcome = "no-donor";
  if (status.kind === "no-app") {
    // The claim is written before its reply arrives, so a lost reply must not deny an app this browser already points at.
    claim = await claimDeviceApp(identityNumber);
    status = await browserAppStatus(identityNumber);
  }

  switch (status.kind) {
    case "unanswered": {
      // Nothing was learned, so nothing is decided; an install is offered only once an answer says there is no app.
      console.error(`the app could not be read: ${status.reason}`);
      const state = asAppState(undefined);
      return { screen: "enable", state, consented, installFirst: false };
    }
    case "no-app": {
      if (claim === "indeterminate") {
        // Nothing was asked of any donor, so this is not an answer that there is no app
        // here: offering the install would hand a second one to a device that has one.
        const state = asAppState(undefined);
        return { screen: "enable", state, consented, installFirst: false };
      }
      // The canister records which browser points at an app, so a second browser on the same phone finds none of its own and would keep asking.
      if (consented && (await saidAlreadyInstalled(identityNumber))) {
        return { screen: "skip", granted: consented };
      }
      const state = asAppState(undefined);
      return {
        screen: "enable",
        state,
        consented,
        // Their word is about the app, not about this dapp: somebody who has the app
        // and has not allowed this one still has a question to answer.
        installFirst: !(await saidAlreadyInstalled(identityNumber)),
      };
    }
    case "awaiting-install": {
      // An install this browser handed a key to and nobody has opened: nothing delivers
      // yet, and the steps are where the user is told to open it.
      const state = asAppState(undefined);
      return { screen: "enable", state, consented, installFirst: true };
    }
    case "app": {
      const state = asAppState(status.app);
      const screen = resolveOptInScreen(state, consented);
      return screen === "skip"
        ? { screen, granted: consented && deliversHere(state) }
        : { screen, state, consented, installFirst: !status.app.subscribed };
    }
  }
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
  // Started rather than awaited: the consent query has nothing to learn from what this device says.
  const consent = actor
    .notification_consent_granted({ anchor_number: identityNumber, origin })
    .catch(() => false);

  // Asked the same way the resolution asks it, or an app told yes on one screen would be told no on the next.
  if (notificationsNeedInstallHere()) {
    return readAppGranted(identityNumber, consent);
  }

  const [consented, browserState] = await Promise.all([
    consent,
    readBrowserPushState(),
  ]);
  if (browserState === undefined) {
    return false;
  }
  return (
    consented &&
    deliversHere(await readDeviceState(identityNumber, browserState))
  );
};

/** Must agree with the resolution on every answer the canister can give about this device. */
const readAppGranted = async (
  identityNumber: bigint,
  consent: Promise<boolean>,
): Promise<boolean> => {
  const [consented, status] = await Promise.all([
    consent,
    browserAppStatus(identityNumber),
  ]);
  switch (status.kind) {
    case "app":
      return consented && deliversHere(asAppState(status.app));
    case "no-app":
      // The same word the resolution reads: an app told yes on the way in must not be told no on the way out.
      return consented && (await saidAlreadyInstalled(identityNumber));
    case "awaiting-install":
      // An install nobody has opened has nothing registered to deliver to.
      return false;
    case "unanswered":
      // An answer that could not be read is not one that delivers.
      console.error(`the app could not be read: ${status.reason}`);
      return false;
  }
};

/** Picks the opt-in screen from device state and existing consent. */
export const resolveOptInScreen = (
  state: DeviceNotificationState,
  allowed: boolean,
): OptInScreen => {
  // Only this browser's path can be unable to carry notifications at all.
  if (state.source === "browser" && !state.supported) {
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
