import { type Readable, writable } from "svelte/store";
import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import type { DeviceNotificationState } from "$lib/utils/notifications/notificationState";

/** What the consent screen needs to run, once the user is authenticated. */
export interface NotificationConsentContext {
  /** The app being asked about, already folded to its canonical spelling. */
  effectiveOrigin: string;
  identityNumber: bigint;
  actor: ActorSubclass<_SERVICE>;
  /** This browser as the resolution found it, so answering the question only does
   *  what is left to do. */
  device: DeviceNotificationState;
  /** Whether this app already holds consent from this identity. */
  consented: boolean;
  /** Answering sends the user to install the app that carries notifications, because
   *  this browser has no prompt to raise. */
  installFirst: boolean;
}

/** What the screen did, where the answer cannot be read back from the canister. */
export interface NotificationConsentOutcome {
  /** The user pressed Allow on a browser that answers by installing an app. Nothing is
   *  recorded yet, and this browser cannot see the app's entry, so what the app is told
   *  rests on this. */
  installStarted: boolean;
}

const contextInternal = writable<NotificationConsentContext | undefined>();
const settledInternal = writable<NotificationConsentOutcome | undefined>();

export const notificationConsentStore = {
  /** Clears any previous outcome first, so a stale settle from an earlier
   *  request cannot resolve this one before the user has seen anything. */
  setContext: (context: NotificationConsentContext): void => {
    settledInternal.set(undefined);
    contextInternal.set(context);
  },
  /**
   * The screen is done, granted or not. What was recorded is read back from the
   * canister rather than reported from here, with one exception: a browser that
   * answers by installing an app has nothing to read back yet, because the app has
   * its own entry and has not been opened. That case says so.
   */
  settle: (
    outcome: NotificationConsentOutcome = { installStarted: false },
  ): void => {
    settledInternal.set(outcome);
  },
  clear: (): void => {
    contextInternal.set(undefined);
    settledInternal.set(undefined);
  },
  subscribe: contextInternal.subscribe,
};

export const notificationConsentSettledStore: Readable<
  NotificationConsentOutcome | undefined
> = {
  subscribe: settledInternal.subscribe,
};
