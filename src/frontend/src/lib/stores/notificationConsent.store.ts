import { get, type Readable, writable } from "svelte/store";
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
  /** Whether answering sends the user to install the Home Screen app, because this browser has no prompt to raise. */
  installFirst: boolean;
  /** Opened because the way this device receives the app's notifications stopped
   *  working, rather than because the app asked: declining drops what this device
   *  registered, so the next sign-in has nothing left to check here. */
  isRepair: boolean;
}

const contextInternal = writable<NotificationConsentContext | undefined>();
const settledInternal = writable<true | undefined>();

export const notificationConsentStore = {
  /** Clears any previous outcome first, so a stale settle from an earlier
   *  request cannot resolve this one before the user has seen anything. */
  setContext: (context: NotificationConsentContext): void => {
    settledInternal.set(undefined);
    contextInternal.set(context);
  },
  /** The screen opened for `context` is done — granted or not. What was actually
   *  recorded is read back from the canister rather than reported from here. A screen
   *  that answers after another one has replaced it settles nothing: the answer was
   *  about a question nobody is asking any more. */
  settle: (context: NotificationConsentContext | undefined): void => {
    if (context === undefined || get(contextInternal) !== context) {
      return;
    }
    settledInternal.set(true);
  },
  clear: (): void => {
    contextInternal.set(undefined);
    settledInternal.set(undefined);
  },
  subscribe: contextInternal.subscribe,
};

export const notificationConsentSettledStore: Readable<true | undefined> = {
  subscribe: settledInternal.subscribe,
};
