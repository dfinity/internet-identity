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
  /** The screen is done — granted or not. What was actually recorded is read
   *  back from the canister rather than reported from here. */
  settle: (): void => {
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
