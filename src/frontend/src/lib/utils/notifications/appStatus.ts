import type {
  _SERVICE,
  NotificationAppInfo,
  EnableNotificationAppError,
  ProbeNotificationAppError,
} from "$lib/generated/internet_identity_types";
import {
  browserKeyActor,
  UnregisteredBrowserError,
} from "$lib/utils/notifications/browserActor";
import { isCanisterError, throwCanisterError } from "$lib/utils/utils";

/** Four answers and not two: a query that could not be made is not an entry that points at nothing, and an install nobody has opened yet is neither. Reading any of them as another sends someone with a working app through the install again. */
export type BrowserAppStatus =
  | { kind: "app"; app: NotificationAppInfo }
  /** An install this browser handed a key to, which the app can still spend until `expiresAtNs`. */
  | { kind: "awaiting-install"; expiresAtNs: bigint }
  | { kind: "no-app" }
  | { kind: "unanswered"; reason: string };

export const browserAppStatus = async (
  identityNumber: bigint,
): Promise<BrowserAppStatus> => {
  try {
    const actor = await browserKeyActor(identityNumber);
    const status = await actor.get_notification_app_status({
      anchor_number: identityNumber,
    });
    if ("Linked" in status) {
      return { kind: "app", app: status.Linked };
    }
    if ("AwaitingInstall" in status) {
      return { kind: "awaiting-install", expiresAtNs: status.AwaitingInstall };
    }
    return { kind: "no-app" };
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return { kind: "no-app" };
    }
    console.error(error);
    return {
      kind: "unanswered",
      reason: error instanceof Error ? error.message : String(error),
    };
  }
};

/** `no-app` is this browser pointing at nothing; `not-sent` is nothing having left the canister, which is no evidence about the app in either direction. */
export type ProbeOutcome =
  | { kind: "sent"; probeId: bigint }
  | { kind: "no-app" }
  | { kind: "not-sent"; reason: string };

/** Nothing says when a user deletes a web app: an app that is gone answers 404 or 410 at the push service, which takes its record with it. */
export const probeApp = async (
  identityNumber: bigint,
): Promise<ProbeOutcome> => {
  try {
    const actor = await browserKeyActor(identityNumber);
    const probeId = await actor
      .probe_notification_app({ anchor_number: identityNumber })
      .then(throwCanisterError);
    return { kind: "sent", probeId };
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return { kind: "no-app" };
    }
    if (isCanisterError<ProbeNotificationAppError>(error)) {
      switch (error.type) {
        case "NoNotificationApp":
          return { kind: "no-app" };
        case "NotReachable":
          return { kind: "not-sent", reason: error.type };
      }
    }
    console.error(error);
    return {
      kind: "not-sent",
      reason: error instanceof Error ? error.message : String(error),
    };
  }
};

/**
 * Switches delivery on for this identity, which is also what proves the app is there.
 *
 * The switch and the probe cannot be separated: a reference that is off is skipped when
 * the canister picks who to send to, so a probe sent first finds nothing to wake, and a
 * switch thrown first has the canister announce it and then say the same thing again on
 * the probe. `not-sent` is the switch left off, which is what it means for a device
 * whose app has gone: nothing here has to be undone.
 */
export const enableApp = async (
  identityNumber: bigint,
): Promise<ProbeOutcome> => {
  try {
    const actor = await browserKeyActor(identityNumber);
    const probeId = await actor
      .enable_notification_app({ anchor_number: identityNumber })
      .then(throwCanisterError);
    return { kind: "sent", probeId };
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return { kind: "no-app" };
    }
    if (isCanisterError<EnableNotificationAppError>(error)) {
      switch (error.type) {
        case "NoNotificationApp":
          return { kind: "no-app" };
        case "NotReachable":
          return { kind: "not-sent", reason: error.type };
      }
    }
    console.error(error);
    return {
      kind: "not-sent",
      reason: error instanceof Error ? error.message : String(error),
    };
  }
};
