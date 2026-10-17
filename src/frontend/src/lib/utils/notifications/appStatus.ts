import type {
  _SERVICE,
  NotificationAppInfo,
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
