import type { ActorSubclass } from "@icp-sdk/core/agent";
import type {
  _SERVICE,
  RemoveUnansweredPwaWebPushSubscriptionsRequest,
} from "$lib/generated/internet_identity_types";
import { get } from "svelte/store";
import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  notificationConsentSettledStore,
  notificationConsentStore,
  type NotificationConsentContext,
} from "$lib/stores/notificationConsent.store";
import {
  checkNotificationPath,
  type BrowserCalls,
  type PathCheckOutcome,
} from "$lib/utils/notifications/repairPath";
import {
  readBrowserPushState,
  resolveOptIn,
} from "$lib/utils/notifications/notificationState";
import { waitForStore } from "$lib/utils/utils";
import { showNotificationProbe } from "$lib/stores/notificationProbe.store";
import {
  browserKeyActor,
  UnregisteredBrowserError,
} from "$lib/utils/notifications/browserActor";
import type { Principal } from "@icp-sdk/core/principal";

/** How the check ended for the sign-in waiting on it: `done` whatever it found, `asked`
 *  where it showed the allow screen, which has no time limit, so what the sign-in minted
 *  before it may have run out, or `identity-changed` where the user switched identity or
 *  signed out meanwhile, so what the sign-in minted is for an identity that is no longer
 *  the one chosen. */
export type RepairOutcome = "done" | "asked" | "identity-changed";

/** The longest the automated part of the check may hold a sign-in: two probes, each
 *  waiting until it has had its answer time, and what comes between them, with room to
 *  spare. Past it the sign-in goes on and the next one checks again. */
const CHECK_DEADLINE_MS = 40_000;

/**
 * Checks, once the identity has signed in to `origin` from its frontend at
 * `frontendOrigin` as the account `accountPrincipal` names, that this device still
 * receives the app's notifications for that frontend, and puts the allow
 * screen in front of the user where only they can repair it. Holds the sign-in only
 * while a path past its limit is probed, and never past {@link CHECK_DEADLINE_MS}.
 * `browserActor` is what signs as this browser.
 *
 * Only a web origin is checked: nothing else is notified. Resolves rather than rejects:
 * the sign-in the app asked for is not the user's to lose over a notification path, so
 * a check that fails is shown and leaves the device as it was.
 */
export const repairNotificationPath = async ({
  identityNumber,
  origin,
  frontendOrigin,
  accountPrincipal,
  actor,
  browserActor = browserKeyActor,
}: {
  identityNumber: bigint;
  origin: string;
  /** The page the sign-in came from: `origin` itself, or one of its alternative
   *  origins. */
  frontendOrigin: string;
  accountPrincipal: Principal;
  actor: ActorSubclass<_SERVICE>;
  browserActor?: (identityNumber: bigint) => Promise<BrowserCalls>;
}): Promise<RepairOutcome> => {
  if (
    !get(PUSH_NOTIFICATIONS) ||
    !isWebOrigin(origin) ||
    !isWebOrigin(frontendOrigin)
  ) {
    return "done";
  }
  // Shown once a path is past its limit, which is when the check probes it, and taken
  // down as soon as the check has an answer.
  let hideProbe: (() => void) | undefined;
  // A switch to another identity ends a check that was about this one, and so does
  // running out of time.
  let isIdentityChanged = false;
  const stopController = new AbortController();
  const stopWatching = authenticationStore.subscribe((current) => {
    if (current?.identityNumber !== identityNumber) {
      isIdentityChanged = true;
      stopController.abort(new IdentitySwitchedError());
    }
  });
  const deadline = setTimeout(
    () => stopController.abort(new CheckTimedOutError()),
    CHECK_DEADLINE_MS,
  );
  let failure: unknown;
  let isAsked = false;
  try {
    const outcome = await checkWithin(
      stopController.signal,
      checkPath({
        identityNumber,
        origin,
        frontendOrigin,
        accountPrincipal,
        actor,
        browserActor,
        onPastLimit: () => {
          // A check that runs on after it was stopped finds the view already taken down.
          if (!stopController.signal.aborted) {
            hideProbe ??= showNotificationProbe();
          }
        },
        signal: stopController.signal,
      }),
    );
    hideProbe?.();
    if (outcome.kind === "ask" && !isIdentityChanged) {
      // What the screen shows is read under the same deadline and switch as the check:
      // only a screen the user can answer may hold the sign-in for longer.
      const context = await checkWithin(
        stopController.signal,
        repairContext({
          identityNumber,
          origin,
          actor,
          installFirst: outcome.installFirst,
          disabledInstalledApps: outcome.disabledInstalledApps,
        }),
      );
      clearTimeout(deadline);
      if (context !== undefined) {
        isAsked = true;
        await askToRepair(context);
      }
    }
  } catch (error) {
    failure = error;
  } finally {
    clearTimeout(deadline);
    stopWatching();
    hideProbe?.();
  }
  // The one way out, so no path answers for an identity the user has left.
  if (isIdentityChanged) {
    return "identity-changed";
  }
  if (failure !== undefined && !(failure instanceof CheckTimedOutError)) {
    console.error(failure);
    // Loaded only when there is a failure to show: its translations read the feature
    // flags, and this module is evaluated before them, in the import cycle every channel
    // handler sits in through the channel store. Not awaited, so the sign-in never waits
    // on the toast.
    void import("$lib/utils/notifications/repairFailure")
      .then(({ showRepairFailure }) => showRepairFailure(failure))
      .catch(console.error);
  }
  return isAsked ? "asked" : "done";
};

/** Whether `origin` is a web page's, the only kind an app's notifications are for. */
const isWebOrigin = (origin: string): boolean => {
  try {
    const { protocol } = new URL(origin);
    return protocol === "https:" || protocol === "http:";
  } catch {
    return false;
  }
};

/** `check`, or `signal`'s reason as soon as it is aborted, whichever comes first: a call
 *  the check is waiting on cannot be stopped, but the sign-in need not wait for it. */
const checkWithin = <T>(signal: AbortSignal, check: Promise<T>): Promise<T> =>
  new Promise<T>((resolve, reject) => {
    const onAbort = () => reject(signal.reason);
    // Handled before anything else, so a check that fails after the sign-in stopped
    // waiting for it is not left as an unhandled rejection.
    check.then(resolve, reject).finally(() => {
      signal.removeEventListener("abort", onAbort);
    });
    if (signal.aborted) {
      onAbort();
      return;
    }
    signal.addEventListener("abort", onAbort, { once: true });
  });

/** The automated part of the check: this browser's actor, then the path. */
const checkPath = async ({
  identityNumber,
  origin,
  frontendOrigin,
  accountPrincipal,
  actor,
  browserActor,
  onPastLimit,
  signal,
}: {
  identityNumber: bigint;
  origin: string;
  frontendOrigin: string;
  accountPrincipal: Principal;
  actor: ActorSubclass<_SERVICE>;
  browserActor: (identityNumber: bigint) => Promise<BrowserCalls>;
  onPastLimit: () => void;
  signal: AbortSignal;
}): Promise<PathCheckOutcome> => {
  let browser: BrowserCalls;
  try {
    browser = await browserActor(identityNumber);
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return { kind: "working" };
    }
    throw error;
  }
  return checkNotificationPath({
    identityNumber,
    origin,
    frontendOrigin,
    accountPrincipal,
    actor,
    browser,
    onPastLimit,
    signal,
  });
};

/** Ends a check about an identity the user has switched away from. */
class IdentitySwitchedError extends Error {
  constructor() {
    super("the identity being checked is no longer the one signed in");
    this.name = "IdentitySwitchedError";
  }
}

/** Ends a check that held the sign-in for {@link CHECK_DEADLINE_MS}. */
class CheckTimedOutError extends Error {
  constructor() {
    super("the notification check ran out of time");
    this.name = "CheckTimedOutError";
  }
}

/** What the allow screen the check opens shows, or `undefined` where there is nothing
 *  left to ask. A device muted by `disabledInstalledApps` is asked even where it is set
 *  up, as long as it delivers here once they are removed. */
const repairContext = async ({
  identityNumber,
  origin,
  actor,
  installFirst,
  disabledInstalledApps,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
  /** The Home Screen app this device routes through stopped answering. */
  installFirst: boolean;
  /** The installed web apps the app disabled that mute this device. */
  disabledInstalledApps:
    RemoveUnansweredPwaWebPushSubscriptionsRequest | undefined;
}): Promise<NotificationConsentContext | undefined> => {
  if (installFirst) {
    return {
      effectiveOrigin: origin,
      identityNumber,
      actor,
      device: { source: "app", subscribed: false, enabled: false },
      consented: true,
      installFirst: true,
      isRepair: true,
      disabledInstalledApps,
    };
  }
  const resolution = await resolveOptIn({
    identityNumber,
    origin,
    actor,
    browser: readBrowserPushState(),
    isInstalledApp: false,
  });
  if (resolution.screen === "skip") {
    if (disabledInstalledApps === undefined || !resolution.granted) {
      return undefined;
    }
    return {
      effectiveOrigin: origin,
      identityNumber,
      actor,
      device: resolution.state,
      consented: true,
      installFirst: false,
      isRepair: true,
      disabledInstalledApps,
    };
  }
  return {
    effectiveOrigin: origin,
    identityNumber,
    actor,
    device: resolution.state,
    consented: resolution.consented,
    installFirst: resolution.installFirst,
    isRepair: true,
    disabledInstalledApps,
  };
};

/** The allow screen, opened by the check rather than by the app, until it is answered or
 *  the identity it asks about is switched away from. */
const askToRepair = async (
  context: NotificationConsentContext,
): Promise<void> => {
  const { identityNumber } = context;
  notificationConsentStore.setContext(context);
  try {
    // The header lets the user switch identity on this screen, and the question was
    // about the identity that signed in, so a switch ends it.
    await Promise.race([
      waitForStore(notificationConsentSettledStore),
      waitForStore(
        authenticationStore,
        (current) => current?.identityNumber !== identityNumber || undefined,
      ),
    ]);
  } finally {
    notificationConsentStore.clear();
  }
};
