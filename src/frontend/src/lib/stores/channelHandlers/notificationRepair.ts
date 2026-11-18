import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { get } from "svelte/store";
import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  notificationConsentSettledStore,
  notificationConsentStore,
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
import { claimScreen } from "$lib/stores/pendingScreen.store";
import { toaster } from "$lib/components/utils/toaster";
import { t } from "$lib/stores/locale.store";
import { failureMessage } from "$lib/utils/notifications/notificationDiagnostics";
import {
  browserKeyActor,
  UnregisteredBrowserError,
} from "$lib/utils/notifications/browserActor";
import type { Principal } from "@icp-sdk/core/principal";

/** How the check ended for the sign-in waiting on it: `done` whatever it found, or
 *  `identity-changed` where the user switched identity or signed out meanwhile, so what
 *  the sign-in minted is for an identity that is no longer the one chosen. */
export type RepairOutcome = "done" | "identity-changed";

/** The longest the automated part of the check may hold a sign-in: two probes and what
 *  comes between them, with room to spare. Past it the sign-in goes on and the next one
 *  checks again. */
const CHECK_DEADLINE_MS = 30_000;

/**
 * Checks, once the identity has signed in to `origin` as the account `accountPrincipal`
 * names, that this device still receives the app's notifications, and puts the allow
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
  accountPrincipal,
  actor,
  browserActor = browserKeyActor,
}: {
  identityNumber: bigint;
  origin: string;
  accountPrincipal: Principal;
  actor: ActorSubclass<_SERVICE>;
  browserActor?: (identityNumber: bigint) => Promise<BrowserCalls>;
}): Promise<RepairOutcome> => {
  if (!get(PUSH_NOTIFICATIONS) || !isWebOrigin(origin)) {
    return "done";
  }
  // Held only once a path is past its limit, which is when the check takes long enough
  // to show; released when the check, and the screen it may open, has settled.
  let releaseScreen: (() => void) | undefined;
  // A switch to another identity ends a check that was about this one, and so does
  // running out of time.
  let isIdentityChanged = false;
  const stopped = new AbortController();
  const stopWatching = authenticationStore.subscribe((current) => {
    if (current?.identityNumber !== identityNumber) {
      isIdentityChanged = true;
      stopped.abort(new IdentitySwitchedError());
    }
  });
  const deadline = setTimeout(
    () => stopped.abort(new CheckTimedOutError()),
    CHECK_DEADLINE_MS,
  );
  let failure: unknown;
  try {
    const outcome = await checkWithin(
      stopped.signal,
      checkPath({
        identityNumber,
        origin,
        accountPrincipal,
        actor,
        browserActor,
        onPastLimit: () => {
          releaseScreen ??= claimScreen();
        },
        signal: stopped.signal,
      }),
    );
    clearTimeout(deadline);
    if (outcome.kind === "ask" && !isIdentityChanged) {
      await askToRepair({
        identityNumber,
        origin,
        actor,
        installFirst: outcome.installFirst,
      });
    }
  } catch (error) {
    failure = error;
  } finally {
    clearTimeout(deadline);
    stopWatching();
    releaseScreen?.();
  }
  // The one way out, so no path answers for an identity the user has left.
  if (isIdentityChanged) {
    return "identity-changed";
  }
  if (failure !== undefined && !(failure instanceof CheckTimedOutError)) {
    console.error(failure);
    toaster.error({
      title: t`Notifications unavailable`,
      description: failureMessage(failure),
    });
  }
  return "done";
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
    if (signal.aborted) {
      onAbort();
      return;
    }
    signal.addEventListener("abort", onAbort, { once: true });
    check.then(resolve, reject).finally(() => {
      signal.removeEventListener("abort", onAbort);
    });
  });

/** The automated part of the check: this browser's actor, then the path. */
const checkPath = async ({
  identityNumber,
  origin,
  accountPrincipal,
  actor,
  browserActor,
  onPastLimit,
  signal,
}: {
  identityNumber: bigint;
  origin: string;
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

/** The allow screen, opened by the check rather than by the app. */
const askToRepair = async ({
  identityNumber,
  origin,
  actor,
  installFirst,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
  /** The Home Screen app this device routes through stopped answering. */
  installFirst: boolean;
}): Promise<void> => {
  if (installFirst) {
    notificationConsentStore.setContext({
      effectiveOrigin: origin,
      identityNumber,
      actor,
      device: { source: "app", subscribed: false, enabled: false },
      consented: true,
      installFirst: true,
      isRepair: true,
    });
  } else {
    const resolution = await resolveOptIn({
      identityNumber,
      origin,
      actor,
      browser: readBrowserPushState(),
      isInstalledApp: false,
    });
    if (resolution.screen === "skip") {
      return;
    }
    notificationConsentStore.setContext({
      effectiveOrigin: origin,
      identityNumber,
      actor,
      device: resolution.state,
      consented: resolution.consented,
      installFirst: resolution.installFirst,
      isRepair: true,
    });
  }
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
