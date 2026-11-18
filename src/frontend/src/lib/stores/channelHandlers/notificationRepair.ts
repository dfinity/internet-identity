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

/**
 * Checks, once the identity has signed in to `origin` as the account `accountPrincipal`
 * names, that this device still receives the app's notifications, and puts the allow
 * screen in front of the user where only they can repair it. Holds the sign-in only
 * while a path past its limit is probed. `browserActor` is what signs as this browser.
 *
 * Resolves rather than rejects: the sign-in the app asked for is not the user's to lose
 * over a notification path, so a check that fails is shown and leaves the device as it
 * was.
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
  if (!get(PUSH_NOTIFICATIONS)) {
    return "done";
  }
  // Held only once a path is past its limit, which is when the check takes long enough
  // to show; released when the check, and the screen it may open, has settled.
  let releaseScreen: (() => void) | undefined;
  // A switch to another identity ends a check that was about this one.
  const switched = new AbortController();
  const stopWatching = authenticationStore.subscribe((current) => {
    if (current?.identityNumber !== identityNumber) {
      switched.abort(new IdentitySwitchedError());
    }
  });
  try {
    let browser: BrowserCalls;
    try {
      browser = await browserActor(identityNumber);
    } catch (error) {
      if (error instanceof UnregisteredBrowserError) {
        return "done";
      }
      throw error;
    }
    const outcome = await checkNotificationPath({
      identityNumber,
      origin,
      accountPrincipal,
      actor,
      browser,
      onPastLimit: () => {
        releaseScreen ??= claimScreen();
      },
      signal: switched.signal,
    });
    switch (outcome.kind) {
      case "working":
        break;
      case "ask":
        await askToRepair({
          identityNumber,
          origin,
          actor,
          installFirst: outcome.installFirst,
        });
        break;
    }
    return switched.signal.aborted ? "identity-changed" : "done";
  } catch (error) {
    if (switched.signal.aborted) {
      return "identity-changed";
    }
    console.error(error);
    toaster.error({
      title: t`Notifications unavailable`,
      description: failureMessage(error),
    });
    return "done";
  } finally {
    stopWatching();
    releaseScreen?.();
  }
};

/** Ends a check about an identity the user has switched away from. */
class IdentitySwitchedError extends Error {
  constructor() {
    super("the identity being checked is no longer the one signed in");
    this.name = "IdentitySwitchedError";
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
