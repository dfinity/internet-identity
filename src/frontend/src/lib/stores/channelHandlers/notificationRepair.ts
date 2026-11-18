import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { get } from "svelte/store";
import { PUSH_NOTIFICATIONS } from "$lib/state/featureFlags";
import { authenticationStore } from "$lib/stores/authentication.store";
import {
  notificationConsentSettledStore,
  notificationConsentStore,
} from "$lib/stores/notificationConsent.store";
import { checkNotificationPath } from "$lib/utils/notifications/repairPath";
import {
  readBrowserPushState,
  resolveOptIn,
} from "$lib/utils/notifications/notificationState";
import { waitForStore } from "$lib/utils/utils";
import { claimScreen } from "$lib/stores/pendingScreen.store";
import { handleError } from "$lib/components/utils/error";

/**
 * Checks, once the identity has signed in to `origin`, that this device still receives
 * the app's notifications, and puts the allow screen in front of the user where only
 * they can repair it. Holds the sign-in only while a path past its limit is probed.
 *
 * Resolves rather than rejects: the sign-in the app asked for is not the user's to lose
 * over a notification path, so a check that fails leaves the device as it was.
 */
export const repairNotificationPath = async ({
  identityNumber,
  origin,
  actor,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
}): Promise<void> => {
  if (!get(PUSH_NOTIFICATIONS)) {
    return;
  }
  // Held only once a path is past its limit, which is when the check takes long enough
  // to show; released when the check, and the screen it may open, has settled.
  let releaseScreen: (() => void) | undefined;
  // A switch to another identity ends a check that was about this one.
  const switched = new AbortController();
  const stopWatching = authenticationStore.subscribe((current) => {
    if (current !== undefined && current.identityNumber !== identityNumber) {
      switched.abort(new IdentitySwitchedError());
    }
  });
  try {
    const outcome = await checkNotificationPath({
      identityNumber,
      origin,
      actor,
      onPastLimit: () => {
        releaseScreen ??= claimScreen();
      },
      signal: switched.signal,
    });
    switch (outcome.kind) {
      case "working":
        return;
      case "ask":
        await askToRepair({
          identityNumber,
          origin,
          actor,
          installFirst: outcome.installFirst,
        });
        return;
    }
  } catch (error) {
    if (error instanceof IdentitySwitchedError) {
      return;
    }
    // Shown, not thrown: the sign-in goes on, and the next one checks again.
    console.error(error);
    handleError(error);
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
      waitForStore(authenticationStore, (current) =>
        current?.identityNumber !== identityNumber ? true : undefined,
      ),
    ]);
  } finally {
    notificationConsentStore.clear();
  }
};
