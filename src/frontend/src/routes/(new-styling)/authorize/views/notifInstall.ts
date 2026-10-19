import type { ActorSubclass } from "@icp-sdk/core/agent";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { allowApp } from "$lib/utils/notifications/enableNotifications";
import {
  openPendingInstallTab,
  installUrlOrNone,
} from "$lib/utils/notifications/installUrl";

/** What pressing Allow on the install screen started. */
export interface StartedInstall {
  /** The consent this press records, held rather than awaited because the steps are walked in a tab of their own. */
  granted: Promise<void>;
  /** Where the install lives, once there is a token to point at it. */
  url: Promise<string | undefined>;
}

/** Synchronous so it can be called straight from the press: a browser allows `window.open` only inside the gesture.
 * The token is signed here rather than while the screen rendered, since one that ran out meanwhile is refused at the end of the steps. */
export const startAppInstall = ({
  identityNumber,
  origin,
  actor,
  consented,
}: {
  identityNumber: bigint;
  origin: string;
  actor: ActorSubclass<_SERVICE>;
  /** Whether this app already holds consent from this identity. */
  consented: boolean;
}): StartedInstall => {
  const tab = openPendingInstallTab();

  const granted = consented
    ? Promise.resolve()
    : allowApp({ identityNumber, origin, actor });
  // Nothing awaits this until sign-in settles, and an unattached rejection in between is an unhandled rejection.
  void granted.catch(() => undefined);

  const url = installUrlOrNone(identityNumber);
  void url.then(tab.point);

  return { granted, url };
};
