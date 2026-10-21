import type { ActorSubclass } from "@icp-sdk/core/agent";
import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
import type { _SERVICE } from "$lib/generated/internet_identity_types";
import { throwCanisterError } from "$lib/utils/utils";
import { browserKeyActor, UnregisteredBrowserError } from "./browserActor";
import { exportLinkKey, generateLinkKey } from "./linkKey";
import { encodeLinkToken } from "./linkToken";
import { NOTIFICATION_APP_PATH } from "./appPath";

/** `undefined` where no sign-in has registered this browser; rejects where its key could not be read, which is not the same thing. */
const registeredBrowserActor = async (
  identityNumber: bigint,
): Promise<ActorSubclass<_SERVICE> | undefined> => {
  try {
    return await browserKeyActor(identityNumber);
  } catch (error) {
    if (error instanceof UnregisteredBrowserError) {
      return undefined;
    }
    throw error;
  }
};

/** `undefined` where this browser is not registered; rejects where its key could not be read or the canister refused the install.
 *  The canister refuses while the app it would replace is still answering, so a caller that offers an install asks the app to prove itself first. */
export const prepareInstallUrl = async ({
  identityNumber,
}: {
  identityNumber: bigint;
}): Promise<string | undefined> => {
  const actor = await registeredBrowserActor(identityNumber);
  if (actor === undefined) {
    return undefined;
  }
  const keyPair = await generateLinkKey();
  const [identity, linkKey] = await Promise.all([
    ECDSAKeyIdentity.fromKeyPair(keyPair),
    exportLinkKey(keyPair),
  ]);
  // The slot is written before the key is handed out, so a URL that exists is one the canister is waiting for.
  await actor
    .prepare_notification_app_link({
      anchor_number: identityNumber,
      link_principal: identity.getPrincipal(),
    })
    .then(throwCanisterError);
  return `${NOTIFICATION_APP_PATH}#${encodeLinkToken({
    identityNumber,
    linkKey,
  })}`;
};

/** The placeholder the new tab holds while the URL is still being minted: this page, told to wait rather than to decide. */
export const INSTALL_PLACEHOLDER_URL = `${NOTIFICATION_APP_PATH}?installing`;

/** Must be called straight from the click, which is the only time a browser allows it; the tab is opened on the placeholder and pointed once the URL resolves.
 *  A blocked tab is not an error: the screen shows the link instead. */
export const openInstallTab = (
  url: string | Promise<string | undefined>,
): boolean => {
  const opened = window.open(INSTALL_PLACEHOLDER_URL, "_blank");
  if (opened === null) {
    // The caller handed its only failure path to this promise, so a tab the browser
    // refused is no reason to leave that failure unhandled.
    if (typeof url !== "string") {
      void url.catch((error: unknown) => console.error(error));
    }
    return false;
  }
  if (typeof url === "string") {
    opened.location.href = url;
    return true;
  }
  void url.then(
    (resolved) => {
      if (resolved === undefined) {
        // Nothing to send them to, and an empty tab says less than none.
        opened.close();
        return;
      }
      opened.location.href = resolved;
    },
    (error: unknown) => {
      // A rejection left here would be an empty tab nothing ever closes.
      console.error(error);
      opened.close();
    },
  );
  return true;
};

/** A tab opened inside the press, waiting for somewhere to go. */
export interface PendingInstallTab {
  /** Whether the browser allowed the tab; a refused one is not a failure, the screen behind it carries the link. */
  opened: boolean;
  /** Points it at the install, or closes it where there is nothing to point it at. */
  point: (url: string | undefined) => void;
}

/** Must be called straight from the click: the tab has to open before anything is awaited. */
export const openPendingInstallTab = (): PendingInstallTab => {
  let point: (url: string | undefined) => void = () => {};
  const opened = openInstallTab(
    new Promise<string | undefined>((resolve) => {
      point = resolve;
    }),
  );
  return { opened, point };
};
