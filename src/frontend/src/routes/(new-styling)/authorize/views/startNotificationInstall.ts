/**
 * Sending the user to install the Home Screen app that will carry their notifications.
 *
 * The token is signed before the button is pressed, not in the handler: Safari allows
 * `window.open` only from inside a user gesture, and an `await` between the click and
 * the call loses it. So the screen prepares a URL while it renders and the handler does
 * nothing but open it.
 */
import { signNotificationAppLink } from "$lib/stores/browser-key.store";
import {
  encodeLinkToken,
  LINK_TOKEN_LIFETIME_MS,
} from "../../notifications/linkToken";

const NOTIFICATIONS_PATH = "/notifications";

/**
 * The URL to install from, or `undefined` where this browser has no key to sign with,
 * which is a browser no sign-in has registered.
 *
 * The token rides the fragment, so it is never sent to the gateway, and iOS bookmarks
 * the whole URL: the installed app launches holding it.
 */
export const prepareInstallUrl = async (
  identityNumber: bigint,
): Promise<string | undefined> => {
  const expiresAtNs =
    BigInt(Date.now() + LINK_TOKEN_LIFETIME_MS) * BigInt(1_000_000);
  const signature = await signNotificationAppLink({
    identityNumber,
    expiresAtNs,
  });
  if (signature === undefined) {
    return undefined;
  }
  return `${NOTIFICATIONS_PATH}#${encodeLinkToken({
    identityNumber,
    expiresAtNs,
    signature,
  })}`;
};

/**
 * Opens the install in a tab of its own, reporting whether the browser allowed it.
 *
 * Must be called straight from the click. A blocked tab is not an error: the design
 * answers it by showing the link instead, which the user can follow themselves.
 */
export const openInstallTab = (url: string): boolean => {
  const opened = window.open(url, "_blank");
  return opened !== null;
};
