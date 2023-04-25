import { features } from "$src/features";
import { isNullish } from "@dfinity/utils";
import { wrapError } from "./utils";

export const checkRequiredFeatures = async (
  url: URL
): Promise<true | string> => {
  if (features.DUMMY_AUTH) {
    // do not check for webauthn compatibility if DUMMY_AUTH is enabled
    return true;
  }
  if (isNullish(window.PublicKeyCredential))
    return "window.PublicKeyCredential is not defined";
  if (url.hash === "#compatibilityNotice")
    return "Remove #compatibilityNotice from the URL and try again.";
  // For mobile devices we want to make sure we can use platform authenticators
  if (!navigator.userAgent.match(/(iPhone|iPod|iPad|Android)/)) return true;
  try {
    return (await PublicKeyCredential.isUserVerifyingPlatformAuthenticatorAvailable())
      ? true
      : "This device does not offer WebAuthn authentication. Please make sure you have biometrics (fingerprint / Touch ID / Face ID) enabled and try again.";
  } catch (error) {
    return `An error occurred when checking for compatibility: ${wrapError(
      error
    )}`;
  }
};
