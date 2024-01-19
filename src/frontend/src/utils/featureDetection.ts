import { features } from "$src/features";
import { isNullish } from "@dfinity/utils";

export const supportsWebAuthn = (): boolean => {
  if (features.DUMMY_AUTH) {
    // do not check for webauthn compatibility if DUMMY_AUTH is enabled
    return true;
  }

  if (isNullish(window.PublicKeyCredential)) return false;

  return true;
};
