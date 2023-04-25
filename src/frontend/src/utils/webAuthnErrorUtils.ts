import { displayError } from "$src/components/displayError";

export const WEBAUTHN_CANCEL_TEMPLATE = {
  title: "Operation canceled",
  message:
    "The interaction with your security device was canceled or timed out. Please try again.",
};

export const displayCancelError = (primaryButton: string) =>
  displayError({
    ...WEBAUTHN_CANCEL_TEMPLATE,
    primaryButton,
  });

/** Checks whether the error corresponds with the WebAuthnSpec for duplicate device:
 *  * https://www.w3.org/TR/webauthn-2/#sctn-createCredential (Step 20)
 *  * https://www.w3.org/TR/webauthn-2/#sctn-op-make-cred (Step 3)
 * @param error error to check
 */
export function isDuplicateDeviceError(error: unknown): boolean {
  return error instanceof DOMException && error.name === "InvalidStateError";
}

/** Checks whether the error corresponds with the WebAuthnSpec for cancelling the operation:
 *  * https://www.w3.org/TR/webauthn-2/#sctn-createCredential (Step 16)
 *  * https://www.w3.org/TR/webauthn-2/#sctn-getAssertion (Step 12)
 *
 * Note: there are many more cases where `NotAllowedError`s can be thrown, however
 * those cases mostly correspond to situations that should never happen with II
 * (such as using an opaque origin for the rpId or trying to register a WebAuthn
 * credential in a cross-origin iFrame).
 *
 * @param error error to check
 */
export function isCancel(error: unknown): boolean {
  return error instanceof DOMException && error.name === "NotAllowedError";
}
