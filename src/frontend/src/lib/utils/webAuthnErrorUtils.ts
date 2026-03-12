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
export const isWebAuthnCancelError = (error: unknown): boolean => {
  return (
    error instanceof DOMException &&
    // According to WebAuthn spec, the browser must throw a NotAllowedError when the user cancels the operation.
    // However, some browsers (Firefox) throw an AbortError instead.
    (error.name === "NotAllowedError" || error.name === "AbortError")
  );
};
