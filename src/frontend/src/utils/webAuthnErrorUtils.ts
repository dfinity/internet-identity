/** Checks whether the error corresponds with the WebAuthnSpec for duplicate device:
 *  * https://www.w3.org/TR/webauthn-2/#sctn-createCredential (Step 20)
 *  * https://www.w3.org/TR/webauthn-2/#sctn-op-make-cred (Step 3)
 * @param error error to check
 */
export function isDuplicateDeviceError(error: unknown): boolean {
  return error instanceof DOMException && error.name === "InvalidStateError";
}
