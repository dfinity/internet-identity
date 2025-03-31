import { supportsWebauthRoR } from "./userAgent";

const isNative = (fn: () => unknown) => /\[native code\]/.test(fn.toString());

/**
 * Util to find out whether the current user's browser supports Related Origin Requests.
 *
 * There are two things to consider:
 * - Does the browser and version support RoR?
 * - Does the user have an installed password manager extension?
 *   - Some extensions monkey patch the `navigator.credentials.get` function, e.g. 1Password.
 *     - We can check for this with `toString` method.
 *   - Others proxy `navigator.credentials.get` to their own implementation, e.g. NordPass.
 *     - We can't check this.
 *
 * @returns {boolean}
 */
export const userSupportsWebauthRoR = (): boolean => {
  const userAgentSuportsRoR = supportsWebauthRoR(navigator.userAgent);
  const hasMonkeyPatchedCredentialGet = !isNative(
    window.navigator.credentials.get,
  );
  return userAgentSuportsRoR && !hasMonkeyPatchedCredentialGet;
};
