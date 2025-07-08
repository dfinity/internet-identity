import { Funnel } from "./Funnel";

/**
 * Registration flow events:
 *
 * Square brackets [] indicate optional events.
 *
 * Authentication flow events:
 *  last-used-present
 *    continue-as-screen
 *      use-another
 *        -> Move to select-method-screen
 *      continue-as-passkey
 *        auth-success
 *      continue-as-google
 *        auth-success
 *  last-used-not-present
 *    select-method-screen
 *      continue-with-google
 *        register-with-google
 *          successful-google-registration
 *            auth-success
 *          jwt-verification-failed
 *        login-with-google
 *          auth-success
 *          jwt-verification-failed
 *      continue-with-passkey-screen
 *        register-with-passkey
 *          enter-name-screen
 *            start-webauthn-creation
 *              successful-passkey-registration
 *                auth-success
 *        use-existing-passkey
 *          auth-success
 */
export const AuthenticationV2Events = {
  LastUsedPresent: "last-used-present",
  LastUsedNotPresent: "last-used-not-present",
  UseAnother: "use-another",
  ContinueAsScreen: "continue-as-screen",
  ContinueAsPasskey: "continue-as-passkey",
  ContinueAsGoogle: "continue-as-google",
  SelectMethodScreen: "select-method-screen",
  ContinueWithGoogle: "continue-with-google",
  RegisterWithGoogle: "register-with-google",
  SuccessfulGoogleRegistration: "successful-google-registration",
  LoginWithGoogle: "login-with-google",
  ContinueWithPasskeyScreen: "continue-with-passkey-screen",
  RegisterWithPasskey: "register-with-passkey",
  EnterNameScreen: "enter-name-screen",
  StartWebauthnCreation: "start-webauthn-creation",
  SuccessfulPasskeyRegistration: "successful-passkey-registration",
  UseExistingPasskey: "use-existing-passkey",
  AuthSuccess: "auth-success",
  JwtVerificationFailed: "jwt-verification-failed",
} as const;

export const authenticationV2Funnel = new Funnel<typeof AuthenticationV2Events>(
  "authentication-v2",
);
