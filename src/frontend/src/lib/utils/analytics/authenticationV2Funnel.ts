import { Funnel } from "./Funnel";

/**
 * Registration flow events:
 *
 * Square brackets [] indicate optional events.
 *
 * Authentication flow events:
 *    continue-as-screen
 *      use-another
 *        -> Move to select-method-screen
 *      continue-as-passkey
 *        auth-success
 *      continue-as-google
 *        auth-success
 *    select-method-screen
 *      continue-with-google
 *        register-with-google
 *          successful-google-registration
 *            auth-success
 *          jwt-verification-failed
 *          jwt-verification-expired
 *        login-with-google
 *          auth-success
 *          jwt-verification-failed
 *          jwt-verification-expired
 *      continue-with-passkey-screen
 *        enter-name-screen
 *          start-webauthn-creation
 *            register-with-passkey
 *              successful-passkey-registration
 *                auth-success
 *        use-existing-passkey
 *          auth-success
 */
export const AuthenticationV2Events = {
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
  EnterNameScreen: "enter-name-screen",
  StartWebauthnCreation: "start-webauthn-creation",
  RegisterWithPasskey: "register-with-passkey",
  SuccessfulPasskeyRegistration: "successful-passkey-registration",
  UseExistingPasskey: "use-existing-passkey",
  AuthSuccess: "auth-success",
  JwtVerificationFailed: "jwt-verification-failed",
  JwtVerificationExpired: "jwt-verification-expired",
} as const;

export const authenticationV2Funnel = new Funnel<typeof AuthenticationV2Events>(
  "authentication-v2",
);
