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
 *        go-to-dashboard
 *      continue-as-openid
 *        auth-success
 *        go-to-dashboard
 *    select-method-screen
 *      continue-with-openid
 *        register-with-openid
 *          successful-openid-registration
 *            auth-success
 *          jwt-verification-failed
 *          jwt-verification-expired
 *        login-with-openid
 *          auth-success
 *          jwt-verification-failed
 *          jwt-verification-expired
 *      continue-with-passkey-screen
 *        enter-name-screen
 *          [info-passkey-screen]
 *          start-webauthn-creation
 *            register-with-passkey
 *              successful-passkey-registration
 *                auth-success
 *                go-to-dashboard
 *        use-existing-passkey
 *          auth-success
 *          go-to-dashboard
 */
export const AuthenticationV2Events = {
  UseAnother: "use-another",
  ContinueAsScreen: "continue-as-screen",
  ContinueAsPasskey: "continue-as-passkey",
  ContinueAsOpenID: "continue-as-openid",
  SelectMethodScreen: "select-method-screen",
  ContinueWithOpenID: "continue-with-openid",
  RegisterWithOpenID: "register-with-openid",
  SuccessfulOpenIDRegistration: "successful-openid-registration",
  LoginWithOpenID: "login-with-openid",
  ContinueWithPasskeyScreen: "continue-with-passkey-screen",
  EnterNameScreen: "enter-name-screen",
  StartWebauthnCreation: "start-webauthn-creation",
  RegisterWithPasskey: "register-with-passkey",
  SuccessfulPasskeyRegistration: "successful-passkey-registration",
  UseExistingPasskey: "use-existing-passkey",
  AuthSuccess: "auth-success",
  JwtVerificationFailed: "jwt-verification-failed",
  JwtVerificationExpired: "jwt-verification-expired",
  InfoPasskeyScreen: "info-passkey-screen",
  GoToDashboard: "go-to-dashboard",
} as const;

export const authenticationV2Funnel = new Funnel<typeof AuthenticationV2Events>(
  "authentication-v2",
);
