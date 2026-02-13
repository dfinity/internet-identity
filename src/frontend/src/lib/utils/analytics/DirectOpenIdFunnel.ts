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
 *          start-webauthn-creation
 *            register-with-passkey
 *              successful-passkey-registration
 *                auth-success
 *                go-to-dashboard
 *        use-existing-passkey
 *          auth-success
 *          go-to-dashboard
 */
export const DirectOpenIdEvents = {
  RedirectToOpenId: "redirect-to-openid",
  CallbackFromOpenId: "callback-from-openid",
  RedirectToApp: "redirect-to-app",
} as const;

export const directOpenIdFunnel = new Funnel<typeof DirectOpenIdEvents>(
  "direct-openid",
  true,
);
