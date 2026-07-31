import { Funnel } from "./Funnel";

/**
 * Verified-email **consent / share** flow events. The user is signed in
 * and a relying party has requested `email` / `verified_email` on the
 * authorize / attribute-consent screen. Complements
 * {@link setupVerifiedEmailFunnel}, which measures *binding* an
 * address; this funnel measures whether that address ever gets shared
 * with a dapp.
 *
 * Funnel shape:
 *
 * start-verified-email-consent    (INIT — consent view resolves with
 *                                  verified_email/email in the request)
 *   verified-email-consent-empty-state-shown   (no addresses bound yet)
 *     verified-email-consent-verify-clicked    (user opens the inline wizard)
 *     | verified-email-consent-skipped         (user dismisses with "Skip for now")
 *   verified-email-consent-denied-all          (clicked the "Deny all" link)
 *   verified-email-consent-shared              (clicked Continue with at
 *                                               least one email-shaped
 *                                               attribute selected, with
 *                                               `source = unscoped | openid | sso`)
 * end-verified-email-consent       (CLOSE — terminal state, carries `duration-…`)
 */
export const VerifiedEmailConsentEvents = {
  EmptyStateShown: "verified-email-consent-empty-state-shown",
  VerifyClicked: "verified-email-consent-verify-clicked",
  Skipped: "verified-email-consent-skipped",
  DeniedAll: "verified-email-consent-denied-all",
  Shared: "verified-email-consent-shared",
} as const;

export const verifiedEmailConsentFunnel = new Funnel<
  typeof VerifiedEmailConsentEvents
>("verified-email-consent");
