import { Funnel } from "./Funnel";

/**
 * Verified-email **setup** flow events. The user is already signed in
 * and is binding an email address as a verified email — a separate
 * primitive from the recovery email, surfaced to dapps as an
 * attribute source.
 *
 * Mirrors {@link setupEmailRecoveryFunnel} one-to-one: the canister-side
 * verification pipeline (DKIM / DMARC / DNSSEC / DoH) is shared between
 * recovery and verified-email setup, so the funnel shape is the same —
 * only the prefix differs so the two flows can be segmented separately
 * in analytics.
 *
 * See {@link setupEmailRecoveryFunnel} for the funnel shape; the events
 * below align one-to-one with the recovery events.
 */
export const SetupVerifiedEmailEvents = {
  AddressSubmitted: "verified-email-setup-address-submitted",
  Prepared: "verified-email-setup-prepared",
  UnsupportedDomain: "verified-email-setup-unsupported-domain",
  NeedDkimLeaf: "verified-email-setup-need-dkim-leaf",
  DkimLeafSubmitted: "verified-email-setup-dkim-leaf-submitted",
  Succeeded: "verified-email-setup-succeeded",
  Failed: "verified-email-setup-failed",
} as const;

export const setupVerifiedEmailFunnel = new Funnel<
  typeof SetupVerifiedEmailEvents
>("verified-email-setup");
