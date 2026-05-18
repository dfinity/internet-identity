import { Funnel } from "./Funnel";

/**
 * Email-recovery **setup** flow events. The user is already signed in
 * and is binding an email address as a recovery method.
 *
 * Square brackets [] indicate optional events.
 *
 * Funnel shape:
 *
 * start-email-recovery-setup     (INIT — manage page opens the wizard)
 *   email-recovery-setup-address-submitted
 *     email-recovery-setup-prepared    (with `path = dnssec | doh`)
 *     | email-recovery-setup-unsupported-domain
 *     [email-recovery-setup-need-dkim-leaf]     (DNSSEC path only)
 *       [email-recovery-setup-dkim-leaf-submitted]
 *     email-recovery-setup-succeeded
 *     | email-recovery-setup-failed   (with `reason = <variant>`)
 * end-email-recovery-setup           (CLOSE — terminal state, carries `duration-…`)
 *
 * The DoH path skips `need-dkim-leaf` / `dkim-leaf-submitted` — the
 * canister finishes verification synchronously inside `smtp_request`,
 * so the FE polls from `prepared` straight to `succeeded` / `failed`.
 *
 * The wizard never explicitly tracks "email sent" — the canister
 * doesn't know, and the FE's view of it is `NeedDkimLeaf` (DNSSEC)
 * or the next non-`Pending` poll (DoH). Both are captured above.
 */
export const SetupEmailRecoveryEvents = {
  AddressSubmitted: "email-recovery-setup-address-submitted",
  Prepared: "email-recovery-setup-prepared",
  UnsupportedDomain: "email-recovery-setup-unsupported-domain",
  NeedDkimLeaf: "email-recovery-setup-need-dkim-leaf",
  DkimLeafSubmitted: "email-recovery-setup-dkim-leaf-submitted",
  Succeeded: "email-recovery-setup-succeeded",
  Failed: "email-recovery-setup-failed",
} as const;

export const setupEmailRecoveryFunnel = new Funnel<
  typeof SetupEmailRecoveryEvents
>("email-recovery-setup");
