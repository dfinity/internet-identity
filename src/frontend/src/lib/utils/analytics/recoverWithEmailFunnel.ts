import { Funnel } from "./Funnel";

/**
 * Email-recovery **sign-in** flow events. The user is anonymous and
 * proving control of a previously-bound email address to obtain a
 * signed delegation.
 *
 * Square brackets [] indicate optional events.
 *
 * Funnel shape:
 *
 * start-email-recovery-recover    (INIT — recovery picker selects email branch)
 *   email-recovery-recover-address-submitted
 *     email-recovery-recover-prepared    (with `path = dnssec | doh`)
 *     | email-recovery-recover-unsupported-domain
 *     [email-recovery-recover-need-dkim-leaf]    (DNSSEC path only)
 *       [email-recovery-recover-dkim-leaf-submitted]
 *     email-recovery-recover-recovery-ready
 *       email-recovery-recover-delegation-retrieved
 *         email-recovery-recover-signed-in
 *     | email-recovery-recover-failed   (with `reason = <variant>`)
 * end-email-recovery-recover       (CLOSE — terminal state, carries `duration-…`)
 *
 * Two stages distinguish the recovery flow from setup:
 *  1. `recovery-ready` — the canister has stamped the delegation seed
 *     and the FE has resolved the anchor from the verified `From:`.
 *  2. `delegation-retrieved` — the FE has called
 *     `email_recovery_get_delegation` and now holds the
 *     `SignedDelegation`. The host page can build the
 *     `DelegationIdentity` from this point.
 *
 * `signed-in` fires once the host's `onSignedIn` callback has resolved
 * — i.e. the auth store is seeded and the user is actually logged in.
 *
 * The DoH path skips `need-dkim-leaf` / `dkim-leaf-submitted` — the
 * canister finishes verification synchronously inside `smtp_request`,
 * so the FE polls from `prepared` straight to `recovery-ready`.
 */
export const RecoverWithEmailEvents = {
  AddressSubmitted: "email-recovery-recover-address-submitted",
  Prepared: "email-recovery-recover-prepared",
  UnsupportedDomain: "email-recovery-recover-unsupported-domain",
  NeedDkimLeaf: "email-recovery-recover-need-dkim-leaf",
  DkimLeafSubmitted: "email-recovery-recover-dkim-leaf-submitted",
  RecoveryReady: "email-recovery-recover-recovery-ready",
  DelegationRetrieved: "email-recovery-recover-delegation-retrieved",
  SignedIn: "email-recovery-recover-signed-in",
  Failed: "email-recovery-recover-failed",
} as const;

export const recoverWithEmailFunnel = new Funnel<typeof RecoverWithEmailEvents>(
  "email-recovery-recover",
);
