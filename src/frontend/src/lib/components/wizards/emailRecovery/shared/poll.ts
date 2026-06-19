/**
 * Shared polling state machine for the email-recovery wizards.
 *
 * Both the setup wizard (binding an email to the current identity) and
 * the recovery wizard (proving control of a bound email to sign in)
 * drive the *same* canister-side flow: send a DKIM-signed email, poll
 * `email_recovery_status`, and — on the DNSSEC path — resolve the one
 * missing DKIM leaf and submit it. The only real differences are the
 * terminal *success* handling (bind credential vs. retrieve delegation)
 * and which analytics funnel fires; everything else (backoff, the leaf
 * walk, the DoH fallback, terminal-failure routing, the transport-error
 * catch) is identical and lives here.
 *
 * The caller injects the wizard-specific bits via {@link EmailRecoveryPollDeps}.
 */

import type {
  EmailRecoveryDiagnostics,
  EmailRecoveryError,
  EmailRecoveryStatus,
  EmailRecoverySubmitDkimLeafArg,
} from "$lib/generated/internet_identity_types";
import type { Funnel } from "$lib/utils/analytics/Funnel";
import { assembleDkimResolution } from "$lib/utils/dnssec";
import { isCanisterError } from "$lib/utils/utils";
import { buildDiagnosticsBlob } from "./diagnostics";
import {
  dohSubReason,
  EXPIRED_MESSAGE,
  friendlyFailedReason,
  plausibleFailureReason,
} from "./errors";

/** Shown when a non-canister error (transport failure, dropped
 *  response) leaves the canister state unknown after our single submit
 *  attempt is already spent. */
const SUBMIT_FAILED_MESSAGE =
  "We couldn't reach Internet Identity to verify your email. Please try again.";

/** How many consecutive transient `status` poll failures to ride out
 *  before surfacing a retryable failure — bounds the network-outage case
 *  so the loop can't poll forever, while tolerating brief blips. */
const MAX_CONSECUTIVE_POLL_ERRORS = 5;

export interface EmailRecoveryPollDeps<E extends Record<string, string>> {
  /** Challenge nonce from prepare; also the email's `Subject:`. */
  nonce: string;
  /** Domain part of the claimed address, for the DKIM leaf walk and the
   *  unsupported-domain view. */
  domain: string;

  // --- canister wrappers ------------------------------------------------
  /** `email_recovery_status` (query). */
  status: (nonce: string) => Promise<EmailRecoveryStatus>;
  /** `email_recovery_submit_dkim_leaf` — the DNSSEC path (≥1 hop). Accepts
   *  the leaf and returns; the verdict is read by polling `status`. Rejects
   *  only on a call-level error (unknown nonce / wrong state). */
  submitDkimLeaf: (arg: EmailRecoverySubmitDkimLeafArg) => Promise<void>;
  /** `email_recovery_resolve_via_doh` — resolves the DKIM key over the
   *  canister's DoH path. Driven repeatedly while the status is
   *  `ResolvingDoh` (the pure-DoH/Gmail case, and the fallback when a DNSSEC
   *  leaf CNAMEs into an unsigned zone). Carries only the nonce; idempotent
   *  and accept-only — poll `status` for the verdict. */
  resolveViaDoh: (nonce: string) => Promise<void>;
  /** `email_recovery_diagnostics` (query). */
  diagnostics: (nonce: string) => Promise<[] | [EmailRecoveryDiagnostics]>;

  // --- analytics --------------------------------------------------------
  funnel: Funnel<E>;
  /** The flow-specific event names this loop emits. */
  events: {
    needDkimLeaf: E[keyof E];
    dkimLeafSubmitted: E[keyof E];
    failed: E[keyof E];
    unsupportedDomain: E[keyof E];
  };

  // --- wizard-specific hooks -------------------------------------------
  /**
   * Handle a terminal *success* status. Returns `true` if `status` was a
   * terminal success (the loop then stops); `false` for any
   * non-terminal status (`Pending` / `ResolvingDoh` / `NeedDkimLeaf`), so the
   * loop keeps going. Setup checks `RegistrationSucceeded`; recovery
   * checks `RecoveryReady` and awaits the delegation retrieval.
   */
  handleSuccess: (status: EmailRecoveryStatus) => boolean | Promise<boolean>;
  /** Whether the loop should keep running: the polling flag is set AND
   *  the wizard is still in a sending/waiting stage. */
  isActive: () => boolean;
  /** Flip the wizard's polling flag (cleared in a `finally`). */
  setPolling: (active: boolean) => void;
  /** Enter the dedicated unsupported-domain stage. */
  toUnsupported: (domain: string) => void;
  /** Enter the failed stage with friendly copy + a diagnostics blob. */
  toFailed: (reason: string, diagnostics: string) => void;
}

/**
 * Run the poll loop to a terminal state. The caller owns the polling
 * flag and must guard against double-starting (`if (polling) return`)
 * before calling; this function clears the flag when it returns.
 */
export const runEmailRecoveryPoll = async <E extends Record<string, string>>(
  deps: EmailRecoveryPollDeps<E>,
): Promise<void> => {
  const {
    nonce,
    domain,
    status,
    submitDkimLeaf,
    resolveViaDoh,
    diagnostics,
    funnel,
    events,
    handleSuccess,
    isActive,
    setPolling,
    toUnsupported,
    toFailed,
  } = deps;

  // Best-effort fetch of the canister's strictly-public diagnostics for
  // this challenge, formatted into a copyable blob (incl. the gateway
  // `message_id`). The pending entry is sticky on `Failed`, so this read
  // lands while it's still present; on any hiccup we fall back to
  // FE-only fields so the user always gets something to share.
  const collectDiagnostics = async (): Promise<string> => {
    try {
      return buildDiagnosticsBlob((await diagnostics(nonce))[0]);
    } catch {
      return buildDiagnosticsBlob(undefined);
    }
  };

  // Route a terminal `Failed`/`Expired` status to the right view. A
  // DomainNotAllowlisted / DomainNotSupported verdict gets the dedicated
  // unsupported-domain view (matching the prepare-time routing); every
  // other reason gets the generic failed view with diagnostics.
  const handleTerminalFailure = async (
    terminal: { Failed: EmailRecoveryError } | { Expired: null },
  ): Promise<void> => {
    if ("Failed" in terminal) {
      const reason = terminal.Failed;
      if ("DomainNotAllowlisted" in reason || "DomainNotSupported" in reason) {
        funnel.trigger(events.unsupportedDomain, {
          reason: plausibleFailureReason(terminal),
        });
        funnel.close();
        toUnsupported(domain);
        return;
      }
      // `reason` is now narrowed to `FailedReason` (the two domain
      // variants are routed away above), so `friendlyFailedReason`
      // accepts it directly. When the failure came from the DoH path,
      // attach the granular `doh_reason` token so the funnel is
      // segmentable by *why* DoH failed (see `dohSubReason`).
      const dohReason = dohSubReason(terminal);
      funnel.trigger(events.failed, {
        reason: plausibleFailureReason(terminal),
        ...(dohReason !== undefined ? { doh_reason: dohReason } : {}),
      });
      funnel.close();
      toFailed(friendlyFailedReason(reason), await collectDiagnostics());
      return;
    }
    funnel.trigger(events.failed, { reason: "Expired" });
    funnel.close();
    toFailed(EXPIRED_MESSAGE, await collectDiagnostics());
  };

  let intervalMs = 1_000;
  let dkimLeafSubmitted = false;
  let consecutivePollErrors = 0;
  const backOff = async () => {
    await new Promise((r) => setTimeout(r, intervalMs));
    intervalMs = Math.min(5_000, intervalMs * 1.5);
  };
  try {
    while (isActive()) {
      let result: EmailRecoveryStatus;
      try {
        result = await status(nonce);
        consecutivePollErrors = 0;
      } catch {
        // A transient query failure (network blip) leaves canister state
        // unchanged, so retry on the next tick rather than letting the
        // throw kill the loop and strand the user on the waiting screen
        // with no 30-minute Expired backstop (we'd have stopped polling).
        // Give up — with a retryable failure — only once several polls in
        // a row fail, i.e. a real outage rather than a blip.
        if (++consecutivePollErrors < MAX_CONSECUTIVE_POLL_ERRORS) {
          await backOff();
          continue;
        }
        funnel.trigger(events.failed, { reason: "PollFailed" });
        funnel.close();
        toFailed(SUBMIT_FAILED_MESSAGE, await collectDiagnostics());
        return;
      }

      if (await handleSuccess(result)) {
        return;
      }
      if ("Failed" in result || "Expired" in result) {
        await handleTerminalFailure(result);
        return;
      }
      if ("NeedDkimLeaf" in result && !dkimLeafSubmitted) {
        dkimLeafSubmitted = true;
        funnel.trigger(events.needDkimLeaf);
        // Walk the one missing DKIM leaf via DNSSEC. When the record
        // CNAMEs into an unsigned zone (outlook.com ->
        // outbound.protection.outlook.com) the walk yields `undefined`;
        // we then ask the canister to resolve the key over its own
        // (allowlist-gated) DoH path instead of leaving the entry stuck
        // at NeedDkimLeaf until it expires.
        const selector = result.NeedDkimLeaf.selector;
        try {
          const walked = await assembleDkimResolution(domain, selector);
          if (walked !== undefined) {
            await submitDkimLeaf({
              nonce,
              hops: walked.hops,
              extra_chains: walked.extraChains,
            });
          } else {
            // The leaf CNAMEs into an unsigned zone — switch to the DoH
            // path. This first call flips the status to `ResolvingDoh`; the
            // `ResolvingDoh` branch below then drives it to a verdict.
            await resolveViaDoh(nonce);
          }
          funnel.trigger(events.dkimLeafSubmitted);
          // The submit only *accepts* — it carries no verdict. The pending
          // status is the single source of truth, so just fall through and
          // let the polling loop carry the flow to its terminal state.
        } catch (e) {
          // A canister `Err` IS the terminal verdict — the contract hands
          // it to us directly, so route it now (exactly as an
          // `Ok(Failed)` status is routed: DomainNotAllowlisted /
          // DomainNotSupported to the unsupported view, everything else to
          // the failed view) instead of discarding it and re-polling
          // canister state we don't own.
          if (isCanisterError<EmailRecoveryError>(e)) {
            await handleTerminalFailure({ Failed: e.raw });
            return;
          }
          // A non-canister error (transport failure, dropped response, an
          // unexpected throw in the leaf walk) leaves the canister state
          // unknown, and our single submit attempt is already spent
          // (`dkimLeafSubmitted`), so the poll loop would never re-submit.
          // Surface a retryable failure instead of staying silent.
          funnel.trigger(events.failed, { reason: "SubmitFailed" });
          funnel.close();
          toFailed(SUBMIT_FAILED_MESSAGE, await collectDiagnostics());
          return;
        }
      }
      if ("ResolvingDoh" in result) {
        // Drive the canister's DoH key resolution. It's idempotent, so it's
        // safe to call on each tick while the cache fetch is in flight — but
        // a *thrown* error is a real failure to handle, not something to
        // retry into oblivion. Mirror the submit path: a canister `Err` (the
        // challenge is gone) is terminal, routed like an `Ok(Failed)` status;
        // a non-canister error (transport failure) leaves the outcome unknown,
        // so surface a retryable failure. Either way, stop — don't re-poll.
        try {
          await resolveViaDoh(nonce);
        } catch (e) {
          if (isCanisterError<EmailRecoveryError>(e)) {
            await handleTerminalFailure({ Failed: e.raw });
            return;
          }
          funnel.trigger(events.failed, { reason: "ResolveFailed" });
          funnel.close();
          toFailed(SUBMIT_FAILED_MESSAGE, await collectDiagnostics());
          return;
        }
      }
      await backOff();
    }
  } finally {
    setPolling(false);
  }
};
