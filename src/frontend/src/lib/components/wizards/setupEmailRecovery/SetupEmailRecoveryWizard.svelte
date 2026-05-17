<script lang="ts">
  /**
   * Setup wizard for binding an email address as a recovery method
   * to the currently-authenticated Internet Identity. Drives the
   * flow described in `docs/ongoing/email-recovery.md` §8.4:
   *
   *   1. User types their email address.
   *   2. We assemble a DNSSEC *skeleton* bundle (chain + optional
   *      DMARC leaf — no DKIM leaf, the selector isn't yet known),
   *      call `email_recovery_credential_prepare_add`, and show the
   *      canister-issued nonce + recipient mailbox.
   *   3. User sends a DKIM-signed email containing that nonce; we
   *      poll `email_recovery_status`. The first non-Pending result
   *      is `NeedDkimLeaf { selector }` (DNSSEC path) — at which
   *      point we walk that one DKIM leaf and call
   *      `email_recovery_submit_dkim_leaf`. We then keep polling
   *      until status flips to `RegistrationSucceeded` (or terminal
   *      Failed/Expired). On the DoH path the canister finishes
   *      verification synchronously inside `smtp_request`, so we go
   *      straight from `Pending` to terminal — no `submit` step.
   *
   * On success the wizard hands control back to the host via
   * `onSuccess(address)`; the host fires a toast and closes the
   * dialog. There is no terminal "all set" view in the wizard.
   */

  import EnterAddress from "./views/EnterAddress.svelte";
  import SendConfirmationEmail from "./views/SendConfirmationEmail.svelte";
  import FailedView from "./views/FailedView.svelte";
  import UnsupportedDomain from "./views/UnsupportedDomain.svelte";
  import type {
    EmailRecoveryChallenge,
    EmailRecoveryDnsInput,
    EmailRecoveryError,
    EmailRecoveryStatus,
    EmailRecoverySubmitDkimLeafArg,
    DnsProofBundle,
  } from "$lib/generated/internet_identity_types";
  import {
    assembleSkeleton,
    assembleDkimResolution,
    type Path,
  } from "$lib/utils/dnssec";
  import { isCanisterError } from "$lib/utils/utils";
  import {
    setupEmailRecoveryFunnel,
    SetupEmailRecoveryEvents,
  } from "$lib/utils/analytics/setupEmailRecoveryFunnel";
  import { onDestroy, onMount } from "svelte";

  interface Props {
    /** Authenticated wrapper around `email_recovery_credential_prepare_add`. */
    prepare: (input: EmailRecoveryDnsInput) => Promise<EmailRecoveryChallenge>;
    /** Anonymous wrapper around `email_recovery_status` (query). */
    status: (nonce: string) => Promise<EmailRecoveryStatus>;
    /** Anonymous wrapper around `email_recovery_submit_dkim_leaf`. */
    submitDkimLeaf: (
      arg: EmailRecoverySubmitDkimLeafArg,
    ) => Promise<EmailRecoveryStatus>;
    /** Called once on `RegistrationSucceeded`. The host is expected to
     *  show a success toast and close the dialog. */
    onSuccess: (address: string) => void;
  }

  const { prepare, status, submitDkimLeaf, onSuccess }: Props = $props();

  type Stage =
    | { kind: "enter"; initialError?: string }
    | {
        kind: "sending";
        challenge: EmailRecoveryChallenge;
        address: string;
        path: Path;
      }
    | { kind: "unsupported"; domain: string }
    | { kind: "failed"; reason: string };

  let stage = $state<Stage>({ kind: "enter" });

  // Cancel the polling loop when the user navigates away mid-flow.
  // The Dialog's close (X) button unmounts this component without
  // going through any user-handler path, so we lean on `onDestroy`
  // to flip the flag — otherwise the closure's poll keeps ticking
  // against a dead component until status() naturally terminates.
  let polling = $state(false);
  onDestroy(() => {
    polling = false;
  });

  // Plausible funnel: emit `start-email-recovery-setup` when the
  // wizard mounts. Each milestone calls `trigger(...)`; the terminal
  // states (success / failed / unsupported) call `close()` to record
  // duration. See `setupEmailRecoveryFunnel.ts` for the event shape.
  onMount(() => {
    setupEmailRecoveryFunnel.init();
  });

  /**
   * Map a terminal `EmailRecoveryStatus` (`Failed` or `Expired`) to
   * the variant-name string used as the `reason` property on the
   * Plausible `*-failed` event. Falls back to `unknown` so a partial
   * candid-shape change doesn't drop the event entirely.
   */
  const failureReason = (variant: EmailRecoveryStatus): string => {
    if ("Failed" in variant) {
      const reason = variant.Failed as Record<string, unknown>;
      return Object.keys(reason)[0] ?? "unknown";
    }
    if ("Expired" in variant) {
      return "Expired";
    }
    return "unknown";
  };

  const friendlyError = (variant: EmailRecoveryStatus): string => {
    if ("Failed" in variant) {
      const reason = variant.Failed;
      if ("DomainNotAllowlisted" in reason) {
        // This shouldn't surface here normally — `handleAddressSubmitted`
        // routes the synchronous form to the unsupported view — but
        // keep the copy in case a delayed status flip carries it.
        return `Internet Identity can't verify mail from ${reason.DomainNotAllowlisted} yet. Try a different email or use a recovery phrase.`;
      }
      if ("DomainNotSupported" in reason) {
        return reason.DomainNotSupported;
      }
      if ("AddressMismatch" in reason) {
        return "The email came from a different address than the one we have on file.";
      }
      if ("SubjectNotSigned" in reason) {
        return "Your email provider didn't sign the Subject header. Try a different provider.";
      }
      if ("DkimLeafMismatch" in reason) {
        return "Your email provider rotated its DKIM keys mid-flow. Please retry.";
      }
      if ("NoDkimLeafExpected" in reason) {
        return "Internal error: the DKIM leaf was submitted at the wrong moment. Please retry.";
      }
      if ("AddressAlreadyRegistered" in reason) {
        return "This email is already used to recover a different identity.";
      }
      if ("EmailVerificationFailed" in reason) {
        return `Your email didn't verify (${reason.EmailVerificationFailed}). Make sure you sent it from the address you typed, no forwarding, no aliases.`;
      }
      if ("InternalCanisterError" in reason) {
        return `Something went wrong on our end: ${reason.InternalCanisterError}`;
      }
      return Object.keys(reason)[0];
    }
    if ("Expired" in variant) {
      return "This recovery link timed out. Please try again.";
    }
    return "Unexpected status from the canister.";
  };

  const handleAddressSubmitted = async (address: string) => {
    setupEmailRecoveryFunnel.trigger(SetupEmailRecoveryEvents.AddressSubmitted);
    const domain = address.split("@")[1] ?? "";

    // Best-effort DNSSEC skeleton-bundle assembly. The DKIM leaf is
    // *not* fetched here — its selector lives only inside the
    // eventual email's `DKIM-Signature: s=` tag, so we walk that
    // leaf later (after polling sees `NeedDkimLeaf`). If the chain
    // assembly yields nothing (no DNSSEC) we still let the canister
    // try the DoH path; only when *neither* domain config applies
    // does the user get a "DomainNotAllowlisted" error back, which
    // we translate into the unsupported-domain view.
    let dnsProof: DnsProofBundle | undefined;
    try {
      dnsProof = await assembleSkeleton(domain, true);
    } catch {
      dnsProof = undefined;
    }
    const path: Path = dnsProof === undefined ? "doh" : "dnssec";

    const input: EmailRecoveryDnsInput = {
      address,
      dns_proof: dnsProof === undefined ? [] : [dnsProof],
    };

    try {
      const challenge = await prepare(input);
      setupEmailRecoveryFunnel.trigger(SetupEmailRecoveryEvents.Prepared, {
        path,
      });
      stage = { kind: "sending", challenge, address, path };
      void runPoll(challenge.nonce, domain, address);
    } catch (e) {
      // `DomainNotAllowlisted` / `DomainNotSupported` at prepare
      // time means we've got neither DNSSEC nor an allowlist entry
      // — route to the dedicated unsupported view so the user gets
      // the technical "why + how to fix" treatment instead of an
      // opaque inline error string.
      if (isCanisterError<EmailRecoveryError>(e)) {
        if (
          e.type === "DomainNotAllowlisted" ||
          e.type === "DomainNotSupported"
        ) {
          setupEmailRecoveryFunnel.trigger(
            SetupEmailRecoveryEvents.UnsupportedDomain,
            { reason: e.type },
          );
          setupEmailRecoveryFunnel.close();
          stage = { kind: "unsupported", domain };
          return;
        }
      }
      // Anything else propagates to EnterAddress's inline error.
      throw e;
    }
  };

  const runPoll = async (nonce: string, domain: string, address: string) => {
    if (polling) return;
    polling = true;
    let intervalMs = 1_000;
    let dkimLeafSubmitted = false;
    try {
      while (polling && stage.kind === "sending") {
        const result = await status(nonce);
        if ("RegistrationSucceeded" in result) {
          setupEmailRecoveryFunnel.trigger(SetupEmailRecoveryEvents.Succeeded);
          setupEmailRecoveryFunnel.close();
          polling = false;
          onSuccess(address);
          return;
        }
        if ("Failed" in result || "Expired" in result) {
          setupEmailRecoveryFunnel.trigger(SetupEmailRecoveryEvents.Failed, {
            reason: failureReason(result),
          });
          setupEmailRecoveryFunnel.close();
          stage = { kind: "failed", reason: friendlyError(result) };
          return;
        }
        if ("NeedDkimLeaf" in result && !dkimLeafSubmitted) {
          dkimLeafSubmitted = true;
          setupEmailRecoveryFunnel.trigger(
            SetupEmailRecoveryEvents.NeedDkimLeaf,
          );
          // Email arrived; the canister has the selector. Walk the
          // single missing DKIM leaf and submit it. If the leaf
          // walk fails we keep the loop alive — the canister-side
          // entry will time out and the user will see Expired.
          const selector = result.NeedDkimLeaf.selector;
          try {
            const walked = await assembleDkimResolution(domain, selector);
            if (walked !== undefined) {
              const submission = await submitDkimLeaf({
                nonce,
                hops: walked.hops,
                extra_chains: walked.extraChains,
              });
              setupEmailRecoveryFunnel.trigger(
                SetupEmailRecoveryEvents.DkimLeafSubmitted,
              );
              if ("RegistrationSucceeded" in submission) {
                setupEmailRecoveryFunnel.trigger(
                  SetupEmailRecoveryEvents.Succeeded,
                );
                setupEmailRecoveryFunnel.close();
                polling = false;
                onSuccess(address);
                return;
              }
              if ("Failed" in submission || "Expired" in submission) {
                setupEmailRecoveryFunnel.trigger(
                  SetupEmailRecoveryEvents.Failed,
                  { reason: failureReason(submission) },
                );
                setupEmailRecoveryFunnel.close();
                stage = { kind: "failed", reason: friendlyError(submission) };
                return;
              }
            }
          } catch {
            // Submit failed; fall through to keep polling so the
            // user sees a clean Expired if the canister times out.
          }
        }
        // Pending or NeedDkimLeaf-but-already-submitted: back off
        // from 1 → 5 s.
        await new Promise((r) => setTimeout(r, intervalMs));
        intervalMs = Math.min(5_000, intervalMs * 1.5);
      }
    } finally {
      polling = false;
    }
  };

  const handleRetry = () => {
    stage = { kind: "enter" };
  };
</script>

{#if stage.kind === "enter"}
  <EnterAddress
    onSubmit={handleAddressSubmitted}
    initialError={stage.initialError}
  />
{:else if stage.kind === "sending"}
  <SendConfirmationEmail
    nonce={stage.challenge.nonce}
    mailbox={`register@${window.location.hostname}`}
    fromAddress={stage.address}
    path={stage.path}
  />
{:else if stage.kind === "unsupported"}
  <UnsupportedDomain domain={stage.domain} onRetry={handleRetry} />
{:else}
  <FailedView reason={stage.reason} onRetry={handleRetry} />
{/if}
