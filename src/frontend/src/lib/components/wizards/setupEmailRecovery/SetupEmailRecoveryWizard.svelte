<script lang="ts">
  /**
   * Setup wizard for binding an email address as a recovery method
   * to the currently-authenticated Internet Identity. Drives the
   * three-step flow described in `docs/ongoing/email-recovery.md`
   * §8.4:
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
   */

  import EnterAddress from "./views/EnterAddress.svelte";
  import SendMagicEmail from "./views/SendMagicEmail.svelte";
  import Done from "./views/Done.svelte";
  import FailedView from "./views/FailedView.svelte";
  import type {
    EmailRecoveryChallenge,
    EmailRecoveryDnsInput,
    EmailRecoveryStatus,
    EmailRecoverySubmitDkimLeafArg,
    DnsProofBundle,
  } from "$lib/generated/internet_identity_types";
  import { assembleSkeleton, assembleDkimResolution } from "$lib/utils/dnssec";

  interface Props {
    /** Authenticated wrapper around `email_recovery_credential_prepare_add`. */
    prepare: (input: EmailRecoveryDnsInput) => Promise<EmailRecoveryChallenge>;
    /** Anonymous wrapper around `email_recovery_status` (query). */
    status: (nonce: string) => Promise<EmailRecoveryStatus>;
    /** Anonymous wrapper around `email_recovery_submit_dkim_leaf`. */
    submitDkimLeaf: (
      arg: EmailRecoverySubmitDkimLeafArg,
    ) => Promise<EmailRecoveryStatus>;
    /** Wizard close — called on user-initiated cancel and on `Done`. */
    onClose: () => void;
  }

  const { prepare, status, submitDkimLeaf, onClose }: Props = $props();

  type Stage =
    | { kind: "enter"; initialError?: string }
    | {
        kind: "sending";
        challenge: EmailRecoveryChallenge;
        address: string;
      }
    | { kind: "done"; address: string }
    | { kind: "failed"; reason: string };

  let stage = $state<Stage>({ kind: "enter" });

  // Cancel the polling loop when the user navigates away mid-flow.
  let polling = $state(false);

  const friendlyError = (variant: EmailRecoveryStatus): string => {
    if ("Failed" in variant) {
      const reason = variant.Failed;
      // The variants are produced canister-side; we map the most
      // common ones to copy the user can act on. Anything else
      // falls through to the variant key.
      if ("DomainNotAllowlisted" in reason) {
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
    const domain = address.split("@")[1] ?? "";

    // Best-effort DNSSEC skeleton-bundle assembly. The DKIM leaf is
    // *not* fetched here — its selector lives only inside the
    // eventual email's `DKIM-Signature: s=` tag, so we walk that
    // leaf later (after polling sees `NeedDkimLeaf`). If the chain
    // assembly yields nothing (no DNSSEC) we still let the canister
    // try the DoH path; only when *neither* domain config applies
    // does the user get a "DomainNotAllowlisted" error back.
    let dnsProof: DnsProofBundle | undefined;
    try {
      dnsProof = await assembleSkeleton(domain, true);
    } catch {
      // If chain assembly throws (malformed RRSIG etc.) just let
      // the canister fall through to the DoH path.
      dnsProof = undefined;
    }

    const input: EmailRecoveryDnsInput = {
      address,
      dns_proof: dnsProof === undefined ? [] : [dnsProof],
    };

    const challenge = await prepare(input);
    stage = { kind: "sending", challenge, address };
    void runPoll(challenge.nonce, domain, address);
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
          stage = { kind: "done", address };
          return;
        }
        if ("Failed" in result || "Expired" in result) {
          stage = { kind: "failed", reason: friendlyError(result) };
          return;
        }
        if ("NeedDkimLeaf" in result && !dkimLeafSubmitted) {
          dkimLeafSubmitted = true;
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
              if ("RegistrationSucceeded" in submission) {
                stage = { kind: "done", address };
                return;
              }
              if ("Failed" in submission || "Expired" in submission) {
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

  const handleCancel = () => {
    polling = false;
    onClose();
  };
</script>

{#if stage.kind === "enter"}
  <EnterAddress
    onSubmit={handleAddressSubmitted}
    onCancel={handleCancel}
    initialError={stage.initialError}
  />
{:else if stage.kind === "sending"}
  <SendMagicEmail
    nonce={stage.challenge.nonce}
    mailbox={stage.challenge.mailbox}
    fromAddress={stage.address}
    expiresAt={stage.challenge.expires_at}
    onCancel={handleCancel}
  />
{:else if stage.kind === "done"}
  <Done address={stage.address} onDone={onClose} />
{:else}
  <FailedView
    reason={stage.reason}
    onRetry={handleRetry}
    onCancel={handleCancel}
  />
{/if}
