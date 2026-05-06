<script lang="ts">
  /**
   * Setup wizard for binding an email address as a recovery method
   * to the currently-authenticated Internet Identity. Drives the
   * three-step flow described in `docs/ongoing/email-recovery.md`
   * §8.4:
   *
   *   1. User types their email address.
   *   2. We discover the DKIM selector + (optionally) assemble a
   *      DNSSEC bundle, call `email_recovery_credential_prepare_add`,
   *      and show the canister-issued nonce + recipient mailbox.
   *   3. User sends a DKIM-signed email containing that nonce; we
   *      poll `email_recovery_status` until it flips to
   *      `RegistrationSucceeded` (or terminal Failed/Expired).
   *
   * Both DNSSEC and DoH paths converge on the same canister method;
   * the wizard doesn't need to know which one the canister picked.
   */

  import EnterAddress from "./views/EnterAddress.svelte";
  import SendMagicEmail from "./views/SendMagicEmail.svelte";
  import Done from "./views/Done.svelte";
  import FailedView from "./views/FailedView.svelte";
  import type {
    EmailRecoveryChallenge,
    EmailRecoveryDnsInput,
    EmailRecoveryStatus,
    DnsProofBundle,
  } from "$lib/generated/internet_identity_types";
  import { discoverSelector, assembleBundle } from "$lib/utils/dnssec";

  interface Props {
    /** Authenticated wrapper around `email_recovery_credential_prepare_add`. */
    prepare: (
      input: EmailRecoveryDnsInput,
    ) => Promise<EmailRecoveryChallenge>;
    /** Anonymous wrapper around `email_recovery_status` (query). */
    status: (nonce: string) => Promise<EmailRecoveryStatus>;
    /** Wizard close — called on user-initiated cancel and on `Done`. */
    onClose: () => void;
  }

  const { prepare, status, onClose }: Props = $props();

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
      if ("SelectorMismatch" in reason) {
        return "Your email provider rotated its DKIM keys. Please retry.";
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

    // Best-effort selector discovery and DNSSEC bundle assembly.
    // If either step yields nothing (no DNSSEC, no published
    // selector candidates) we still let the canister try the DoH
    // path; only when *neither* domain config applies does the
    // user get a "DomainNotAllowlisted" error back.
    const selector = (await discoverSelector(domain)) ?? "default";
    let dnsProof: DnsProofBundle | undefined;
    try {
      dnsProof = await assembleBundle(domain, selector, true);
    } catch {
      // If chain assembly throws (malformed RRSIG etc.) just let
      // the canister fall through to the DoH path.
      dnsProof = undefined;
    }

    const input: EmailRecoveryDnsInput = {
      address,
      selector,
      dns_proof: dnsProof === undefined ? [] : [dnsProof],
    };

    const challenge = await prepare(input);
    stage = { kind: "sending", challenge, address };
    void runPoll(challenge.nonce, address);
  };

  const runPoll = async (nonce: string, address: string) => {
    if (polling) return;
    polling = true;
    let intervalMs = 1_000;
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
        // Pending: back off from 1 → 5 s.
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
