<script lang="ts">
  /**
   * Setup wizard for binding an email address as a **verified email**
   * to the currently-authenticated Internet Identity. Parallel to
   * {@link SetupEmailRecoveryWizard} — both flows ride the same SMTP
   * gateway, DKIM verifier and DMARC alignment; the only differences
   * are which canister method binds the entry, which `Anchor` field
   * it lands on, and the nonce prefix (`II-Verify-` here vs
   * `II-Recovery-` on the recovery wizard).
   *
   * Flow mirrors the recovery setup wizard:
   *
   *   1. User types their email address.
   *   2. Assemble a DNSSEC skeleton bundle, call
   *      `verified_email_prepare_add`, show the canister-issued nonce
   *      + recipient mailbox (`register@<related_origin>` — same
   *      recipient as the recovery setup flow; the `II-Verify-`
   *      subject prefix is what disambiguates the two on the canister
   *      side).
   *   3. User sends the DKIM-signed email; we poll
   *      `email_challenge_status` (shared with recovery — keyed by
   *      nonce, not by flow). On the DNSSEC path that yields a
   *      `NeedDkimLeaf` step; on the DoH path verification finishes
   *      synchronously and the loop goes straight to terminal.
   *
   * On success the wizard hands control back to the host via
   * `onSuccess(address)`; the host fires a toast and closes the
   * dialog.
   */

  import EnterAddress from "./views/EnterAddress.svelte";
  import SendConfirmationEmail from "$lib/components/wizards/emailRecovery/shared/views/SendConfirmationEmail.svelte";
  import SuccessView from "$lib/components/wizards/emailRecovery/shared/views/SuccessView.svelte";
  import FailedView from "$lib/components/wizards/emailRecovery/shared/views/FailedView.svelte";
  import UnsupportedDomain from "$lib/components/wizards/emailRecovery/shared/views/UnsupportedDomain.svelte";
  import { runEmailRecoveryPoll } from "$lib/components/wizards/emailRecovery/shared/poll";
  import type {
    EmailChallenge,
    EmailChallengeDiagnostics,
    EmailChallengeDnsInput,
    EmailChallengeError,
    EmailChallengeStatus,
    EmailChallengeSubmitDkimLeafArg,
    DnsProofBundle,
  } from "$lib/generated/internet_identity_types";
  import { assembleSkeleton, type Path } from "$lib/utils/dnssec";
  import { isCanisterError } from "$lib/utils/utils";
  import { t } from "$lib/stores/locale.store";
  import {
    setupVerifiedEmailFunnel,
    SetupVerifiedEmailEvents,
  } from "$lib/utils/analytics/setupVerifiedEmailFunnel";
  import { onDestroy, onMount } from "svelte";

  interface Props {
    /** Authenticated wrapper around `verified_email_prepare_add`. */
    prepare: (input: EmailChallengeDnsInput) => Promise<EmailChallenge>;
    /** Anonymous wrapper around `email_challenge_status` (query). Status,
     *  diagnostics, submit-dkim-leaf and resolve-via-doh are shared with
     *  the recovery flow — keyed by nonce. */
    status: (nonce: string) => Promise<EmailChallengeStatus>;
    /** Anonymous wrapper around `email_challenge_diagnostics` (query). */
    diagnostics: (nonce: string) => Promise<[] | [EmailChallengeDiagnostics]>;
    /** Anonymous wrapper around `email_challenge_submit_dkim_leaf`. */
    submitDkimLeaf: (arg: EmailChallengeSubmitDkimLeafArg) => Promise<void>;
    /** Anonymous wrapper around `email_challenge_resolve_via_doh`. */
    resolveViaDoh: (nonce: string) => Promise<void>;
    /** Address to pre-fill (Phase 1.5 "Verify from unverified" entry). */
    initialAddress?: string;
    /** When true, the address input is locked to `initialAddress`. */
    addressLocked?: boolean;
    /** Pool of addresses bound to the user's recovery email slot — the
     *  wizard surfaces a non-blocking heads-up when the typed address
     *  matches one of them. */
    recoveryAddresses?: string[];
    /** Addresses already in the anchor's verified-emails bucket — used
     *  to block a duplicate submit client-side. */
    verifiedAddresses?: string[];
    /** Called once on `RegistrationSucceeded`. The host shows a toast
     *  and closes the dialog. */
    onSuccess: (address: string) => void;
  }

  const {
    prepare,
    status,
    diagnostics,
    submitDkimLeaf,
    resolveViaDoh,
    initialAddress,
    addressLocked = false,
    recoveryAddresses = [],
    verifiedAddresses = [],
    onSuccess,
  }: Props = $props();

  type Stage =
    | { kind: "enter"; initialError?: string }
    | {
        kind: "sending";
        challenge: EmailChallenge;
        address: string;
        path: Path;
      }
    | {
        kind: "waiting";
        challenge: EmailChallenge;
        address: string;
        path: Path;
      }
    | { kind: "succeeded"; address: string }
    | { kind: "unsupported"; domain: string }
    | { kind: "failed"; reason: string; diagnostics?: string };

  let stage = $state<Stage>({ kind: "enter" });

  let polling = $state(false);
  onDestroy(() => {
    polling = false;
  });

  onMount(() => {
    setupVerifiedEmailFunnel.init();
  });
  onDestroy(() => {
    setupVerifiedEmailFunnel.close();
  });

  const handleAddressSubmitted = async (address: string) => {
    setupVerifiedEmailFunnel.trigger(SetupVerifiedEmailEvents.AddressSubmitted);
    const domain = address.split("@")[1] ?? "";

    let dnsProof: DnsProofBundle | undefined;
    try {
      dnsProof = await assembleSkeleton(domain, true);
    } catch {
      dnsProof = undefined;
    }
    const path: Path = dnsProof === undefined ? "doh" : "dnssec";

    const input: EmailChallengeDnsInput = {
      address,
      dns_proof: dnsProof === undefined ? [] : [dnsProof],
    };

    try {
      const challenge = await prepare(input);
      setupVerifiedEmailFunnel.trigger(SetupVerifiedEmailEvents.Prepared, {
        path,
      });
      stage = { kind: "sending", challenge, address, path };
      void runPoll(challenge.nonce, domain, address);
    } catch (e) {
      if (isCanisterError<EmailChallengeError>(e)) {
        if (
          e.type === "DomainNotAllowlisted" ||
          e.type === "DomainNotSupported"
        ) {
          setupVerifiedEmailFunnel.trigger(
            SetupVerifiedEmailEvents.UnsupportedDomain,
            { reason: e.type },
          );
          setupVerifiedEmailFunnel.close();
          stage = { kind: "unsupported", domain };
          return;
        }
        // Prepare-time variants that the EnterAddress view surfaces
        // inline rather than via a dedicated stage.
        if (e.type === "LimitReached") {
          const { limit } = e.value("LimitReached");
          throw new Error(
            $t`You've reached the limit of ${limit} verified emails. Remove one to add another.`,
          );
        }
        if (e.type === "InvalidEmailAddress") {
          throw new Error($t`This doesn't look like a valid email address.`);
        }
      }
      throw e;
    }
  };

  const runPoll = async (nonce: string, domain: string, address: string) => {
    if (polling) return;
    polling = true;
    await runEmailRecoveryPoll({
      nonce,
      domain,
      status,
      submitDkimLeaf,
      resolveViaDoh,
      diagnostics,
      funnel: setupVerifiedEmailFunnel,
      events: {
        needDkimLeaf: SetupVerifiedEmailEvents.NeedDkimLeaf,
        dkimLeafSubmitted: SetupVerifiedEmailEvents.DkimLeafSubmitted,
        failed: SetupVerifiedEmailEvents.Failed,
        unsupportedDomain: SetupVerifiedEmailEvents.UnsupportedDomain,
      },
      handleSuccess: (result) => {
        if (!("RegistrationSucceeded" in result)) return false;
        setupVerifiedEmailFunnel.trigger(SetupVerifiedEmailEvents.Succeeded);
        setupVerifiedEmailFunnel.close();
        // Park the wizard on the success step; the host's
        // `onSuccess` fires once the user dismisses it via Done so
        // toast + close happen together.
        stage = { kind: "succeeded", address };
        return true;
      },
      isActive: () =>
        polling && (stage.kind === "sending" || stage.kind === "waiting"),
      setPolling: (active) => {
        polling = active;
      },
      toUnsupported: (d) => {
        stage = { kind: "unsupported", domain: d };
      },
      toFailed: (reason, diagnostics) => {
        stage = { kind: "failed", reason, diagnostics };
      },
    });
  };

  const handleRetry = () => {
    stage = { kind: "enter" };
  };

  const handleSent = () => {
    if (stage.kind !== "sending") return;
    stage = {
      kind: "waiting",
      challenge: stage.challenge,
      address: stage.address,
      path: stage.path,
    };
  };
</script>

{#if stage.kind === "enter"}
  <EnterAddress
    onSubmit={handleAddressSubmitted}
    {initialAddress}
    {addressLocked}
    {recoveryAddresses}
    {verifiedAddresses}
    initialError={stage.initialError}
  />
{:else if stage.kind === "sending"}
  <SendConfirmationEmail
    nonce={stage.challenge.nonce}
    mailbox={`register@${window.location.hostname}`}
    fromAddress={stage.address}
    path={stage.path}
    onSent={handleSent}
  />
{:else if stage.kind === "waiting"}
  <SendConfirmationEmail
    nonce={stage.challenge.nonce}
    mailbox={`register@${window.location.hostname}`}
    fromAddress={stage.address}
    path={stage.path}
    sent
  />
{:else if stage.kind === "succeeded"}
  {@const succeededAddress = stage.address}
  <SuccessView
    address={succeededAddress}
    flow="verified"
    onDone={() => onSuccess(succeededAddress)}
  />
{:else if stage.kind === "unsupported"}
  <UnsupportedDomain domain={stage.domain} onRetry={handleRetry} />
{:else}
  <FailedView
    reason={stage.reason}
    diagnostics={stage.diagnostics}
    onRetry={handleRetry}
  />
{/if}
