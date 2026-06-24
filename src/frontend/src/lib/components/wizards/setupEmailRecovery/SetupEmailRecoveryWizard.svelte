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
   *      poll the challenge-status query. The first non-Pending result
   *      is `NeedDkimLeaf { selector }` (DNSSEC path) — at which
   *      point we walk that one DKIM leaf and submit it via the
   *      challenge submit-leaf method. We then keep polling
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
    setupEmailRecoveryFunnel,
    SetupEmailRecoveryEvents,
  } from "$lib/utils/analytics/setupEmailRecoveryFunnel";
  import { onDestroy, onMount } from "svelte";

  interface Props {
    /** Authenticated wrapper around `email_recovery_credential_prepare_add`. */
    prepare: (input: EmailChallengeDnsInput) => Promise<EmailChallenge>;
    /** Anonymous wrapper around the challenge-status query. */
    status: (nonce: string) => Promise<EmailChallengeStatus>;
    /** Anonymous wrapper around the challenge-diagnostics query. */
    diagnostics: (nonce: string) => Promise<[] | [EmailChallengeDiagnostics]>;
    /** Anonymous wrapper around the challenge submit-dkim-leaf method.
     *  Accept-only: rejects on a call-level error, else resolves void
     *  (poll for verdict). */
    submitDkimLeaf: (arg: EmailChallengeSubmitDkimLeafArg) => Promise<void>;
    /** Anonymous wrapper around the challenge resolve-via-doh method. */
    resolveViaDoh: (nonce: string) => Promise<void>;
    /** Pool of addresses the user has already verified on the share
     *  page. Surfaced as a non-blocking heads-up on the address-entry
     *  view when the typed address matches one of them. */
    verifiedAddresses?: string[];
    /** Called once on `RegistrationSucceeded`. The host is expected to
     *  show a success toast and close the dialog. */
    onSuccess: (address: string) => void;
  }

  const {
    prepare,
    status,
    diagnostics,
    submitDkimLeaf,
    resolveViaDoh,
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
    // Presentation-only stage entered when the user clicks "I've
    // sent the email" on the SendConfirmationEmail view. Same fields
    // as `sending`; the only difference is that the view re-renders
    // with `sent={true}`. The polling loop runs continuously across
    // both stages — see the guard in `runPoll`.
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
  //
  // Belt-and-braces `close()` on unmount: if the user abandons the
  // wizard mid-flow (Dialog `×`, navigation away, browser back)
  // without reaching a terminal branch, the funnel's
  // `trackWindowSession` listener would otherwise stay registered.
  // A later remount's `init()` would then overwrite the cleanup
  // pointer, leaking the old listener. `close()` is idempotent (it
  // only does work if a `start-…` timestamp is set), so calling it
  // here both fires `end-…` for the abandoned flow and unregisters
  // the visibility listener cleanly.
  onMount(() => {
    setupEmailRecoveryFunnel.init();
  });
  onDestroy(() => {
    setupEmailRecoveryFunnel.close();
  });

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

    const input: EmailChallengeDnsInput = {
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
      if (isCanisterError<EmailChallengeError>(e)) {
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
        if (e.type === "InvalidEmailAddress") {
          throw new Error($t`This doesn't look like a valid email address.`);
        }
      }
      // Anything else propagates to EnterAddress's inline error.
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
      funnel: setupEmailRecoveryFunnel,
      events: {
        needDkimLeaf: SetupEmailRecoveryEvents.NeedDkimLeaf,
        dkimLeafSubmitted: SetupEmailRecoveryEvents.DkimLeafSubmitted,
        failed: SetupEmailRecoveryEvents.Failed,
        unsupportedDomain: SetupEmailRecoveryEvents.UnsupportedDomain,
      },
      // Setup completes on `RegistrationSucceeded`: bind done, park
      // the wizard on the shared SuccessView. The host's
      // `onSuccess` fires once the user dismisses it via Done so
      // toast + close happen together.
      handleSuccess: (result) => {
        if (!("RegistrationSucceeded" in result)) return false;
        setupEmailRecoveryFunnel.trigger(SetupEmailRecoveryEvents.Succeeded);
        setupEmailRecoveryFunnel.close();
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

  // Triggered by SendConfirmationEmail's "I've sent the email"
  // button. Pure presentation transition — the polling loop already
  // covers the `waiting` stage via the loop's stage-kind guard, so
  // we don't restart it here.
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
    flow="recovery"
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
