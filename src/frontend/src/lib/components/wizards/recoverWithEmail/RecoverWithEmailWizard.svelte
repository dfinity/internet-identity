<script lang="ts">
  /**
   * Recovery-flow wizard. Anonymous from start to finish — by the
   * time a user reaches for this they may have lost every authn
   * method on their anchor, so the canister takes no caller
   * principal and resolves the anchor only later from the verified
   * `From:` of the inbound email.
   *
   * Three stages, mirroring `SetupEmailRecoveryWizard`:
   *
   *   1. User types the email they registered as a recovery method.
   *   2. We generate a fresh ECDSA session keypair locally, ask the
   *      canister for a fresh nonce + the `recover@id.ai` recipient
   *      via `email_recovery_prepare_delegation`, and show those
   *      to the user.
   *   3. User sends a DKIM-signed email containing that nonce; we
   *      poll `email_challenge_status` until it flips to
   *      `RecoveryReady { user_key, expiration }`, then call
   *      `email_recovery_get_delegation` for the
   *      `SignedDelegation`. The wizard hands the delegation to the
   *      caller via `onSignedIn` so the host page can complete the
   *      sign-in.
   */

  import EnterAddressForRecovery from "./views/EnterAddressForRecovery.svelte";
  import SendConfirmationEmail from "$lib/components/wizards/emailRecovery/shared/views/SendConfirmationEmail.svelte";
  import FailedView from "$lib/components/wizards/emailRecovery/shared/views/FailedView.svelte";
  import UnsupportedDomain from "$lib/components/wizards/emailRecovery/shared/views/UnsupportedDomain.svelte";
  import { buildDiagnosticsBlob } from "$lib/components/wizards/emailRecovery/shared/diagnostics";
  import { runEmailRecoveryPoll } from "$lib/components/wizards/emailRecovery/shared/poll";
  import type {
    EmailChallenge,
    EmailChallengeDiagnostics,
    EmailChallengeDnsInput,
    EmailChallengeError,
    EmailRecoveryGetDelegationArgs,
    EmailChallengeStatus,
    EmailChallengeSubmitDkimLeafArg,
    SignedDelegation,
    DnsProofBundle,
  } from "$lib/generated/internet_identity_types";
  import { assembleSkeleton, type Path } from "$lib/utils/dnssec";
  import { isCanisterError } from "$lib/utils/utils";
  import {
    recoverWithEmailFunnel,
    RecoverWithEmailEvents,
  } from "$lib/utils/analytics/recoverWithEmailFunnel";
  import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
  import { onDestroy, onMount } from "svelte";
  import type { RecoverySuccess } from "./index";

  interface Props {
    /** Anonymous wrapper around `email_recovery_prepare_delegation`. */
    prepareDelegation: (
      input: EmailChallengeDnsInput,
      sessionPublicKey: Uint8Array,
    ) => Promise<EmailChallenge>;
    /** Anonymous wrapper around `email_challenge_status` (query). */
    status: (nonce: string) => Promise<EmailChallengeStatus>;
    /** Anonymous wrapper around `email_challenge_diagnostics` (query). */
    diagnostics: (nonce: string) => Promise<[] | [EmailChallengeDiagnostics]>;
    /** Anonymous wrapper around `email_challenge_submit_dkim_leaf`. Accept-only:
     *  rejects on a call-level error, else resolves void (poll for verdict). */
    submitDkimLeaf: (arg: EmailChallengeSubmitDkimLeafArg) => Promise<void>;
    /** Anonymous wrapper around `email_challenge_resolve_via_doh`. */
    resolveViaDoh: (nonce: string) => Promise<void>;
    /** Anonymous wrapper around `email_recovery_get_delegation`. */
    getDelegation: (
      args: EmailRecoveryGetDelegationArgs,
    ) => Promise<SignedDelegation>;
    /** Called after a successful recovery; the host page builds the
        DelegationIdentity and proceeds with the rest of the sign-in. */
    onSignedIn: (success: RecoverySuccess) => Promise<void>;
  }

  const {
    prepareDelegation,
    status,
    diagnostics,
    submitDkimLeaf,
    resolveViaDoh,
    getDelegation,
    onSignedIn,
  }: Props = $props();

  type Stage =
    | { kind: "enter"; initialError?: string }
    | {
        kind: "sending";
        challenge: EmailChallenge;
        address: string;
        sessionIdentity: ECDSAKeyIdentity;
        path: Path;
      }
    // Presentation-only stage entered when the user clicks "I've
    // sent the email" on the SendConfirmationEmail view. Same
    // semantics as in SetupEmailRecoveryWizard — polling continues.
    | {
        kind: "waiting";
        challenge: EmailChallenge;
        address: string;
        sessionIdentity: ECDSAKeyIdentity;
        path: Path;
      }
    | { kind: "unsupported"; domain: string }
    | { kind: "failed"; reason: string; diagnostics?: string };

  let stage = $state<Stage>({ kind: "enter" });
  let polling = $state(false);
  onDestroy(() => {
    polling = false;
  });

  // Plausible funnel: emit `start-email-recovery-recover` when the
  // wizard mounts. Each milestone calls `trigger(...)`; the terminal
  // states (signed-in / failed / unsupported) call `close()` to record
  // duration. See `recoverWithEmailFunnel.ts` for the event shape.
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
    recoverWithEmailFunnel.init();
  });
  onDestroy(() => {
    recoverWithEmailFunnel.close();
  });

  const handleAddressSubmitted = async (address: string) => {
    recoverWithEmailFunnel.trigger(RecoverWithEmailEvents.AddressSubmitted);
    const domain = address.split("@")[1] ?? "";

    // Generate the session keypair the eventual delegation will be
    // bound to. Local-only — never leaves the browser. The
    // canister hashes its public key into the delegation seed so a
    // matching `email_recovery_get_delegation` lookup later will
    // produce a usable signature.
    const sessionIdentity = await ECDSAKeyIdentity.generate();
    const sessionPublicKey = new Uint8Array(
      sessionIdentity.getPublicKey().toDer(),
    );

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
      const challenge = await prepareDelegation(input, sessionPublicKey);
      recoverWithEmailFunnel.trigger(RecoverWithEmailEvents.Prepared, { path });
      stage = { kind: "sending", challenge, address, sessionIdentity, path };
      void runPoll(challenge.nonce, domain, sessionIdentity);
    } catch (e) {
      if (isCanisterError<EmailChallengeError>(e)) {
        if (
          e.type === "DomainNotAllowlisted" ||
          e.type === "DomainNotSupported"
        ) {
          recoverWithEmailFunnel.trigger(
            RecoverWithEmailEvents.UnsupportedDomain,
            { reason: e.type },
          );
          recoverWithEmailFunnel.close();
          stage = { kind: "unsupported", domain };
          return;
        }
      }
      throw e;
    }
  };

  const runPoll = async (
    nonce: string,
    domain: string,
    sessionIdentity: ECDSAKeyIdentity,
  ) => {
    if (polling) return;
    polling = true;
    await runEmailRecoveryPoll({
      nonce,
      domain,
      status,
      submitDkimLeaf,
      resolveViaDoh,
      diagnostics,
      funnel: recoverWithEmailFunnel,
      events: {
        needDkimLeaf: RecoverWithEmailEvents.NeedDkimLeaf,
        dkimLeafSubmitted: RecoverWithEmailEvents.DkimLeafSubmitted,
        failed: RecoverWithEmailEvents.Failed,
        unsupportedDomain: RecoverWithEmailEvents.UnsupportedDomain,
      },
      // Recovery completes on `RecoveryReady`: the canister stamped the
      // delegation seed, so fetch the SignedDelegation and hand it to
      // the host. `retrieveDelegation` owns its own failure handling.
      handleSuccess: async (result) => {
        if (!("RecoveryReady" in result)) return false;
        recoverWithEmailFunnel.trigger(RecoverWithEmailEvents.RecoveryReady);
        await retrieveDelegation(
          nonce,
          result.RecoveryReady.user_key,
          result.RecoveryReady.expiration,
          result.RecoveryReady.anchor_number,
          sessionIdentity,
        );
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

  const retrieveDelegation = async (
    nonce: string,
    userKey: Uint8Array | number[],
    expiration: bigint,
    identityNumber: bigint,
    sessionIdentity: ECDSAKeyIdentity,
  ) => {
    try {
      const sessionPublicKey = new Uint8Array(
        sessionIdentity.getPublicKey().toDer(),
      );
      const delegation = await getDelegation({
        nonce,
        session_key: sessionPublicKey,
        expiration,
      });
      recoverWithEmailFunnel.trigger(
        RecoverWithEmailEvents.DelegationRetrieved,
      );
      await onSignedIn({
        sessionIdentity,
        userKey,
        delegation,
        identityNumber,
      });
      recoverWithEmailFunnel.trigger(RecoverWithEmailEvents.SignedIn);
      recoverWithEmailFunnel.close();
    } catch (e) {
      recoverWithEmailFunnel.trigger(RecoverWithEmailEvents.Failed, {
        reason: e instanceof Error ? e.message : String(e),
      });
      recoverWithEmailFunnel.close();
      stage = {
        kind: "failed",
        reason: e instanceof Error ? e.message : String(e),
        // FE-side failure (the delegation fetch threw) — no canister
        // challenge state to read, so the blob carries the build id only.
        diagnostics: buildDiagnosticsBlob(undefined),
      };
    }
  };

  const handleRetry = () => {
    stage = { kind: "enter" };
  };

  // Pure presentation transition — see SetupEmailRecoveryWizard's
  // handleSent for the rationale; the polling loop already covers
  // the `waiting` stage.
  const handleSent = () => {
    if (stage.kind !== "sending") return;
    stage = {
      kind: "waiting",
      challenge: stage.challenge,
      address: stage.address,
      sessionIdentity: stage.sessionIdentity,
      path: stage.path,
    };
  };
</script>

{#if stage.kind === "enter"}
  <EnterAddressForRecovery
    onSubmit={handleAddressSubmitted}
    initialError={stage.initialError}
  />
{:else if stage.kind === "sending"}
  <SendConfirmationEmail
    nonce={stage.challenge.nonce}
    mailbox={`recover@${window.location.hostname}`}
    fromAddress={stage.address}
    path={stage.path}
    onSent={handleSent}
  />
{:else if stage.kind === "waiting"}
  <SendConfirmationEmail
    nonce={stage.challenge.nonce}
    mailbox={`recover@${window.location.hostname}`}
    fromAddress={stage.address}
    path={stage.path}
    sent
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
