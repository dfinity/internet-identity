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
   *      poll `email_recovery_status` until it flips to
   *      `RecoveryReady { user_key, expiration }`, then call
   *      `email_recovery_get_delegation` for the
   *      `SignedDelegation`. The wizard hands the delegation to the
   *      caller via `onSignedIn` so the host page can complete the
   *      sign-in.
   */

  import EnterAddressForRecovery from "./views/EnterAddressForRecovery.svelte";
  import SendConfirmationEmail from "$lib/components/wizards/setupEmailRecovery/views/SendConfirmationEmail.svelte";
  import FailedView from "$lib/components/wizards/setupEmailRecovery/views/FailedView.svelte";
  import UnsupportedDomain from "$lib/components/wizards/setupEmailRecovery/views/UnsupportedDomain.svelte";
  import type {
    EmailRecoveryChallenge,
    EmailRecoveryDnsInput,
    EmailRecoveryError,
    EmailRecoveryGetDelegationArgs,
    EmailRecoveryStatus,
    EmailRecoverySubmitDkimLeafArg,
    SignedDelegation,
    DnsProofBundle,
  } from "$lib/generated/internet_identity_types";
  import { assembleSkeleton, assembleDkimResolution } from "$lib/utils/dnssec";
  import { isCanisterError } from "$lib/utils/utils";
  import { ECDSAKeyIdentity } from "@icp-sdk/core/identity";
  import { onDestroy } from "svelte";
  import type { RecoverySuccess } from "./index";

  interface Props {
    /** Anonymous wrapper around `email_recovery_prepare_delegation`. */
    prepareDelegation: (
      input: EmailRecoveryDnsInput,
      sessionPublicKey: Uint8Array,
    ) => Promise<EmailRecoveryChallenge>;
    /** Anonymous wrapper around `email_recovery_status` (query). */
    status: (nonce: string) => Promise<EmailRecoveryStatus>;
    /** Anonymous wrapper around `email_recovery_submit_dkim_leaf`. */
    submitDkimLeaf: (
      arg: EmailRecoverySubmitDkimLeafArg,
    ) => Promise<EmailRecoveryStatus>;
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
    submitDkimLeaf,
    getDelegation,
    onSignedIn,
  }: Props = $props();

  type Path = "dnssec" | "doh";

  type Stage =
    | { kind: "enter"; initialError?: string }
    | {
        kind: "sending";
        challenge: EmailRecoveryChallenge;
        address: string;
        sessionIdentity: ECDSAKeyIdentity;
        path: Path;
      }
    | { kind: "unsupported"; domain: string }
    | { kind: "failed"; reason: string };

  let stage = $state<Stage>({ kind: "enter" });
  let polling = $state(false);
  onDestroy(() => {
    polling = false;
  });

  const friendlyError = (variant: EmailRecoveryStatus): string => {
    if ("Failed" in variant) {
      const reason = variant.Failed;
      if ("AddressNotRegistered" in reason) {
        return "We don't recognise this email. If you haven't registered it as a recovery method yet, sign in with another method first and add it.";
      }
      if ("DomainNotAllowlisted" in reason) {
        return `Internet Identity can't verify mail from ${reason.DomainNotAllowlisted} yet.`;
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

    const input: EmailRecoveryDnsInput = {
      address,
      dns_proof: dnsProof === undefined ? [] : [dnsProof],
    };

    try {
      const challenge = await prepareDelegation(input, sessionPublicKey);
      stage = { kind: "sending", challenge, address, sessionIdentity, path };
      void runPoll(challenge.nonce, domain, sessionIdentity);
    } catch (e) {
      if (isCanisterError<EmailRecoveryError>(e)) {
        if (
          e.type === "DomainNotAllowlisted" ||
          e.type === "DomainNotSupported"
        ) {
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
    let intervalMs = 1_000;
    let dkimLeafSubmitted = false;
    try {
      while (polling && stage.kind === "sending") {
        const result = await status(nonce);
        if ("RecoveryReady" in result) {
          await retrieveDelegation(
            nonce,
            result.RecoveryReady.user_key,
            result.RecoveryReady.expiration,
            result.RecoveryReady.anchor_number,
            sessionIdentity,
          );
          return;
        }
        if ("Failed" in result || "Expired" in result) {
          stage = { kind: "failed", reason: friendlyError(result) };
          return;
        }
        if ("NeedDkimLeaf" in result && !dkimLeafSubmitted) {
          dkimLeafSubmitted = true;
          const selector = result.NeedDkimLeaf.selector;
          try {
            const walked = await assembleDkimResolution(domain, selector);
            if (walked !== undefined) {
              const submission = await submitDkimLeaf({
                nonce,
                hops: walked.hops,
                extra_chains: walked.extraChains,
              });
              if ("RecoveryReady" in submission) {
                await retrieveDelegation(
                  nonce,
                  submission.RecoveryReady.user_key,
                  submission.RecoveryReady.expiration,
                  submission.RecoveryReady.anchor_number,
                  sessionIdentity,
                );
                return;
              }
              if ("Failed" in submission || "Expired" in submission) {
                stage = { kind: "failed", reason: friendlyError(submission) };
                return;
              }
            }
          } catch {
            // Leave the loop running so the user sees a clean
            // Expired if the canister times out.
          }
        }
        await new Promise((r) => setTimeout(r, intervalMs));
        intervalMs = Math.min(5_000, intervalMs * 1.5);
      }
    } finally {
      polling = false;
    }
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
      await onSignedIn({
        sessionIdentity,
        userKey,
        delegation,
        identityNumber,
      });
    } catch (e) {
      stage = {
        kind: "failed",
        reason: e instanceof Error ? e.message : String(e),
      };
    }
  };

  const handleRetry = () => {
    stage = { kind: "enter" };
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
  />
{:else if stage.kind === "unsupported"}
  <UnsupportedDomain domain={stage.domain} onRetry={handleRetry} />
{:else}
  <FailedView reason={stage.reason} onRetry={handleRetry} />
{/if}
