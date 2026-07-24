<script lang="ts">
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
    prepare: (input: EmailChallengeDnsInput) => Promise<EmailChallenge>;
    status: (nonce: string) => Promise<EmailChallengeStatus>;
    diagnostics: (nonce: string) => Promise<[] | [EmailChallengeDiagnostics]>;
    submitDkimLeaf: (arg: EmailChallengeSubmitDkimLeafArg) => Promise<void>;
    resolveViaDoh: (nonce: string) => Promise<void>;
    initialAddress?: string;
    addressLocked?: boolean;
    recoveryAddresses?: string[];
    verifiedAddresses?: string[];
    openidAddresses?: string[];
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
    openidAddresses = [],
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
        if (e.type === "LimitReached") {
          const { limit } = e.value("LimitReached");
          throw new Error(
            $t`You've reached the limit of ${limit} verified emails. Remove one to add another.`,
            { cause: e },
          );
        }
        if (e.type === "InvalidEmailAddress") {
          throw new Error($t`This doesn't look like a valid email address.`, {
            cause: e,
          });
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
    {openidAddresses}
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
