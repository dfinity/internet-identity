<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import {
    authenticatedStore,
    authenticationStore,
  } from "$lib/stores/authentication.store";
  import { t } from "$lib/stores/locale.store";
  import { nanosToMillis } from "$lib/utils/time";
  import { nonNullish } from "@dfinity/utils";
  import { goto } from "$app/navigation";
  import type { IdentityInfo } from "$lib/generated/internet_identity_types";
  import SuccessfulRecovery from "../components/SuccessfulRecovery.svelte";
  import RecoveryError from "../components/RecoveryError.svelte";
  import IdentityNotFound from "../components/IdentityNotFound.svelte";
  import CancelRecovery from "../components/CancelRecovery.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { throwCanisterError } from "$lib/utils/utils";
  import RecoveryPhraseInput from "$lib/components/views/RecoveryPhraseInput.svelte";
  import { Trans } from "$lib/components/locale";
  import {
    fromMnemonicWithoutValidation,
    IC_DERIVATION_PATH,
    isValidMnemonic,
  } from "$lib/utils/recoveryPhrase";
  import { anonymousActor, anonymousAgent } from "$lib/globals";
  import { handleError } from "$lib/components/utils/error";
  import { HttpAgent } from "@icp-sdk/core/agent";
  import { authenticateWithSession } from "$lib/utils/authentication";

  const EMPTY_PHRASE = Array.from({ length: 24 }).map(() => "");

  interface IdentityData {
    identityNumber: bigint;
    info: IdentityInfo;
  }

  let value = $state(EMPTY_PHRASE);
  let showValues = $state(false);
  let recoveredIdentityData = $state<IdentityData>();
  // let continueInProgress = $state(false);
  let invalidPhraseError = $state(false); // Recovery phrase fails checksum validation
  let identityNotFoundError = $state(false);
  let autoSubmit = $state(true);
  let showCancelDialog = $state(false);
  let isCheckingPhrase = $state(false);

  const phraseValid = $derived(value.every((word) => word.length > 0));

  const handleSubmit = async () => {
    const recoveryPhrase = value.join(" ");
    if (!isValidMnemonic(recoveryPhrase)) {
      invalidPhraseError = true;
      return;
    }
    const identity = await fromMnemonicWithoutValidation(
      recoveryPhrase,
      IC_DERIVATION_PATH,
    );
    const agent = await HttpAgent.from(anonymousAgent);
    agent.replaceIdentity(identity);
    try {
      const [identityNumber] =
        await anonymousActor.lookup_caller_identity_by_recovery_phrase.withOptions(
          { agent },
        )();
      if (identityNumber === undefined) {
        identityNotFoundError = true;
        return;
      }
      recoveredIdentityData = {
        identityNumber,
        info: await anonymousActor.identity_info
          .withOptions({ agent })(identityNumber)
          .then(throwCanisterError),
      };
    } catch (error) {
      handleError(error);
    }
  };

  const handleTryAnother = () => {
    recoveredIdentityData = undefined;
  };

  const handleRetry = () => {
    invalidPhraseError = false;
    identityNotFoundError = false;
    manualSubmitRequired = true;
    showValues = true;
    clearTimeout(submitTimeoutId);
    submitTimeoutId = undefined;
  };

  const handleContinue = async (newName?: string) => {
    if (recoveredIdentityData === undefined) {
      return;
    }
    try {
      // Authenticate with recovery phrase
      const identity = await fromMnemonicWithoutValidation(
        value.join(" "),
        IC_DERIVATION_PATH,
      );
      await authenticationStore.set({
        identity: await authenticateWithSession({ session: { identity } }),
        identityNumber: recoveredIdentityData.identityNumber,
        authMethod: {
          recoveryPhrase: true,
        },
      });
      // Set name if identity didn't have a name yet
      if (newName !== undefined) {
        await $authenticatedStore.actor
          .identity_properties_replace($authenticatedStore.identityNumber, {
            name: [newName],
          })
          .then(throwCanisterError);
      }
      toaster.success({
        title: $t`Successfully restored access to your identity`,
        description: $t`Make sure to add a new access method so that you can sign in next time or reset your recovery phrase.`,
        duration: 5000,
      });
      await goto("/manage/access");
    } catch (error) {
      recoveredIdentityData = undefined;
      autoSubmit = false;
      authenticationStore.reset();
      handleError(error);
    }
  };

  const handleLeaveRecovery = () => goto("/login");
</script>

<div class="mt-auto flex flex-col sm:my-auto">
  <h1 class="text-text-primary mb-3 text-2xl font-medium">
    {$t`Enter your recovery phrase`}
  </h1>
  <p class="text-text-tertiary mb-6 text-sm">
    {#if isCheckingPhrase}
      <Trans>This may take a few seconds</Trans>
    {:else}
      <Trans>Type each word in the correct order:</Trans>
    {/if}
  </p>
  <RecoveryPhraseInput
    bind:value
    {showValues}
    disabled={isCheckingPhrase}
    class="mb-5"
  />
  <div class="limited-height mb-5 flex flex-row">
    <Button
      onclick={() => (showValues = !showValues)}
      variant="tertiary"
      disabled={isCheckingPhrase}
      class="flex-1"
    >
      {showValues ? $t`Hide all` : $t`Show all`}
    </Button>
    <Button
      onclick={() => (value = EMPTY_PHRASE)}
      variant="tertiary"
      disabled={isCheckingPhrase}
      class="flex-1"
    >
      {$t`Clear all`}
    </Button>
  </div>
  {#if !autoSubmit}
    <Button
      onclick={handleSubmit}
      size="xl"
      disabled={!phraseValid || isCheckingPhrase}
      class="mb-3"
    >
      {$t`Submit`}
    </Button>
  {/if}
  <Button
    onclick={() => (showCancelDialog = true)}
    variant="secondary"
    size="xl"
    disabled={isCheckingPhrase}
  >
    {$t`Cancel`}
  </Button>
</div>

{#if recoveredIdentityData}
  {@const identityName = recoveredIdentityData.info.name[0]}
  {@const createdAt = nonNullish(recoveredIdentityData.info.created_at[0])
    ? new Date(nanosToMillis(recoveredIdentityData.info.created_at[0]))
    : undefined}
  <Dialog onClose={handleTryAnother}>
    <SuccessfulRecovery
      identityNumber={recoveredIdentityData.identityNumber}
      {identityName}
      {createdAt}
      onContinue={handleContinue}
      onCancel={handleLeaveRecovery}
    />
  </Dialog>
{/if}

{#if invalidPhraseError}
  <Dialog>
    <RecoveryError onRetry={handleRetry} onCancel={handleLeaveRecovery} />
  </Dialog>
{/if}

{#if identityNotFoundError}
  <Dialog>
    <IdentityNotFound onRetry={handleRetry} onCancel={handleLeaveRecovery} />
  </Dialog>
{/if}

{#if showCancelDialog}
  <Dialog onClose={() => (showCancelDialog = false)}>
    <CancelRecovery
      onCancel={handleLeaveRecovery}
      onClose={() => (showCancelDialog = false)}
    />
  </Dialog>
{/if}

<style>
  @media (max-height: 700px) {
    .limited-height {
      display: none !important;
    }
  }
</style>
