<script lang="ts">
  import { isValidMnemonic } from "$lib/utils/recoveryPhrase";
  import { waitFor } from "$lib/utils/utils";
  import EnterRecoveryPhrase from "./views/EnterRecoveryPhrase.svelte";
  import IdentityNotFound from "./views/IdentityNotFound.svelte";
  import InvalidChecksum from "./views/InvalidChecksum.svelte";
  import IdentityFound from "./views/IdentityFound.svelte";
  import MissingName from "./views/MissingName.svelte";
  import type { FoundIdentity } from "./index";

  interface Props {
    onSubmit: (recoveryPhrase: string[]) => Promise<FoundIdentity | undefined>;
    onSignIn: (
      identityNumber: bigint,
      recoveryPhrase: string[],
      newName?: string,
    ) => Promise<void>;
    onCancel: () => void;
  }

  const { onSubmit, onSignIn, onCancel }: Props = $props();

  let foundIdentityWithPhrase = $state<
    FoundIdentity & { recoveryPhrase: string[] }
  >();
  let identityNotFound = $state(false);
  let invalidChecksum = $state(false);
  let incorrectPhrase = $state<string[]>();

  const handleSubmit = async (recoveryPhrase: string[]) => {
    if (!isValidMnemonic(recoveryPhrase.join(" "))) {
      await waitFor(2000); // Artificial delay to improve UX
      incorrectPhrase = recoveryPhrase;
      invalidChecksum = true;
      return;
    }
    const foundIdentity = await onSubmit(recoveryPhrase);
    if (foundIdentity === undefined) {
      incorrectPhrase = undefined; // Cleared when it's valid but not for II
      identityNotFound = true;
      return;
    }
    foundIdentityWithPhrase = { ...foundIdentity, recoveryPhrase };
  };
  const handleContinue = async (newName?: string) => {
    if (foundIdentityWithPhrase === undefined) {
      return;
    }
    await onSignIn(
      foundIdentityWithPhrase.identityNumber,
      foundIdentityWithPhrase.recoveryPhrase,
      newName,
    );
  };
  const handleRetry = () => {
    foundIdentityWithPhrase = undefined;
    invalidChecksum = false;
    identityNotFound = false;
  };
</script>

{#if invalidChecksum}
  <InvalidChecksum onRetry={handleRetry} {onCancel} />
{:else if identityNotFound}
  <IdentityNotFound onRetry={handleRetry} {onCancel} />
{:else if foundIdentityWithPhrase !== undefined}
  {#if foundIdentityWithPhrase.identityInfo.name[0] === undefined}
    <MissingName
      identityNumber={foundIdentityWithPhrase.identityNumber}
      onContinue={handleContinue}
      {onCancel}
    />
  {:else}
    <IdentityFound
      identityInfo={foundIdentityWithPhrase.identityInfo}
      onContinue={handleContinue}
      {onCancel}
    />
  {/if}
{:else}
  <EnterRecoveryPhrase
    recoveryPhrase={incorrectPhrase}
    onSubmit={handleSubmit}
  />
{/if}
