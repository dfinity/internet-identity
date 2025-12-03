<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Steps from "$lib/components/wizards/createRecoveryPhrase/components/Steps.svelte";
  import StepsProgressBar from "$lib/components/wizards/createRecoveryPhrase/components/StepsProgressBar.svelte";
  import RecoveryPhraseInput from "$lib/components/views/RecoveryPhraseInput.svelte";
  import Button from "$lib/components/ui/Button.svelte";

  const EMPTY_PHRASE = Array.from({ length: 24 }).map(() => "");

  interface Props {
    onCompleted: (recoveryPhrase: string[]) => Promise<void>;
    recoveryPhrase?: string[];
  }

  const { onCompleted, recoveryPhrase }: Props = $props();

  let value = $state<string[]>(recoveryPhrase ?? EMPTY_PHRASE);
  let showValues = $state(recoveryPhrase !== undefined);
  let isCheckingPhrase = $state(false);

  const phraseValid = $derived(value.every((word) => word.length > 0));
  const autoSubmit = $derived(recoveryPhrase === undefined);

  const handleSubmit = async () => {
    try {
      isCheckingPhrase = true;
      await onCompleted(value);
    } finally {
      isCheckingPhrase = false;
    }
  };

  // Auto-submit after the last word has been entered
  $effect(() => {
    if (!phraseValid || !autoSubmit) {
      return;
    }
    handleSubmit();
  });
</script>

<div class="limited-height my-10">
  {#if isCheckingPhrase}
    <StepsProgressBar total={3} />
  {:else}
    <Steps total={3} current={3} />
  {/if}
</div>
<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {#if isCheckingPhrase}
    {$t`Checking recovery phrase`}
  {:else}
    {$t`Verify your recovery phrase`}
  {/if}
</h2>
<p class="text-text-tertiary mb-8 text-base font-medium">
  {#if isCheckingPhrase}
    <Trans>This may take a few seconds</Trans>
  {:else}
    <Trans>Type each word in the correct order:</Trans>
  {/if}
</p>
<RecoveryPhraseInput bind:value {showValues} disabled={isCheckingPhrase} />
<div class="more-limited-height mt-5 flex flex-row">
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
    variant="primary"
    size="xl"
    disabled={isCheckingPhrase || !phraseValid}
    class="mt-5"
  >
    {$t`Submit`}
  </Button>
{/if}

<style>
  @media (max-height: 700px) {
    .limited-height {
      display: none !important;
    }
  }
  @media (max-height: 570px) {
    .more-limited-height {
      display: none !important;
    }
  }
</style>
