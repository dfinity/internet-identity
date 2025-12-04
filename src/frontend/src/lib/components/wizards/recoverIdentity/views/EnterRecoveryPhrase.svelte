<script lang="ts">
  import RecoveryPhraseInput from "$lib/components/views/RecoveryPhraseInput.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Button from "$lib/components/ui/Button.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  const EMPTY_PHRASE = Array.from({ length: 24 }).map(() => "");

  interface Props {
    recoveryPhrase?: string[];
    onSubmit: (recoveryPhrase: string[]) => Promise<void>;
  }

  const { recoveryPhrase, onSubmit }: Props = $props();

  let value = $state(recoveryPhrase ?? EMPTY_PHRASE);
  let showValues = $state(false);
  let isCheckingPhrase = $state(false);

  const phraseValid = $derived(value.every((word) => word.length > 0));
  const autoSubmit = $derived(recoveryPhrase === undefined);

  const handleSubmit = async () => {
    try {
      isCheckingPhrase = true;
      await onSubmit(value);
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

<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {$t`Enter your recovery phrase`}
</h2>
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
    <ProgressRing />
    <span>{$t`Submit`}</span>
  </Button>
{/if}

<style>
  @media (max-height: 700px) {
    .limited-height {
      display: none !important;
    }
  }
</style>
