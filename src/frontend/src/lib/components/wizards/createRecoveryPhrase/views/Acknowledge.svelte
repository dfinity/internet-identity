<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { Trans } from "$lib/components/locale";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Steps from "$lib/components/wizards/createRecoveryPhrase/components/Steps.svelte";

  interface Props {
    onAcknowledged: () => Promise<void>;
  }

  const { onAcknowledged }: Props = $props();

  let isAcknowledged = $state(false);
  let isGeneratingPhrase = $state(false);

  const handleAcknowledge = async () => {
    try {
      isGeneratingPhrase = true;
      await onAcknowledged();
    } finally {
      isGeneratingPhrase = false;
    }
  };
</script>

<Steps total={3} current={1} class="my-10" />
<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {$t`Before you continue`}
</h2>
<p class="text-text-tertiary mb-8 text-base font-medium">
  <Trans>
    You're about to see your recovery phrase, which Internet Identity doesn't
    store and thus can't be retrieved if lost, so keep it safe offline.
  </Trans>
</p>
<Checkbox
  bind:checked={isAcknowledged}
  label={$t`I Acknowledge`}
  hint={$t`I am responsible for my recovery phrase and it cannot be retrieved from Internet Identity if lost.`}
  size="sm"
  class="mb-8"
/>
<Button
  onclick={handleAcknowledge}
  disabled={!isAcknowledged || isGeneratingPhrase}
  size="lg"
>
  {#if isGeneratingPhrase}
    <ProgressRing />
    <span>{$t`Generating phrase...`}</span>
  {:else}
    <span>{$t`Continue`}</span>
  {/if}
</Button>
