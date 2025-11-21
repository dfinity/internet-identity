<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import Button from "$lib/components/ui/Button.svelte";
  import { Trans } from "$lib/components/locale";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    onReset: () => Promise<void>;
    onCancel: () => void;
  }

  const { onReset, onCancel }: Props = $props();

  let isGeneratingPhrase = $state(false);

  const handleReset = async () => {
    try {
      isGeneratingPhrase = true;
      await onReset();
    } finally {
      isGeneratingPhrase = false;
    }
  };
</script>

<FeaturedIcon variant="warning" size="lg" class="mb-4">
  <TriangleAlertIcon class="size-6" />
</FeaturedIcon>
<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {$t`Reset your recovery phrase?`}
</h2>
<p class="text-text-tertiary mb-3 text-base font-medium">
  <Trans>
    Resetting will invalidate your current recovery phrase, leaving only the new
    one usable.
  </Trans>
</p>
<p class="text-text-tertiary mb-8 text-base font-medium">
  <Trans>Do you want to continue?</Trans>
</p>
<Button
  onclick={handleReset}
  size="lg"
  danger
  class="mb-3"
  disabled={isGeneratingPhrase}
>
  {#if isGeneratingPhrase}
    <ProgressRing />
    <span>{$t`Resetting recovery phrase...`}</span>
  {:else}
    <span>{$t`Reset`}</span>
  {/if}
</Button>
<Button
  onclick={onCancel}
  variant="tertiary"
  size="lg"
  disabled={isGeneratingPhrase}
>
  {$t`Cancel`}
</Button>
