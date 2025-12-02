<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Button from "$lib/components/ui/Button.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { MessageSquareXIcon, TriangleAlertIcon } from "@lucide/svelte";

  interface Props {
    onRetry: () => void;
    onCancel: () => void;
    inputMethod: "selecting" | "typing";
  }

  const { onRetry, onCancel, inputMethod }: Props = $props();
</script>

<FeaturedIcon variant="error" size="lg" class="mb-4">
  <MessageSquareXIcon class="size-6" />
</FeaturedIcon>
<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {$t`Something is wrong!`}
</h2>
<p class="text-text-tertiary mb-8 text-base font-medium">
  {#if inputMethod === "selecting"}
    <Trans>Incorrect word order. Review and try again.</Trans>
  {:else}
    <Trans>Incorrect recovery phrase. Please try again.</Trans>
  {/if}
</p>
<Button onclick={onRetry} size="lg" class="mb-1.5">
  {$t`Retry`}
</Button>
<Button onclick={onCancel} variant="tertiary" size="lg">
  {$t`Cancel`}
</Button>
