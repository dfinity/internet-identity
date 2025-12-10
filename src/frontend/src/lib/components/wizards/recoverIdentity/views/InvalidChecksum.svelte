<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { MessageSquareXIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onRetry: () => void;
    onCancel: () => void;
  }

  const { onRetry, onCancel }: Props = $props();
</script>

<FeaturedIcon variant="error" size="lg" class="mb-4">
  <MessageSquareXIcon class="size-6" />
</FeaturedIcon>
<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {$t`Something is wrong!`}
</h2>
<p class="text-text-tertiary mb-8 text-base font-medium">
  <Trans>Incorrect word order. Review and try again.</Trans>
</p>
<button onclick={onRetry} class="btn btn-lg mb-3">
  {$t`Retry`}
</button>
<button onclick={onCancel} class="btn btn-tertiary btn-lg">
  {$t`Cancel`}
</button>
