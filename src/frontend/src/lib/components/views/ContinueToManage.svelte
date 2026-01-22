<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import { ExternalLinkIcon } from "@lucide/svelte";

  interface Props {
    onContinue: () => void;
  }

  const { onContinue }: Props = $props();
</script>

<h2 class="text-text-primary mb-3 text-2xl">
  {$t`Almost there`}
</h2>
<p class="text-text-tertiary mb-6 text-base">
  <Trans>
    You have been successfully authenticated and can now manage your identity in
    a new window.
  </Trans>
</p>
<button onclick={onContinue} class="btn btn-xl">
  <span>{$t`Manage identity`}</span>
  <ExternalLinkIcon class="size-5" />
</button>
