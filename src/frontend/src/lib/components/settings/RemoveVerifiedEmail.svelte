<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { Trash2Icon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    address: string;
    onRemove: () => Promise<void>;
    onCancel: () => void;
  }

  const { address, onRemove, onCancel }: Props = $props();

  let isRemoving = $state(false);

  const handleRemove = async () => {
    try {
      isRemoving = true;
      await onRemove();
    } finally {
      isRemoving = false;
    }
  };
</script>

<div class="flex flex-1 flex-col gap-2">
  <FeaturedIcon variant="info" size="lg" class="mb-3">
    <Trash2Icon class="size-5" />
  </FeaturedIcon>
  <h1 class="text-text-primary text-xl font-medium">
    {$t`Forget this email?`}
  </h1>
  <p class="text-text-secondary mb-6 text-sm">
    <Trans>
      <strong class="text-text-primary break-all">{address}</strong> will no longer
      be associated with your Internet Identity.
    </Trans>
  </p>
  <div class="mt-auto flex flex-row items-stretch gap-3">
    <button
      class="btn btn-tertiary btn-lg flex-1"
      onclick={onCancel}
      disabled={isRemoving}
    >
      {$t`Cancel`}
    </button>
    <button
      class="btn btn-primary btn-lg btn-danger flex-1"
      onclick={handleRemove}
      disabled={isRemoving}
    >
      {#if isRemoving}
        <ProgressRing />
        <span>{$t`Removing…`}</span>
      {:else}
        <span>{$t`Remove`}</span>
      {/if}
    </button>
  </div>
</div>
