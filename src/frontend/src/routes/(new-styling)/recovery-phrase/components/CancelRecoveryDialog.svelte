<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { AlertTriangleIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import Trans from "$lib/components/locale/Trans.svelte";

  type Props = {
    onConfirm: () => void;
    onClose: () => void;
  };

  const { onConfirm, onClose }: Props = $props();
</script>

<div class="flex flex-col gap-8">
  <div class="mt-6 flex flex-col gap-4">
    <FeaturedIcon size="md" variant="warning">
      <AlertTriangleIcon class="size-6" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h2 class="text-text-primary text-xl">
        {$t`Cancel recovery?`}
      </h2>
      <p class="text-text-tertiary text-sm">
        <Trans>Are you sure you want to cancel the recovery process?</Trans>
      </p>
    </div>
  </div>
  <div class="align-stretch flex flex-col gap-2">
    <Button size="lg" variant="primary" danger onclick={onConfirm}>
      {$t`Cancel`}
    </Button>
    <Button size="lg" variant="tertiary" onclick={onClose}>
      {$t`Close`}
    </Button>
  </div>
</div>
