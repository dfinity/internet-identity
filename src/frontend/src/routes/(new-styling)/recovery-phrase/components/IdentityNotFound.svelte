<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { UserXIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";

  type Props = {
    onRetry: () => void;
    onCancel: () => void;
  };

  const { onRetry, onCancel }: Props = $props();
</script>

<div class="flex flex-col gap-8">
  <div class="mt-6 flex flex-col gap-4">
    <FeaturedIcon size="md" variant="error">
      <UserXIcon class="size-6" />
    </FeaturedIcon>
    <div class="flex flex-col gap-3">
      <h2 class="text-text-primary text-xl">
        {$t`No identity found`}
      </h2>
      <p class="text-text-tertiary text-sm">
        {$t`Your recovery phrase is valid but not for internet identity. You may have used the wrong seed phrase for the wrong product.`}
      </p>
    </div>
  </div>
  <div class="align-stretch flex flex-col gap-2">
    <Button size="lg" onclick={onRetry}>
      {$t`Retry`}
    </Button>
    <Button size="lg" variant="tertiary" onclick={onCancel}>
      {$t`Cancel`}
    </Button>
  </div>
</div>
