<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { UserXIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import Trans from "$lib/components/locale/Trans.svelte";

  type Props = {
    onRetry: () => void;
    onCancel: () => void;
  };

  const { onRetry, onCancel }: Props = $props();
</script>

<FeaturedIcon variant="error" size="lg" class="mb-4">
  <UserXIcon class="size-6" />
</FeaturedIcon>
<h2 class="text-text-primary mb-3 text-2xl font-medium">
  {$t`No identity found`}
</h2>
<p class="text-text-tertiary mb-8 text-base font-medium">
  <Trans>
    Your recovery phrase is valid but not for internet identity. You may have
    used the wrong seed phrase for the wrong product.
  </Trans>
</p>
<Button onclick={onRetry} size="lg" class="mb-1.5">
  {$t`Retry`}
</Button>
<Button onclick={onCancel} variant="tertiary" size="lg">
  {$t`Cancel`}
</Button>
