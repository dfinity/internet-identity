<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { TriangleAlertIcon } from "@lucide/svelte";
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

<div class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <FeaturedIcon variant="warning" size="lg" class="mb-3">
      <TriangleAlertIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Remove verified email?`}
    </h1>
    <div
      class={[
        "flex flex-col gap-4",
        "[&_p]:text-text-tertiary [&_p]:text-base [&_p]:font-medium",
      ]}
    >
      <p>
        <Trans>
          Apps will no longer be able to request
          <strong class="text-text-primary break-all">{address}</strong>
          from this identity.
        </Trans>
      </p>
      <p>
        <Trans>
          You can verify this address again at any time. Your sign-in methods
          are unaffected.
        </Trans>
      </p>
    </div>
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <button
      class="btn btn-primary btn-lg btn-danger"
      onclick={handleRemove}
      disabled={isRemoving}
    >
      {#if isRemoving}
        <ProgressRing />
        <span>{$t`Removing email...`}</span>
      {:else}
        <span>{$t`Remove email`}</span>
      {/if}
    </button>
    <button
      class="btn btn-tertiary btn-lg"
      onclick={onCancel}
      disabled={isRemoving}
    >
      {$t`Cancel`}
    </button>
  </div>
</div>
