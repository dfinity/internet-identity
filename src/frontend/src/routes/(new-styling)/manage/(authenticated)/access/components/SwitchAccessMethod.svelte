<script lang="ts">
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { ArrowLeftRightIcon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    onSwitch: () => Promise<void>;
    onCancel: () => void;
  }

  const { onSwitch, onCancel }: Props = $props();

  let isSwitching = $state(false);

  const handleSwitch = async () => {
    try {
      isSwitching = true;
      await onSwitch();
    } finally {
      isSwitching = false;
    }
  };
</script>

<div class="flex flex-1 flex-col">
  <div class="mb-8 flex flex-col">
    <FeaturedIcon variant="info" size="lg" class="mb-3">
      <ArrowLeftRightIcon class="size-6" />
    </FeaturedIcon>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Switch access method`}
    </h1>
    <div
      class={[
        "flex flex-col gap-4",
        "[&_p]:text-text-tertiary [&_p]:text-base [&_p]:font-medium",
      ]}
    >
      <p>
        <Trans>
          You are about to re-authenticate yourself with another access method
          on this device.
        </Trans>
      </p>
      <p>
        <Trans>
          This will become the default method used to authenticate on this
          device moving forward.
        </Trans>
      </p>
    </div>
  </div>
  <div class="mt-auto flex flex-col items-stretch gap-3">
    <button
      class="btn btn-primary btn-lg"
      onclick={handleSwitch}
      disabled={isSwitching}
    >
      {#if isSwitching}
        <ProgressRing />
        <span>{$t`Switching...`}</span>
      {:else}
        <span>{$t`Continue`}</span>
      {/if}
    </button>
    <button
      class="btn btn-tertiary btn-lg"
      onclick={onCancel}
      disabled={isSwitching}
    >
      {$t`Cancel`}
    </button>
  </div>
</div>
