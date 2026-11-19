<script lang="ts">
  import { onMount } from "svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";

  interface Props {
    /** Leaves the page behind Apple's sheet, back to where the user came from. */
    onReturnToApp: () => void;
  }

  const { onReturnToApp }: Props = $props();

  /**
   * How long the page waits before offering a way back: iOS tells it nothing
   * when the sheet is canceled.
   */
  const RETURN_AFTER_MILLIS = 10_000;

  let canReturnToApp = $state(false);

  onMount(() => {
    const timer = setTimeout(
      () => (canReturnToApp = true),
      RETURN_AFTER_MILLIS,
    );
    return () => clearTimeout(timer);
  });
</script>

<div role="status" class="flex flex-col items-center gap-3 px-8 text-center">
  <ProgressRing class="size-8" />
  <p class="text-text-primary text-base font-medium">
    {$t`Connecting to Apple`}
  </p>
  {#if canReturnToApp}
    <button class="btn btn-secondary mt-3" onclick={onReturnToApp}>
      {$t`Return to app`}
    </button>
  {/if}
</div>
