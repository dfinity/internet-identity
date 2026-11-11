<script lang="ts">
  import { onMount } from "svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { t } from "$lib/stores/locale.store";

  const SLOW_AFTER_MS = 10_000;

  let isSlow = $state(false);

  onMount(() => {
    const timer = setTimeout(() => (isSlow = true), SLOW_AFTER_MS);
    return () => clearTimeout(timer);
  });
</script>

<div role="status" class="flex flex-col items-center gap-3 px-8 text-center">
  <ProgressRing class="size-8" />
  <p class="text-text-primary text-base font-medium">
    {$t`Connecting to your organization`}
  </p>
  {#if isSlow}
    <p class="text-text-tertiary text-sm text-pretty">
      {$t`This is taking longer than usual.`}
    </p>
  {/if}
</div>
