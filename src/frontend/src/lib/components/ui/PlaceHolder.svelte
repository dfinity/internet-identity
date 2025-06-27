<script lang="ts">
  import { fade } from "svelte/transition";

  const {
    class: classes,
    hiddenIndex = 2,
  }: { class?: string; hiddenIndex?: number } = $props();
</script>

<div
  class={`min-h-1 min-w-1 ${classes} bg-fg-brand-primary animate-pulse rounded-lg nth-[${hiddenIndex}]:hidden`}
  out:fade={{ duration: 30, delay: 30 }}
></div>
