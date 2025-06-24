<script lang="ts">
  import { fade } from "svelte/transition";

  const { class: classes }: { class?: string } = $props();
</script>

<div
  class={`min-h-1 min-w-1 ${classes} bg-fg-brand-primary animate-pulse rounded-lg`}
  out:fade={{ duration: 30 }}
></div>
