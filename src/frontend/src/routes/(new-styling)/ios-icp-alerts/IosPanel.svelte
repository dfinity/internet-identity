<script lang="ts">
  // Hidden from assistive technology: the instruction above carries the step, and a mock read aloud control by control says nothing a user can act on.
  import type { Snippet } from "svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";

  const { children }: { children: Snippet } = $props();
</script>

<Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
  {@render children()}
</Surface>
