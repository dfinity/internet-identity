<script lang="ts">
  import type { Snippet } from "svelte";

  const { children }: { children: Snippet } = $props();
</script>

<ol class="flex flex-col gap-3">
  {@render children()}
</ol>
