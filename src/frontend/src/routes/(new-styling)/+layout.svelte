<script lang="ts">
  // Enable new styles only in the new layout pages.
  import "$lib/app.css";
  import { Toaster } from "@skeletonlabs/skeleton-svelte";
  import { toaster } from "$lib/utils/toaster";

  const { children, data } = $props();
</script>

{#key data.url}
  {@render children()}
  <Toaster {toaster}></Toaster>
{/key}
