<script lang="ts">
  // Enable new styles only in the new layout pages.
  import "$lib/app.css";
  import Background from "$lib/components/UI/Background.svelte";
  import { Toaster } from "@skeletonlabs/skeleton-svelte";
  import { toaster } from "$lib/utils/toaster";

  const { children, data } = $props();
</script>

{#key data.url}
  <Background
    class="text-ii-text-primary-dark dark:text-ii-text-primary-light bg-ii-background-primary-light dark:bg-ii-background-primary-dark"
  >
    {@render children()}
  </Background>
  <Toaster {toaster}></Toaster>
{/key}
