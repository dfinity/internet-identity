<script lang="ts">
  // Enable new styles only in the new layout pages.
  import "$lib/app.css";
  import Background from "$lib/components/UI/Background.svelte";
  import { fly } from "svelte/transition";

  const { children, data } = $props();
</script>

{#key data.url}
  <Background
    class="text-ii-text-primary-dark dark:text-ii-text-primary-light bg-ii-background-primary-light dark:bg-ii-background-primary-dark"
  >
    <div
      in:fly={{
        duration: 300,
        delay: 300,
        x: 200,
      }}
      out:fly={{
        duration: 300,
        x: -200,
      }}
    >
      {@render children()}
    </div>
  </Background>
{/key}
