<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { onMount } from "svelte";
  import { channelStore } from "$lib/stores/channelStore";

  const { children }: LayoutProps = $props();

  onMount(() => {
    channelStore.establish();
  });
</script>

{#if $channelStore !== undefined}
  {@render children()}
{/if}
