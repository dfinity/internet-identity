<script lang="ts">
  import { onMount } from "svelte";

  let {
    children,
    appearTime,
    disappearTime,
  }: { children: any; appearTime: Date; disappearTime: Date } = $props();

  let renderChildren = $state(false);

  function checkTime() {
    const now = Date.now();
    renderChildren =
      now >= appearTime.getTime() && now <= disappearTime.getTime();
  }

  onMount(() => {
    checkTime();
    const interval = setInterval(checkTime, 1000);

    return () => {
      clearInterval(interval);
    };
  });
</script>

{#if renderChildren}
  {@render children?.()}
{/if}
