<script lang="ts">
  import { page } from "$app/state";
  import { onMount } from "svelte";
  import { Spring } from "svelte/motion";
  import { fade } from "svelte/transition";

  let { children, bindableGroupRef = $bindable() } = $props();
  let groupRef = $state<HTMLDivElement>();
  let activeAnchor = $derived.by(getActiveAnchor);
  let activeStyle = $state("");
  let loaded = $state(false);

  let activeCoords = new Spring(
    { x: 0, y: 0 },
    {
      stiffness: 0.6,
      damping: 0.6,
    },
  );

  onMount(() => {
    setTimeout(() => {
      // Tick doesn't work here
      activeAnchor = getActiveAnchor();
      updateHighlight();
      loaded = true;
    }, 0);
  });

  $effect(() => {
    if (page.url.pathname) {
      activeAnchor = getActiveAnchor();
      updateHighlight();
    }
  });

  function getActiveAnchor() {
    if (!groupRef) return;
    const anchors = Array.from(groupRef.querySelectorAll("a"));
    const currentPath = page.url.pathname;
    return anchors.find((a) => a.getAttribute("href") === currentPath);
  }

  function updateHighlight(instant?: boolean) {
    const anchor = activeAnchor;
    if (anchor && groupRef) {
      const { x, y, height, width } = anchor.getBoundingClientRect();

      if (instant) {
        activeCoords.set({ x, y }, { instant });
      } else {
        activeCoords.target = { x, y };
      }
      activeStyle = `
        width: ${width}px;
        height: ${height}px;
      `;
    }
  }
</script>

<div
  class="border-border-secondary z-2 flex flex-row gap-4 border-b pb-2"
  bind:this={groupRef}
  bind:this={bindableGroupRef}
>
  {@render children?.()}
  {#if activeAnchor && loaded}
    <div
      class="border-fg-primary pointer-events-none absolute border-b-2"
      style={`top: ${activeCoords.current.y + 8}px; left: ${activeCoords.current.x}px; ${activeStyle}`}
      transition:fade={{ duration: 150 }}
    ></div>
  {/if}
</div>
