<script lang="ts">
  import { fade } from "svelte/transition";
  import { page } from "$app/state";
  import { onDestroy, onMount } from "svelte";
  import { Spring } from "svelte/motion";
  import { afterNavigate } from "$app/navigation";

  let { children, bindableGroupRef = $bindable<HTMLDivElement>() } = $props();
  let groupRef = $state<HTMLDivElement>();
  let hoveredAnchor = $state<HTMLAnchorElement>();
  let activeAnchor = $derived.by(getActiveAnchor);
  let hoveredStyle = $state("");
  let activeStyle = $state("");

  let hoveredCoords = new Spring(
    { x: 0, y: 0 },
    {
      stiffness: 0.5,
      damping: 0.5,
    },
  );
  let activeCoords = new Spring(
    { x: 0, y: 0 },
    {
      stiffness: 0.6,
      damping: 0.6,
    },
  );
  let loaded = $state(false);

  onMount(() => {
    setTimeout(() => {
      // Tick doesn't work here
      groupRef?.childNodes.forEach((node) => {
        // Click listener on div is not great for aria
        // So we just add to kids
        node.addEventListener("click", handleClick);
      });
      activeAnchor = getActiveAnchor();
      updateHighlight(true);
      loaded = true;
    }, 0);
  });

  onDestroy(() => {
    groupRef?.childNodes.forEach((node) => {
      node.removeEventListener("click", handleClick);
    });
  });

  afterNavigate(() => {
    updateHighlight();
  });

  function getActiveAnchor() {
    if (!groupRef) return;
    const anchors = Array.from(groupRef.querySelectorAll("a"));
    const currentPath = page.url.pathname;
    return anchors.find((a) => a.getAttribute("href") === currentPath);
  }

  const handlePointerOver = (e: PointerEvent) => {
    if (!groupRef) return;

    const anchor = (e.target as HTMLElement).closest("a");

    if (anchor && anchor.parentElement === groupRef) {
      hoveredAnchor = anchor;
      updateHighlight();
    }
  };

  const handlePointerOut = (e: PointerEvent) => {
    if (!groupRef) return;
    // Only clear if leaving the anchor, not moving between children
    const related = (e.relatedTarget as HTMLElement)?.closest("a");
    if (!related || related.parentElement !== groupRef) {
      hoveredAnchor = undefined;
      updateHighlight();
    }
  };

  const handleClick = (e: Event) => {
    if (!groupRef) return;

    const anchor = (e.target as HTMLElement).closest("a");

    if (anchor && anchor.parentElement === groupRef) {
      activeAnchor = anchor;
      updateHighlight();
    }
  };

  function updateHighlight(instant?: boolean) {
    const anchor = hoveredAnchor ?? activeAnchor;
    if (anchor && groupRef) {
      const { x, y, height, width } = anchor.getBoundingClientRect();
      if (instant) {
        hoveredCoords.set({ x, y }, { instant });
      } else {
        hoveredCoords.target = { x, y };
      }
      hoveredStyle = `
        width: ${width}px;
        height: ${height}px;
      `;
    }
    if (activeAnchor) {
      const { x, y, height, width } = activeAnchor.getBoundingClientRect();

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
  bind:this={groupRef}
  bind:this={bindableGroupRef}
  onpointerover={handlePointerOver}
  onpointerout={handlePointerOut}
  class="z-2 flex flex-col gap-1"
>
  {@render children?.()}
  {#if activeAnchor && loaded}
    <div
      class="bg-bg-active top-[] pointer-events-none absolute -z-[1] rounded-sm"
      style={`top: ${activeCoords.current.y}px; left: ${activeCoords.current.x}px; ${activeStyle}`}
      transition:fade={{ duration: 150 }}
    ></div>
  {/if}
  {#if (hoveredAnchor || activeAnchor) && loaded}
    <div
      class="border-border-secondary pointer-events-none absolute -z-[1] rounded-sm border"
      style={`top: ${hoveredCoords.current.y}px; left: ${hoveredCoords.current.x}px; ${hoveredStyle}`}
      transition:fade={{ duration: 150 }}
    ></div>
  {/if}
</div>
