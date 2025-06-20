<script lang="ts">
  import { fade } from "svelte/transition";
  import { page } from "$app/state";
  import { onDestroy, onMount } from "svelte";
  import { Spring } from "svelte/motion";

  let { children, bindableGroupRef = $bindable<HTMLDivElement>() } = $props();
  let groupRef = $state<HTMLDivElement>();
  let hoveredAnchor = $state<HTMLAnchorElement>();
  let activeAnchor = $derived.by(getActiveAnchor);
  let hoveredStyle = $state("");
  let activeStyle = $state("");
  let pullOpts = {
    stiffness: 0.1,
    damping: 0.25,
  };
  let releaseOpts = {
    stiffness: 0.1,
    damping: 0.25,
  };
  let hoveredCoords = new Spring({ x: 0, y: 0 });
  let activeCoords = new Spring({ x: 0, y: 0 });
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
      updateHighlight();
      loaded = true;
    }, 0);
  });

  onDestroy(() => {
    groupRef?.childNodes.forEach((node) => {
      node.removeEventListener("click", handleClick);
    });
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

  function updateHighlight() {
    const anchor = hoveredAnchor ?? activeAnchor;
    if (anchor && groupRef) {
      const anchorRect = anchor.getBoundingClientRect();
      hoveredStyle = `
        position: absolute;
        top: ${anchorRect.top}px;
        left: ${anchorRect.left}px;
        width: ${anchorRect.width}px;
        height: ${anchorRect.height}px;
        z-index: -1;
        transition: all 0.15s cubic-bezier(.4,1,.4,1);
      `;
    }
    if (activeAnchor) {
      const anchorRect = activeAnchor.getBoundingClientRect();
      activeStyle = `
        position: absolute;
        top: ${anchorRect.top}px;
        left: ${anchorRect.left}px;
        width: ${anchorRect.width}px;
        height: ${anchorRect.height}px;
        z-index: -1;
        transition: all 0.15s cubic-bezier(.4,1,.4,1);
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
      class="bg-bg-active pointer-events-none rounded-sm"
      style={activeStyle}
      transition:fade={{ duration: 150 }}
    ></div>
  {/if}
  {#if (hoveredAnchor || activeAnchor) && loaded}
    <div
      class=" border-border-secondary pointer-events-none rounded-sm border"
      style={hoveredStyle}
      transition:fade={{ duration: 150 }}
    ></div>
  {/if}
</div>
