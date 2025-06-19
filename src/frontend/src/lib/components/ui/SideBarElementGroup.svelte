<script lang="ts">
  import { fade } from "svelte/transition";
  import { page } from "$app/state";
  import { onMount } from "svelte";

  let { children } = $props();
  let groupRef = $state<HTMLDivElement>();
  let hoveredAnchor = $state<HTMLAnchorElement>();
  let activeAnchor = $derived.by(getActiveAnchor);
  let highlightStyle = $state("");

  onMount(() => {
    setTimeout(() => {
      // Tick doesn't work here
      activeAnchor = getActiveAnchor();
      updateHighlight();
    }, 0);
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

  function updateHighlight() {
    const anchor = hoveredAnchor ?? activeAnchor;
    if (anchor && groupRef) {
      const anchorRect = anchor.getBoundingClientRect();
      highlightStyle = `
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
  onpointerover={handlePointerOver}
  onpointerout={handlePointerOut}
  class="z-2"
>
  {@render children?.()}
  {#if hoveredAnchor || activeAnchor}
    <div
      class="bg-bg-active border-border-secondary pointer-events-none rounded-sm border"
      style={highlightStyle}
      transition:fade={{ duration: 150 }}
    ></div>
  {/if}
</div>
