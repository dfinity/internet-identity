<script lang="ts">
  import { fade } from "svelte/transition";

  let { children } = $props();
  let groupRef = $state<HTMLDivElement>();
  let hoveredAnchor = $state<HTMLAnchorElement>();
  let highlightStyle = $state("");

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
    }
  };

  function updateHighlight() {
    if (hoveredAnchor && groupRef) {
      const anchorRect = hoveredAnchor.getBoundingClientRect();
      const groupRect = groupRef.getBoundingClientRect();
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
  {#if hoveredAnchor}
    <div
      class="bg-bg-active border-border-secondary pointer-events-none rounded-sm border"
      style={highlightStyle}
      transition:fade={{ duration: 50 }}
    ></div>
  {/if}
  {@render children?.()}
</div>
