<script lang="ts">
  import { page } from "$app/state";
  import { onMount } from "svelte";
  import { fade } from "svelte/transition";

  const { children } = $props();
  let groupRef = $state<HTMLDivElement>();
  let activeAnchor = $derived.by(getActiveAnchor);
  let highlightStyle = $state("");

  onMount(() => {
    setTimeout(() => {
      // Tick doesn't work here
      activeAnchor = getActiveAnchor();
      updateHighlight();
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

  function updateHighlight() {
    const anchor = activeAnchor;
    if (anchor && groupRef) {
      const headingRect = anchor.querySelector("h1")?.getBoundingClientRect();

      if (!headingRect) return;

      highlightStyle = `
        position: absolute;
        top: ${headingRect.top + 8}px;
        left: ${headingRect.left}px;
        width: ${headingRect.width}px;
        height: ${headingRect.height}px;
        z-index: -1;
        transition: all 0.15s cubic-bezier(.4,1,.4,1);
      `;
    }
  }
</script>

<div
  class="border-border-secondary z-2 flex flex-row gap-4 border-b pb-2"
  bind:this={groupRef}
>
  {@render children?.()}
  {#if activeAnchor}
    <div
      class="border-fg-primary pointer-events-none border-b-2"
      style={highlightStyle}
      transition:fade={{ duration: 150 }}
    ></div>
  {/if}
</div>
