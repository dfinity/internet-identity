<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { onMount } from "svelte";
  import { fade } from "svelte/transition";
  import anchorPolyfill from "@oddbird/css-anchor-positioning/fn";
  import Dialog from "$lib/components/ui/Dialog.svelte";

  type Props = HTMLAttributes<HTMLElement> & {
    onClose: () => void;
  };

  let { onClose, children, class: className, ...props }: Props = $props();

  const supportsAnchorPosition = "anchorName" in document.documentElement.style;

  let outer = $state<HTMLElement>();
  let inner = $state<HTMLElement>();
  let element = $derived(supportsAnchorPosition ? inner : outer);
  let windowWidth = $state(window.innerWidth);

  onMount(() => {
    const listener = () => {
      if (!element?.matches(":popover-open")) {
        onClose();
      }
    };
    element?.addEventListener("toggle", listener);
    return () => element?.removeEventListener("toggle", listener);
  });
</script>

<svelte:window bind:innerWidth={windowWidth} />

{#if windowWidth >= 480}
  <div
    bind:this={outer}
    popover={supportsAnchorPosition ? undefined : "auto"}
    class="popover-outer"
    in:fade|global={{ duration: 1 }}
    out:fade|global={{ delay: 160, duration: 1 }}
    onintrostart={async () => {
      if (!supportsAnchorPosition) {
        await anchorPolyfill();
      }
      element?.showPopover();
    }}
    onoutrostart={() => element?.hidePopover()}
  >
    <div
      {...props}
      bind:this={inner}
      popover={supportsAnchorPosition ? "auto" : undefined}
      class={[
        "popover-inner bg-bg-primary_alt border-border-secondary text-text-primary relative flex w-90 flex-col rounded-xl border p-4 shadow-xl",
        className,
      ]}
    >
      {@render children?.()}
    </div>
  </div>
{:else}
  <Dialog {onClose} showCloseButton={false}>
    {@render children?.()}
  </Dialog>
{/if}

<style>
  .popover-outer {
    overflow: visible;
  }

  .popover-outer:not([popover]) {
    display: contents;
  }

  .popover-inner {
    position: absolute;
    opacity: 0;
    transform: scale(0.95);
    transition-property: opacity, transform, overlay, display;
    transition-duration: 0.16s;
    transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1);
    transition-behavior: allow-discrete;
  }

  .popover-outer:popover-open .popover-inner,
  .popover-inner:popover-open {
    opacity: 1;
    transform: scale(1);
    transition-duration: 0.2s;
  }

  @starting-style {
    .popover-outer:popover-open .popover-inner,
    .popover-inner:popover-open {
      opacity: 0;
      transform: scale(0.95);
    }
  }
</style>
