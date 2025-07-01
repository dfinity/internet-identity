<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { fade } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { nonNullish } from "@dfinity/utils";

  type Direction = "up" | "right" | "down" | "left";
  type Align = "start" | "center" | "end";

  type Props = HTMLAttributes<HTMLElement> & {
    anchor?: HTMLElement;
    onClose?: () => void;
    closeOnOutsideClick?: boolean;
    direction?: Direction;
    align?: Align;
    distance?: string;
  };

  let {
    anchor: anchorRef,
    onClose,
    closeOnOutsideClick = true,
    direction = "down",
    align = "center",
    distance = "0px",
    children,
    class: className,
    ...props
  }: Props = $props();

  const MOBILE_BREAKPOINT = 480;

  let popoverRef = $state<HTMLElement>();
  let windowWidth = $state(window.innerWidth);

  $effect(() => {
    let tracking = true;
    const track = () => {
      if (nonNullish(anchorRef) && nonNullish(popoverRef)) {
        const anchorRect = anchorRef.getBoundingClientRect();
        const popoverRect = popoverRef.getBoundingClientRect();
        popoverRef.style.top = {
          up: `calc(${anchorRect.top - popoverRect.height}px - ${distance})`,
          right: {
            start: `${anchorRect.top}px`,
            center: `${anchorRect.top + anchorRect.height * 0.5 - popoverRect.height * 0.5}px`,
            end: `${anchorRect.bottom - popoverRect.height}px`,
          }[align],
          down: `calc(${anchorRect.bottom}px + ${distance})`,
          left: {
            start: `${anchorRect.top}px`,
            center: `${anchorRect.top + anchorRect.height * 0.5 - popoverRect.height * 0.5}px`,
            end: `${anchorRect.bottom - popoverRect.height}px`,
          }[align],
        }[direction];
        popoverRef.style.left = {
          up: {
            start: `${anchorRect.left}px`,
            center: `${anchorRect.right + anchorRect.width * 0.5 - popoverRect.width * 0.5}px`,
            end: `${anchorRect.right - popoverRect.width}px`,
          }[align],
          right: `calc(${anchorRect.right}px + ${distance})`,
          down: {
            start: `${anchorRect.left}px`,
            center: `${anchorRect.left + anchorRect.width * 0.5 - popoverRect.width * 0.5}px`,
            end: `${anchorRect.right - popoverRect.width}px`,
          }[align],
          left: `calc(${anchorRect.left - popoverRect.width}px - ${distance})`,
        }[direction];
      }
      if (tracking) {
        requestAnimationFrame(track);
      }
    };
    requestAnimationFrame(track);
    return () => {
      tracking = false;
    };
  });

  $effect(() => {
    const listener = () => {
      if (!popoverRef?.matches(":popover-open")) {
        onClose?.();
      }
    };
    popoverRef?.addEventListener("toggle", listener);
    return () => popoverRef?.removeEventListener("toggle", listener);
  });
</script>

<svelte:window bind:innerWidth={windowWidth} />

{#if windowWidth >= MOBILE_BREAKPOINT}
  <div
    {...props}
    bind:this={popoverRef}
    popover={closeOnOutsideClick ? "auto" : "manual"}
    in:fade|global={{ duration: 1 }}
    out:fade|global={{ delay: 160, duration: 1 }}
    onintrostart={() => popoverRef?.showPopover()}
    onoutrostart={() => popoverRef?.hidePopover()}
    class="popover fixed overflow-visible bg-transparent"
  >
    <div
      class={[
        "bg-bg-primary_alt border-border-secondary flex w-90 flex-col rounded-xl border p-4 shadow-xl",
        {
          up: {
            start: "origin-bottom-left",
            center: "origin-bottom",
            end: "origin-bottom-right",
          }[align],
          right: {
            start: "origin-top-left",
            center: "origin-left",
            end: "origin-bottom-left",
          }[align],
          down: {
            start: "origin-top-left",
            center: "origin-top",
            end: "origin-top-right",
          }[align],
          left: {
            start: "origin-top-right",
            center: "origin-right",
            end: "origin-bottom-right",
          }[align],
        }[direction],
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
  .popover {
    transition-property: overlay, display;
    transition-duration: 0.16s;
    transition-behavior: allow-discrete;
  }

  .popover > div {
    opacity: 0;
    transform: scale(0.95);
    transition-property: opacity, transform;
    transition-duration: 0.16s;
    transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1);
  }

  .popover:popover-open > div {
    opacity: 1;
    transform: scale(1);
    transition-duration: 0.2s;
  }

  @starting-style {
    .popover:popover-open > div {
      opacity: 0;
      transform: scale(0.95);
    }
  }
</style>
