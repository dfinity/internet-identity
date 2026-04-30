<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { fade } from "svelte/transition";
  import Dialog from "$lib/components/ui/Dialog.svelte";

  type Direction = "up" | "right" | "down" | "left";
  type Align = "start" | "center" | "end";

  type Props = HTMLAttributes<HTMLElement> & {
    anchor?: HTMLElement | null;
    onClose?: () => void;
    direction?: Direction;
    align?: Align;
    distance?: string;
    responsive?: boolean;
    flip?: boolean;
  };

  let {
    anchor: anchorRef,
    onClose,
    direction = "down",
    align = "center",
    distance = "0px",
    children,
    class: className,
    responsive = true,
    flip = true,
    ...props
  }: Props = $props();

  const MOBILE_BREAKPOINT = 480;

  let popoverRef = $state<HTMLElement | null>();
  let windowWidth = $state(window.innerWidth);

  const getInlineAlign = (align: Align): Align => {
    if (document.documentElement.dir !== "rtl") {
      return align;
    }

    if (align === "start") {
      return "end";
    }

    if (align === "end") {
      return "start";
    }

    return align;
  };

  $effect(() => {
    let tracking = true;

    const track = () => {
      if (
        anchorRef !== undefined &&
        anchorRef !== null &&
        popoverRef !== undefined &&
        popoverRef !== null
      ) {
        const anchorRect = anchorRef.getBoundingClientRect();
        const popoverRect = popoverRef.getBoundingClientRect();

        popoverRef.style.inset = "auto";
        popoverRef.style.right = "auto";
        popoverRef.style.bottom = "auto";

        // Available space around the anchor
        const spaceAbove = anchorRect.top;
        const spaceBelow = window.innerHeight - anchorRect.bottom;
        const spaceLeft = anchorRect.left;
        const spaceRight = window.innerWidth - anchorRect.right;

        // Determine flipped direction if needed
        let finalDirection = direction;

        // Vertical flip
        if (
          flip &&
          direction === "down" &&
          spaceBelow < popoverRect.height &&
          spaceAbove > spaceBelow
        ) {
          finalDirection = "up";
        } else if (
          flip &&
          direction === "up" &&
          spaceAbove < popoverRect.height &&
          spaceBelow > spaceAbove
        ) {
          finalDirection = "down";
        }

        // Horizontal flip
        if (
          flip &&
          direction === "right" &&
          spaceRight < popoverRect.width &&
          spaceLeft > spaceRight
        ) {
          finalDirection = "left";
        } else if (
          flip &&
          direction === "left" &&
          spaceLeft < popoverRect.width &&
          spaceRight > spaceLeft
        ) {
          finalDirection = "right";
        }

        // Compute top position
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
        }[finalDirection];

        // Compute left position
        const inlineAlign = getInlineAlign(align);

        popoverRef.style.left = {
          up: {
            start: `${anchorRect.left}px`,
            center: `${anchorRect.left + anchorRect.width * 0.5 - popoverRect.width * 0.5}px`,
            end: `${anchorRect.right - popoverRect.width}px`,
          }[inlineAlign],
          right: `calc(${anchorRect.right}px + ${distance})`,
          down: {
            start: `${anchorRect.left}px`,
            center: `${anchorRect.left + anchorRect.width * 0.5 - popoverRect.width * 0.5}px`,
            end: `${anchorRect.right - popoverRect.width}px`,
          }[inlineAlign],
          left: `calc(${anchorRect.left - popoverRect.width}px - ${distance})`,
        }[finalDirection];
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
</script>

<svelte:window bind:innerWidth={windowWidth} />

{#if windowWidth >= MOBILE_BREAKPOINT || !responsive}
  <div
    class="fixed inset-0 z-10"
    role="presentation"
    tabindex="-1"
    onclick={() => onClose?.()}
    onkeydown={(e) => {
      if (e.key === "Escape" && document.activeElement?.closest(".popover")) {
        e.stopPropagation();
        onClose?.();
      }
    }}
  ></div>
  <div
    {...props}
    bind:this={popoverRef}
    popover="manual"
    in:fade|global={{ duration: 1 }}
    out:fade|global={{ delay: 160, duration: 1 }}
    ontoggle={() => {
      if (popoverRef?.matches(":popover-open") === true) {
        return;
      }
      onClose?.();
    }}
    onintrostart={() => popoverRef?.showPopover()}
    onoutrostart={() => popoverRef?.hidePopover()}
    onfocusout={(e) => {
      if (
        e.relatedTarget instanceof Node &&
        popoverRef?.contains(e.relatedTarget) === true
      ) {
        return;
      }
      anchorRef?.querySelector("button")?.focus();
      onClose?.();
    }}
    class="popover fixed overflow-visible bg-transparent"
  >
    <div
      class={[
        "border-border-secondary bg-bg-primary_alt relative flex w-90 flex-col overflow-clip rounded-xl border shadow-xl",
        {
          up: {
            start: "origin-bottom-left",
            center: "origin-bottom",
            end: "origin-bottom-right",
          }[getInlineAlign(align)],
          right: {
            start: "origin-top-left",
            center: "origin-left",
            end: "origin-bottom-left",
          }[align],
          down: {
            start: "origin-top-left",
            center: "origin-top",
            end: "origin-top-right",
          }[getInlineAlign(align)],
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
  <Dialog
    {onClose}
    showCloseButton={false}
    class={className}
    contentClass="!p-0"
  >
    {@render children?.()}
  </Dialog>
{/if}

<style>
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

  .popover[inert] > div {
    opacity: 0;
    transform: scale(0.95);
  }

  @starting-style {
    .popover:popover-open > div {
      opacity: 0;
      transform: scale(0.95);
    }
  }
</style>
