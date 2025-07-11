<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";

  type Direction = "up" | "right" | "down" | "left";
  type Align = "start" | "center" | "end";

  type Props = HTMLAttributes<HTMLElement> & {
    label: string;
    description?: string;
    direction?: Direction;
    align?: Align;
    distance?: string;
    hidden?: boolean;
    arrow?: boolean;
    manual?: boolean; // Always render tooltip, even when not hovered
    anchor?: HTMLElement;
  };

  const id = $props.id();
  let {
    label,
    description,
    direction = "up",
    align = "center",
    distance = "0.5rem",
    hidden = false,
    manual = false,
    arrow = true,
    anchor,
    children,
    class: className,
    ...restProps
  }: Props = $props();

  let wrapperRef = $state<HTMLElement>();
  let tooltipRef = $state<HTMLElement>();
  let isTooltipVisible = $state(manual ? !hidden : false);

  const anchorRef = $derived(
    anchor ??
      ((wrapperRef?.firstElementChild ?? undefined) as HTMLElement | undefined),
  );

  $effect(() => {
    if (!isTooltipVisible) {
      return;
    }
    let tracking = true;
    const padding = "0.75rem";
    const track = () => {
      if (nonNullish(anchorRef) && nonNullish(tooltipRef)) {
        const anchorRect = anchorRef.getBoundingClientRect();
        const tooltipRect = tooltipRef.getBoundingClientRect();
        tooltipRef.style.top = {
          up: `calc(${anchorRect.top - tooltipRect.height}px - ${distance})`,
          right: {
            start: `calc(${anchorRect.top}px - ${padding})`,
            center: `${anchorRect.top + anchorRect.height * 0.5 - tooltipRect.height * 0.5}px`,
            end: `calc(${anchorRect.bottom - tooltipRect.height}px + ${padding})`,
          }[align],
          down: `calc(${anchorRect.bottom}px + ${distance})`,
          left: {
            start: `calc(${anchorRect.top}px - ${padding})`,
            center: `${anchorRect.top + anchorRect.height * 0.5 - tooltipRect.height * 0.5}px`,
            end: `calc(${anchorRect.bottom - tooltipRect.height}px + ${padding})`,
          }[align],
        }[direction];
        tooltipRef.style.left = {
          up: {
            start: `calc(${anchorRect.left}px - ${padding})`,
            center: `${anchorRect.left + anchorRect.width * 0.5 - tooltipRect.width * 0.5}px`,
            end: `calc(${anchorRect.right - tooltipRect.width}px + ${padding})`,
          }[align],
          right: `calc(${anchorRect.right}px + ${distance})`,
          down: {
            start: `calc(${anchorRect.left}px - ${padding})`,
            center: `${anchorRect.left + anchorRect.width * 0.5 - tooltipRect.width * 0.5}px`,
            end: `calc(${anchorRect.right - tooltipRect.width}px + ${padding})`,
          }[align],
          left: `calc(${anchorRect.left - tooltipRect.width}px - ${distance})`,
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
    if (isTooltipVisible) {
      tooltipRef?.showPopover();
    } else {
      tooltipRef?.hidePopover();
    }
  });

  $effect(() => {
    if (manual) {
      isTooltipVisible = !hidden;
    }
  });
</script>

{#if !manual && hidden}
  {@render children?.()}
{:else}
  <!-- Wrapper used for event handlers and (optionally) anchoring -->
  <!-- svelte-ignore a11y_no_static_element_interactions -->
  <div
    bind:this={wrapperRef}
    onmouseenter={() => !manual && (isTooltipVisible = true)}
    onmouseleave={() => !manual && (isTooltipVisible = false)}
    onfocusin={() => !manual && (isTooltipVisible = true)}
    onfocusout={() => !manual && (isTooltipVisible = false)}
    ontouchend={() => !manual && (isTooltipVisible = !isTooltipVisible)}
    class="contents"
    aria-describedby={id}
  >
    {@render children?.()}
  </div>
  <!-- Tooltip wrapper that doesn't animate so its dimensions don't change -->
  <div
    {id}
    {...restProps}
    bind:this={tooltipRef}
    popover={"manual"}
    class="tooltip pointer-events-none fixed overflow-visible bg-transparent"
    role="tooltip"
  >
    <!-- Tooltip inner container that animates -->
    <div
      class={[
        "bg-fg-primary relative flex max-w-80 flex-col items-start rounded-lg p-3 drop-shadow-lg",
        {
          up: {
            start: "origin-[1.6rem_100%]",
            center: "origin-bottom",
            end: "origin-[calc(100%-1.6rem)_100%]",
          }[align],
          right: {
            start: "origin-[0_1.6rem]",
            center: "origin-left",
            end: "origin-[0_calc(100%-1.6rem)]",
          }[align],
          down: {
            start: "origin-[1.6rem_0]",
            center: "origin-top",
            end: "origin-[calc(100%-1.6rem)_0]",
          }[align],
          left: {
            start: "origin-[100%_1.6rem]",
            center: "origin-right",
            end: "origin-[100%_calc(100%-1.6rem)]",
          }[align],
        }[direction],
        className,
      ]}
    >
      <!-- Tooltip content -->
      <span class="text-bg-primary text-start text-xs font-semibold"
        >{label}</span
      >
      {#if nonNullish(description)}
        <span class="text-bg-tertiary mt-1 text-start text-xs font-medium">
          {description}
        </span>
      {/if}
      <!-- Tooltip arrow -->
      {#if arrow}
        <div
          class={[
            "absolute size-0",
            {
              up: {
                start: "bottom-0 left-7",
                center: "bottom-0 left-[50%]",
                end: "right-7 bottom-0",
              }[align],
              right: {
                start: "top-7 left-0",
                center: "top-[50%] left-0",
                end: "bottom-7 left-0",
              }[align],
              down: {
                start: "top-0 left-7",
                center: "top-0 left-[50%]",
                end: "top-0 right-7",
              }[align],
              left: {
                start: "top-7 right-0",
                center: "top-[50%] right-0",
                end: "right-0 bottom-7",
              }[align],
            }[direction],
          ]}
        >
          <div
            class={[
              "bg-fg-primary size-2 origin-center -translate-1 rotate-45",
              {
                up: "rounded-br-xs",
                right: "rounded-bl-xs",
                down: "rounded-tl-xs",
                left: "rounded-tr-xs",
              }[direction],
            ]}
          ></div>
        </div>
      {/if}
    </div>
  </div>
{/if}

<style>
  .tooltip {
    transition-property: overlay, display;
    transition-duration: 0.16s;
    transition-behavior: allow-discrete;
  }

  .tooltip > div {
    opacity: 0;
    transform: scale(0.95);
    transition-property: opacity, transform;
    transition-duration: 0.16s;
    transition-timing-function: cubic-bezier(0.4, 0, 0.2, 1);
  }

  .tooltip:popover-open > div {
    opacity: 1;
    transform: scale(1);
    transition-duration: 0.2s;
  }

  @starting-style {
    .tooltip:popover-open > div {
      opacity: 0;
      transform: scale(0.95);
    }
  }
</style>
