<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Direction = "up" | "right" | "down" | "left";
  type Align = "start" | "center" | "end";

  type Props = HTMLAttributes<HTMLElement> & {
    label: string;
    description?: string;
    direction?: Direction;
    align?: Align;
    distance?: string;
    offset?: string;
    hidden?: boolean;
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
    offset = "0.75rem",
    hidden = false,
    manual = false,
    anchor,
    children,
    class: className,
    ...restProps
  }: Props = $props();

  let wrapperRef = $state<HTMLElement | null>();
  let tooltipRef = $state<HTMLElement | null>();
  let isTooltipVisible = $state(manual ? !hidden : false);

  const getInlineAlign = (value: Align): Align => {
    if (document.documentElement.dir !== "rtl") {
      return value;
    }

    if (value === "start") {
      return "end";
    }

    if (value === "end") {
      return "start";
    }

    return value;
  };

  const anchorRef = $derived(
    anchor ??
      ((wrapperRef?.firstElementChild ?? undefined) as HTMLElement | undefined),
  );

  // CSS length parser — supports the units we actually pass in (px, rem, em).
  // Falls back to 0 for anything we can't resolve.
  const toPx = (value: string): number => {
    const n = parseFloat(value);
    if (Number.isNaN(n)) return 0;
    if (value.endsWith("rem") || value.endsWith("em")) {
      const rootSize = getComputedStyle(document.documentElement).fontSize;
      return n * (rootSize !== "" ? parseFloat(rootSize) : 16);
    }
    return n;
  };

  $effect(() => {
    if (!isTooltipVisible) {
      return;
    }
    let tracking = true;
    const track = () => {
      if (
        anchorRef !== undefined &&
        anchorRef !== null &&
        tooltipRef !== undefined &&
        tooltipRef !== null
      ) {
        const inlineAlign = getInlineAlign(align);
        const anchorRect = anchorRef.getBoundingClientRect();
        const tooltipRect = tooltipRef.getBoundingClientRect();
        const distancePx = toPx(distance);
        const offsetPx = toPx(offset);

        tooltipRef.style.inset = "auto";
        tooltipRef.style.right = "auto";
        tooltipRef.style.bottom = "auto";

        // Compute (top, left) for a given side direction.
        const computeTop = (dir: Direction): number => {
          switch (dir) {
            case "up":
              return anchorRect.top - tooltipRect.height - distancePx;
            case "down":
              return anchorRect.bottom + distancePx;
            case "right":
            case "left":
              return align === "start"
                ? anchorRect.top - offsetPx
                : align === "end"
                  ? anchorRect.bottom - tooltipRect.height + offsetPx
                  : anchorRect.top +
                    anchorRect.height * 0.5 -
                    tooltipRect.height * 0.5;
          }
        };
        const computeLeft = (dir: Direction): number => {
          switch (dir) {
            case "right":
              return anchorRect.right + distancePx;
            case "left":
              return anchorRect.left - tooltipRect.width - distancePx;
            case "up":
            case "down":
              return inlineAlign === "start"
                ? anchorRect.left - offsetPx
                : inlineAlign === "end"
                  ? anchorRect.right - tooltipRect.width + offsetPx
                  : anchorRect.left +
                    anchorRect.width * 0.5 -
                    tooltipRect.width * 0.5;
          }
        };

        // Score how far a candidate position lies outside the viewport
        // (lower is better; 0 means fully visible).
        const margin = 4;
        const overflow = (top: number, left: number): number =>
          Math.max(0, margin - left) +
          Math.max(0, left + tooltipRect.width - (window.innerWidth - margin)) +
          Math.max(0, margin - top) +
          Math.max(0, top + tooltipRect.height - (window.innerHeight - margin));

        // Try the requested direction first, then flip to the opposite side,
        // then fall back to the perpendicular axis.
        const opposite: Record<Direction, Direction> = {
          up: "down",
          down: "up",
          left: "right",
          right: "left",
        };
        const perpendicular: Record<Direction, Direction[]> = {
          up: ["right", "left"],
          down: ["right", "left"],
          left: ["down", "up"],
          right: ["down", "up"],
        };
        const order: Direction[] = [
          direction,
          opposite[direction],
          ...perpendicular[direction],
        ];

        let bestTop = computeTop(direction);
        let bestLeft = computeLeft(direction);
        let bestScore = overflow(bestTop, bestLeft);
        for (let i = 1; i < order.length && bestScore > 0; i++) {
          const candTop = computeTop(order[i]);
          const candLeft = computeLeft(order[i]);
          const candScore = overflow(candTop, candLeft);
          if (candScore < bestScore) {
            bestTop = candTop;
            bestLeft = candLeft;
            bestScore = candScore;
          }
        }

        // If even the best candidate clips, clamp into the viewport so the
        // tooltip is fully visible even when no anchored placement fits.
        if (bestScore > 0) {
          bestLeft = Math.max(
            margin,
            Math.min(bestLeft, window.innerWidth - tooltipRect.width - margin),
          );
          bestTop = Math.max(
            margin,
            Math.min(bestTop, window.innerHeight - tooltipRect.height - margin),
          );
        }

        tooltipRef.style.top = `${bestTop}px`;
        tooltipRef.style.left = `${bestLeft}px`;
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

  $effect(() => {
    wrapperRef?.firstElementChild?.setAttribute("aria-describedby", id);
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
  >
    {@render children?.()}
  </div>
  <!-- Tooltip wrapper that doesn't animate so its dimensions don't change -->
  <div
    {id}
    {...restProps}
    bind:this={tooltipRef}
    popover="manual"
    class="tooltip pointer-events-none fixed overflow-visible bg-transparent"
    role="tooltip"
  >
    <!-- Tooltip inner container that animates -->
    <div
      class={[
        "bg-bg-tertiary border-border-secondary relative flex max-w-80 flex-col items-start rounded-lg border px-3 py-2 shadow-lg shadow-black/6",
        {
          up: {
            start: "origin-[1.6rem_100%]",
            center: "origin-bottom",
            end: "origin-[calc(100%-1.6rem)_100%]",
          }[getInlineAlign(align)],
          right: {
            start: "origin-[0_1.6rem]",
            center: "origin-left",
            end: "origin-[0_calc(100%-1.6rem)]",
          }[align],
          down: {
            start: "origin-[1.6rem_0]",
            center: "origin-top",
            end: "origin-[calc(100%-1.6rem)_0]",
          }[getInlineAlign(align)],
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
      <span class="text-text-primary text-start text-xs font-semibold">
        {label}
      </span>
      {#if description !== undefined}
        <span class="text-text-tertiary mt-1 text-start text-xs font-medium">
          {description}
        </span>
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
