<script lang="ts">
  /**
   * A blur that deepens toward one edge, after Magic UI's Progressive Blur
   * (https://magicui.design/docs/components/progressive-blur): a stack of
   * backdrop blurs, each masked to a band one step nearer the edge and twice
   * as strong as the one before it.
   */
  import type { ClassValue } from "svelte/elements";
  import { useSurface } from "./Elevated.svelte";

  interface Props {
    /** The edge where the blur is strongest. */
    side: "top" | "bottom" | "left" | "right";
    /** Also fade into the surface underneath, so what passes the edge is gone
     *  by the time it gets there rather than cut off. */
    fade?: boolean;
    /** With `fade`, this much at the edge is the surface itself, for what
     *  stands there (a header's controls); the blur and the fade take the rest
     *  of the box. Any CSS length. */
    hold?: string;
    /** Size and placement; the blur fills whatever box this gives it. */
    class?: ClassValue;
  }

  const { side, fade = false, hold, class: className }: Props = $props();

  // The surface is fixed for a component's life, like the place it renders in.
  const surface = `var(--ii-surface-${useSurface()})`;

  /** From 0.5px at the inner edge to 64px at the outer one. */
  const LEVELS = [0.5, 1, 2, 4, 8, 16, 32, 64];
  const BAND = 100 / LEVELS.length;

  /** A layer fades in over one band, holds for two and fades out over the
   *  next; the strongest holds right up to the edge. */
  const layerStyle = (level: number, index: number) => {
    const from = index * BAND;
    const stops =
      index === LEVELS.length - 1
        ? `transparent ${from}%, black 100%`
        : `transparent ${from}%, black ${from + BAND}%, black ${from + 2 * BAND}%, transparent ${from + 3 * BAND}%`;
    const mask = `linear-gradient(to ${side}, ${stops})`;
    return [
      `backdrop-filter: blur(${level}px)`,
      `-webkit-backdrop-filter: blur(${level}px)`,
      `mask-image: ${mask}`,
      `-webkit-mask-image: ${mask}`,
    ].join("; ");
  };

  /** Clear for the first third, then easing into the surface at the edge. */
  const fadeStyle = $derived(
    `background: linear-gradient(to ${side}, transparent 30%, color-mix(in srgb, ${surface} 60%, transparent) 70%, ${surface})`,
  );

  const OPPOSITE = {
    top: "bottom",
    bottom: "top",
    left: "right",
    right: "left",
  };
  const holdSize = $derived(
    side === "top" || side === "bottom" ? "height" : "width",
  );
</script>

<div
  aria-hidden="true"
  class={[
    "pointer-events-none absolute",
    {
      top: "inset-x-0 top-0",
      bottom: "inset-x-0 bottom-0",
      left: "inset-y-0 left-0",
      right: "inset-y-0 right-0",
    }[side],
    className,
  ]}
>
  <div
    class="absolute inset-0"
    style={fade && hold !== undefined ? `${side}: ${hold}` : undefined}
  >
    {#each LEVELS as level, index (level)}
      <div class="absolute inset-0" style={layerStyle(level, index)}></div>
    {/each}
    {#if fade}
      <div class="absolute inset-0" style={fadeStyle}></div>
    {/if}
  </div>
  {#if fade && hold !== undefined}
    <div
      class="absolute inset-0"
      style={`${OPPOSITE[side]}: auto; ${holdSize}: ${hold}; background: ${surface}`}
    ></div>
  {/if}
</div>
