<script lang="ts">
  /**
   * A blur that deepens toward one edge, after Magic UI's Progressive Blur
   * (https://magicui.design/docs/components/progressive-blur): a stack of
   * backdrop blurs, each masked to a band one step nearer the edge and twice
   * as strong as the one before it.
   */
  import type { ClassValue } from "svelte/elements";

  interface Props {
    /** The edge where the blur is strongest. */
    side: "left" | "right";
    /** Size and placement; the blur fills whatever box this gives it. */
    class?: ClassValue;
  }

  const { side, class: className }: Props = $props();

  /** From 0.5px at the inner edge to 64px at the outer one. */
  const LEVELS = [0.5, 1, 2, 4, 8, 16, 32, 64];
  const BAND = 100 / LEVELS.length;

  const towardEdge = $derived(side === "left" ? "to left" : "to right");

  /** A layer fades in over one band, holds for two and fades out over the
   *  next; the strongest holds right up to the edge. */
  const layerStyle = (level: number, index: number) => {
    const from = index * BAND;
    const stops =
      index === LEVELS.length - 1
        ? `transparent ${from}%, black 100%`
        : `transparent ${from}%, black ${from + BAND}%, black ${from + 2 * BAND}%, transparent ${from + 3 * BAND}%`;
    const mask = `linear-gradient(${towardEdge}, ${stops})`;
    return [
      `backdrop-filter: blur(${level}px)`,
      `-webkit-backdrop-filter: blur(${level}px)`,
      `mask-image: ${mask}`,
      `-webkit-mask-image: ${mask}`,
    ].join("; ");
  };
</script>

<div
  aria-hidden="true"
  class={[
    "pointer-events-none absolute inset-y-0",
    side === "left" ? "left-0" : "right-0",
    className,
  ]}
>
  {#each LEVELS as level, index (level)}
    <div class="absolute inset-0" style={layerStyle(level, index)}></div>
  {/each}
</div>
