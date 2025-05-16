<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";
  import { isNullish } from "@dfinity/utils";

  type Props = SVGAttributes<SVGSVGElement> & {
    value?: number;
  };

  const { value, class: className }: Props = $props();

  const strokeWidth = 2;
  let clientWidth = $state(0);
  const circumference = $derived(
    2 * Math.PI * ((clientWidth - strokeWidth) * 0.5),
  );
</script>

<svg
  bind:clientWidth
  viewBox="0 0 {clientWidth} {clientWidth}"
  class={[
    "size-5 -rotate-90 rounded-full",
    className,
    isNullish(value) && "progress_rotate",
  ]}
  style="--circumference: {circumference}px"
>
  <circle
    class={["fill-none stroke-current/20"]}
    stroke-width={strokeWidth}
    r={(clientWidth - strokeWidth) / 2}
    cx={clientWidth / 2}
    cy={clientWidth / 2}
  />
  <circle
    class={["fill-none stroke-current", isNullish(value) && "progress_pulsate"]}
    stroke-linecap="round"
    stroke-width={strokeWidth}
    r={(clientWidth - strokeWidth) / 2}
    cx={clientWidth / 2}
    cy={clientWidth / 2}
    stroke-dasharray={circumference}
    stroke-dashoffset={circumference - (circumference / 100) * (value ?? 0)}
  />
</svg>

<style>
  /*noinspection CssUnusedSymbol*/
  .progress_rotate {
    transform-origin: center;
    animation: rotate 2s linear infinite;
  }

  /*noinspection CssUnusedSymbol*/
  .progress_pulsate {
    transform-origin: center;
    stroke-dashoffset: 0;
    animation: pulsate 2s ease-in-out infinite;
  }

  @keyframes rotate {
    from {
      transform: rotate(0deg);
    }
    to {
      transform: rotate(360deg);
    }
  }

  @keyframes pulsate {
    0% {
      stroke-dashoffset: var(--circumference);
    }
    50% {
      stroke-dashoffset: calc(var(--circumference) * 1 / 4);
      transform: rotate(45deg);
    }
    100% {
      stroke-dashoffset: var(--circumference);
      transform: rotate(360deg);
    }
  }
</style>
