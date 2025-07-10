<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  type Props = SVGAttributes<SVGSVGElement>;

  const { class: className, ...props }: Props = $props();
</script>

<svg
  viewBox="0 0 72 88"
  {...props}
  class={["fill-none stroke-current", className]}
>
  <path
    d="M69.5 44.1c0 20.55-22.42 35.5-30.57 40.26-.93.54-1.4.8-2.05.95-.5.1-1.25.1-1.76 0a6.76 6.76 0 0 1-2.04-.95C24.92 79.6 2.5 64.65 2.5 44.09V24.07c0-3.35 0-5.03.55-6.46a8.39 8.39 0 0 1 2.29-3.3c1.15-1.03 2.72-1.61 5.86-2.79L33.65 3.1c.87-.33 1.3-.49 1.75-.55.4-.06.8-.06 1.2 0 .45.06.88.22 1.76.55l22.45 8.42c3.13 1.18 4.7 1.76 5.86 2.78 1.02.9 1.8 2.03 2.28 3.3.55 1.44.55 3.12.55 6.47z"
    vector-effect="non-scaling-stroke"
  />
  <path
    d="m23.44 42 8.37 8.37 18.85-18.84"
    stroke-linecap="round"
    vector-effect="non-scaling-stroke"
  />
</svg>
