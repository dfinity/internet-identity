<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  type Props = SVGAttributes<SVGSVGElement>;

  const { class: className, ...props }: Props = $props();
</script>

<svg
  viewBox="0 0 163 124"
  {...props}
  class={["fill-none stroke-current", className]}
>
  <path
    d="M59.5 97h89.7c4.5 0 6.7 0 8.4-.9a8 8 0 0 0 3.5-3.5c.9-1.7.9-4 .9-8.4V13.8c0-4.5 0-6.7-.9-8.4a8 8 0 0 0-3.5-3.5c-1.7-.9-4-.9-8.4-.9H28.8c-4.5 0-6.7 0-8.4.9a8 8 0 0 0-3.5 3.5c-.9 1.7-.9 4-.9 8.4v14.7"
    shape-rendering="crispEdges"
    vector-effect="non-scaling-stroke"
  />
  <rect
    width="59"
    height="95"
    x=".5"
    y="28.5"
    rx="7.5"
    shape-rendering="crispEdges"
    vector-effect="non-scaling-stroke"
  />
  <path
    stroke-linecap="round"
    d="M64 123h50M75 28h28m-87 75h28m34-70h22m-81 75h22M28 65h26m6 0h29m0 32v26"
    shape-rendering="crispEdges"
    vector-effect="non-scaling-stroke"
  />
  <rect
    width="20"
    height="3"
    x="20"
    y="33"
    rx="1.5"
    shape-rendering="crispEdges"
    vector-effect="non-scaling-stroke"
  />
  <rect
    width="14"
    height="3"
    x="83"
    y="6"
    rx="1.5"
    shape-rendering="crispEdges"
    vector-effect="non-scaling-stroke"
  />
  <path
    stroke-linecap="round"
    stroke-linejoin="round"
    d="m33 60-6 5 6 5m51-10 6 5-6 5"
    shape-rendering="crispEdges"
    vector-effect="non-scaling-stroke"
  />
</svg>
