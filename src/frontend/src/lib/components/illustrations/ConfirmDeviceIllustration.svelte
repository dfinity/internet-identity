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
    d="M64 123h50M75 28h28m-87 75h28m34-70h22m-81 75h22m-3-43h16m6 0h16m13 32v26"
    shape-rendering="crispEdges"
    vector-effect="non-scaling-stroke"
  />
  <circle cx="30" cy="65" r="7.5" vector-effect="non-scaling-stroke" />
  <rect
    width="20"
    height="3"
    x="20"
    y="33"
    fill="#1D1D1F"
    rx="1.5"
    shape-rendering="crispEdges"
    vector-effect="non-scaling-stroke"
  />
  <rect
    width="14"
    height="3"
    x="83"
    y="6"
    fill="#1D1D1F"
    rx="1.5"
    shape-rendering="crispEdges"
    vector-effect="non-scaling-stroke"
  />
  <path
    stroke-linecap="round"
    stroke-linejoin="round"
    d="m27 65 2 2 4-4"
    vector-effect="non-scaling-stroke"
  />
  <circle cx="89" cy="65" r="12.5" vector-effect="non-scaling-stroke" />
  <path
    stroke-linecap="round"
    stroke-linejoin="round"
    d="m84 65 3.3 3 6.7-6"
    vector-effect="non-scaling-stroke"
  />
</svg>
