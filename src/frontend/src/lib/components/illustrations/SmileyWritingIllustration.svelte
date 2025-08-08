<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  type Props = SVGAttributes<SVGSVGElement>;

  const { class: className, ...props }: Props = $props();
</script>

<svg
  {...props}
  class={["h-full fill-none stroke-current", className]}
  fill="none"
  viewBox="0 0 93 93"
>
  <path
    vector-effect="non-scaling-stroke"
    stroke-linecap="round"
    d="M85.14 37.5A42.5 42.5 0 0 0 .51 42.98a42.5 42.5 0 0 0 39.02 42.25"
  />
  <circle cx="29" cy="33" r="4.5" />
  <circle cx="54" cy="33" r="4.5" />
  <path
    vector-effect="non-scaling-stroke"
    stroke-linecap="round"
    d="M57.8 51.43c-2.01 6.09-8.31 10.53-15.77 10.53-7.47 0-13.77-4.44-15.78-10.53"
  />
  <path
    vector-effect="non-scaling-stroke"
    d="M77.6 47.4 45.5 79.5v11h11l32.76-32.76.6-.63a7.8 7.8 0 0 0-.28-10.7 7.8 7.8 0 0 0-10.73-.24c-.43.39-1.24 1.22-1.24 1.22z"
  />
</svg>
