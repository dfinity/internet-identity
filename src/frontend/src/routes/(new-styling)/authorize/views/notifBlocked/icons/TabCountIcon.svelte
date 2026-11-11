<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  // Not a Lucide glyph: a mobile browser counts its open tabs inside the square,
  // and an empty one reads as a different control.
  const props: SVGAttributes<SVGSVGElement> = $props();
</script>

<svg
  viewBox="0 0 24 24"
  fill="none"
  stroke="currentColor"
  stroke-width="2"
  stroke-linecap="round"
  stroke-linejoin="round"
  {...props}
>
  <rect width="18" height="18" x="3" y="3" rx="2" />
  <text
    x="12"
    y="16.5"
    text-anchor="middle"
    font-size="12"
    font-weight="700"
    fill="currentColor"
    stroke="none"
    font-family="Inter, system-ui, sans-serif">2</text
  >
</svg>
