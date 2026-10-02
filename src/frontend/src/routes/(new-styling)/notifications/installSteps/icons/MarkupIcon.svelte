<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  // Safari's markup tool. Not a Lucide glyph: kept as the design drew it.
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
  <path d="M11.83 5.6 15.7 13.75H7.96Z" />
  <path d="M8.35 13.6 7.1 21.6" />
  <path d="M15.3 13.6l1.25 8" />
</svg>
