<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  // The design's own bookmark, which is not the shape Lucide's `bookmark` draws.
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
  <path d="m19 21-7-4-7 4V5a2 2 0 0 1 2-2h10a2 2 0 0 1 2 2v16z" />
</svg>
