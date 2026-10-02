<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  // The design's own bookmark, kept because Lucide has since redrawn its own and the
  // two are no longer the same shape. Named by `icon-match.py`, not by eye.
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
