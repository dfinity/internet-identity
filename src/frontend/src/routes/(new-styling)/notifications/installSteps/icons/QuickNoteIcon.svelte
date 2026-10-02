<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  // Notes' quick-note glyph. Not a Lucide shape: kept as the design drew it.
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
  <path d="M7 15c1.5-4 3-4 4 0s2.5 4 4 0" />
</svg>
