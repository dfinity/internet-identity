<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  // Settings' wallpaper row. No Lucide shape matches it, so it is the design's own.
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
  <circle cx="12" cy="12" r="10" />
  <path
    d="M12 16.5A4.5 4.5 0 1 1 7.5 12 4.5 4.5 0 1 0 12 7.5a4.5 4.5 0 1 1 4.5 4.5 4.5 4.5 0 1 0-4.5 4.5"
  />
</svg>
