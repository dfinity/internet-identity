<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  // Safari's markup tool, as the design draws it: each part carries its own fill and
  // stroke width, so the nib is solid and the ring is thinner than the strokes. Not a
  // Lucide shape.
  const props: SVGAttributes<SVGSVGElement> = $props();
</script>

<svg viewBox="0 0 24 24" {...props}>
  <circle
    cx="12"
    cy="12"
    r="11.1"
    fill="none"
    stroke="currentColor"
    stroke-width="1.75"
  />
  <path d="M11.83 5.6 15.7 13.75H7.96Z" fill="currentColor" />
  <path
    d="M8.35 13.6 7.1 21.6M15.3 13.6l1.25 8"
    fill="none"
    stroke="currentColor"
    stroke-width="1.45"
  />
</svg>
