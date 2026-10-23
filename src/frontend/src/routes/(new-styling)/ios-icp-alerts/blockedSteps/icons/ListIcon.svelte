<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  // Reminders' list glyph as the design draws it, with circles for bullets rather than
  // the line caps of Lucide's `list`.
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
  <path d="M13 5h8" />
  <path d="M13 12h8" />
  <path d="M13 19h8" />
  <circle cx="5" cy="5" r="1" />
  <circle cx="5" cy="12" r="1" />
  <circle cx="5" cy="19" r="1" />
</svg>
