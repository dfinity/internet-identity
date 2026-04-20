<script lang="ts">
  import type { SVGAttributes } from "svelte/elements";

  const { class: className, ...props }: SVGAttributes<SVGSVGElement> = $props();
</script>

<svg viewBox="0 0 20 20" {...props} class={["size-5", className]}>
  <path
    d="M10 1a4.5 4.5 0 0 0-4.5 4.5c0 1.56.8 2.93 2 3.74V18a1 1 0 0 0 1.7.7L11 16.92l1.3 1.3a1 1 0 0 0 1.42 0l1.3-1.3a1 1 0 0 0 0-1.42l-1.3-1.3 1-1a1 1 0 0 0 0-1.42L13.5 10.6V9.24a4.5 4.5 0 0 0 1-7.74A4.5 4.5 0 0 0 10 1Zm0 2.5a2 2 0 1 1 0 4 2 2 0 0 1 0-4Z"
    class="fill-current"
  />
</svg>
