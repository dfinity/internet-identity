<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  const { class: className, ...props }: SvelteHTMLElements["div"] = $props();
</script>

<div
  {...props}
  class={["bg-surface-light-200 dark:bg-surface-dark-700 h-px", className]}
></div>
