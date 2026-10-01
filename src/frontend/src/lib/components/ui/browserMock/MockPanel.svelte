<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  const {
    children,
    class: className,
    ...props
  }: SvelteHTMLElements["div"] = $props();
</script>

<!-- A browser's own panel: a settings pane, a dropdown, a site-info popover. -->
<div
  {...props}
  class={[
    "bg-surface-light-50 dark:bg-surface-dark-900 pointer-events-none flex flex-col rounded-xl p-2 text-[10px]",
    className,
  ]}
>
  {@render children?.()}
</div>
