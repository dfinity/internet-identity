<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  const {
    children,
    class: className,
    ...props
  }: SvelteHTMLElements["div"] = $props();
</script>

<!-- The browser's own surface: a toolbar, a site panel, a settings pane. Padding is
     the caller's, since each of those has its own. -->
<div
  {...props}
  class={[
    "bg-surface-light-50 dark:bg-surface-dark-900 text-text-primary pointer-events-none rounded-xl",
    className,
  ]}
>
  {@render children?.()}
</div>
