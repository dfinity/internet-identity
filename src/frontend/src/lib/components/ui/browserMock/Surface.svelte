<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  const {
    children,
    class: className,
    ...props
  }: SvelteHTMLElements["div"] = $props();
</script>

<div
  {...props}
  class={[
    "bg-surface-light-50 dark:bg-surface-dark-900 pointer-events-none rounded-xl",
    className,
  ]}
>
  {@render children?.()}
</div>
