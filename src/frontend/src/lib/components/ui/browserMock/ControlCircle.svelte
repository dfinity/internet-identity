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
    "bg-surface-light-300 dark:bg-surface-dark-600 flex items-center justify-center rounded-full",
    className,
  ]}
>
  {@render children?.()}
</div>
