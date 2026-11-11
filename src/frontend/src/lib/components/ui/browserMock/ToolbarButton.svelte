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
    "bg-surface-light-200 dark:bg-surface-dark-700 text-text-primary flex h-9 min-w-9 items-center justify-center rounded-full",
    className,
  ]}
>
  {@render children?.()}
</div>
