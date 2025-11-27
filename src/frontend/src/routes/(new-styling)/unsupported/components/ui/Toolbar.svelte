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
    "bg-surface-light-50 dark:bg-surface-dark-900 pointer-events-none flex flex-row gap-2 rounded-xl p-2.5",
    className,
  ]}
>
  {@render children?.()}
</div>
