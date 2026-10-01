<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  const {
    children,
    class: className,
    ...props
  }: SvelteHTMLElements["div"] = $props();
</script>

<!-- A control sitting on the surface: the address field, a button, a chip. -->
<div
  {...props}
  class={[
    "bg-surface-light-200 dark:bg-surface-dark-700 flex items-center",
    className,
  ]}
>
  {@render children?.()}
</div>
