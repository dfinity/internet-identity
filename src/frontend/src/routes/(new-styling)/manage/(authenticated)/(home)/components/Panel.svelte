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
    "flex flex-col p-4",
    "bg-bg-primary border-border-secondary rounded-2xl border",
    "not-dark:shadow-sm",
    className,
  ]}
>
  {@render children?.()}
</div>
