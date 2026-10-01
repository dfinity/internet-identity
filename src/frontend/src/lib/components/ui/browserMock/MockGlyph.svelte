<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";

  const {
    children,
    class: className,
    ...props
  }: SvelteHTMLElements["div"] = $props();
</script>

<!-- The round well a browser gives the control a step points at, which is what the
     highlight rings are drawn around. -->
<div
  {...props}
  class={[
    "bg-surface-light-300 dark:bg-surface-dark-600 flex size-7 items-center justify-center rounded-full",
    className,
  ]}
>
  {@render children?.()}
</div>
