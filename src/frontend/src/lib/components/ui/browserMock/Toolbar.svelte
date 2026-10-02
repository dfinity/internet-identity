<script lang="ts">
  import type { SvelteHTMLElements } from "svelte/elements";
  import Surface from "./Surface.svelte";

  const {
    children,
    class: className,
    ...props
  }: SvelteHTMLElements["div"] = $props();
</script>

<Surface {...props} class={["flex flex-row gap-2 p-2.5", className]}>
  {@render children?.()}
</Surface>
