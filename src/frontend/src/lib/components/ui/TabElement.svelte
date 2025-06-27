<script lang="ts">
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import { type Snippet } from "svelte";

  let {
    children,
    href,
    class: classes,
  }: { children: Snippet; href?: string; class?: string } = $props();
</script>

<ButtonOrAnchor
  class={`${classes} text-text-primary flex items-center gap-2 rounded-sm`}
  {href}
>
  {@render children?.()}
</ButtonOrAnchor>
