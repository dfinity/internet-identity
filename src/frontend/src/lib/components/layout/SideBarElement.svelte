<script lang="ts">
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";

  let { children, href, class: classes } = $props();
</script>

<ButtonOrAnchor
  class={`${classes} text-text-secondary hover:bg-bg-active hover:border-border-secondary rounded-sm border border-transparent px-3 py-2`}
  {href}
>
  {@render children?.()}
</ButtonOrAnchor>
