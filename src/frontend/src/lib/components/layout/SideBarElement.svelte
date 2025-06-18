<script lang="ts">
  import ButtonOrAnchor from "../utils/ButtonOrAnchor.svelte";

  let { children, href, class: classes } = $props();
</script>

<ButtonOrAnchor class={`${classes} `} {href}>
  {@render children?.()}
</ButtonOrAnchor>
