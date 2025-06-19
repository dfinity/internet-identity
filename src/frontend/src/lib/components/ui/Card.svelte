<script lang="ts">
  import { type Snippet } from "svelte";

  const { children, class: classes }: { children: Snippet; class?: string } =
    $props();
</script>

<div
  class={[
    "bg-bg-primary border-border-secondary rounded-2xl border shadow-sm",
    classes,
  ]}
>
  {@render children?.()}
</div>
