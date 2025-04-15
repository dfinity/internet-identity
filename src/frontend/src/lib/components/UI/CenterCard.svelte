<script lang="ts">
  import { type Snippet } from "svelte";

  const {
    children,
    class: classes,
    ...rest
  } = $props<{
    children: Snippet;
    class?: string;
    [key: string]: any;
  }>();
</script>

<div class={`card flex w-100 flex-col gap-2 p-6 ${classes}`} {...rest}>
  {@render children()}
</div>
