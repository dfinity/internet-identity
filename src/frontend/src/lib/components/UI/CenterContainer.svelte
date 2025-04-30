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

<div
  class={`flex min-h-screen w-screen items-center justify-center ${classes}`}
  {...rest}
>
  {@render children()}
</div>
