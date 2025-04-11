<script lang="ts">
  const {
    children,
    class: classes,
    ...rest
  } = $props<{
    children: () => any;
    class?: string;
    [key: string]: any;
  }>();
</script>

<div
  class={`flex h-screen w-screen items-center justify-center ${classes}`}
  {...rest}
>
  {@render children()}
</div>
