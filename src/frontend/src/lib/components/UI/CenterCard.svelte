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
  class={`card sm:min-w-s flex flex-col gap-2 md:min-w-md ${classes}`}
  {...rest}
>
  {@render children()}
</div>
