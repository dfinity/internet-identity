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
  class={`card flex min-w-80 flex-col gap-2 sm:min-w-80 md:min-w-md lg:min-w-lg ${classes}`}
  {...rest}
>
  {@render children()}
</div>
