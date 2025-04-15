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
  class={`bg-ii-background-primary-light dark:bg-ii-background-primary-dark h-screen w-screen ${classes}`}
  {...rest}
>
  {@render children()}
</div>
