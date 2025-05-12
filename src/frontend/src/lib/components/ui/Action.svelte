<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Props = HTMLAttributes<HTMLDivElement>;

  const { children, class: className, ...props }: Props = $props();
</script>

<div
  {...props}
  class={[
    "flex size-8 items-center justify-center rounded-full border text-sm font-semibold",
    "border-gray-light-300 text-gray-light-400 bg-gray-light-200",
    "dark:border-gray-light-600 dark:bg-gray-dark-800 dark:text-gray-light-500",
    className,
  ]}
>
  {@render children?.()}
</div>
