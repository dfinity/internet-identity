<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Props = HTMLAttributes<HTMLDivElement> & {
    size?: keyof typeof sizes;
  };

  const { children, class: className, size = "md", ...props }: Props = $props();

  const sizes = {
    sm: "size-8",
    md: "size-10",
    lg: "size-12",
    xl: "size-14",
  };
</script>

<div
  {...props}
  class={[
    "flex shrink-0 items-center justify-center rounded-lg border",
    "border-gray-light-300 text-gray-light-700 bg-white",
    "dark:border-gray-light-600 dark:bg-gray-dark-950 dark:text-gray-light-300",
    sizes[size],
    className,
  ]}
>
  {@render children?.()}
</div>
