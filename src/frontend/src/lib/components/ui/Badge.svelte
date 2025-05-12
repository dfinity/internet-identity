<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Props = HTMLAttributes<HTMLDivElement> & {
    color?: keyof typeof colors;
    size?: keyof typeof sizes;
  };

  const {
    children,
    class: className,
    color = "gray",
    size = "md",
    ...props
  }: Props = $props();

  const colors = {
    gray: "border-gray-light-200 text-gray-light-700 bg-white dark:border-gray-light-700 dark:text-gray-dark-50 dark:bg-gray-dark-950",
    // Add more color variants as needed here
  };
  const sizes = {
    sm: "px-2 py-0.5 text-xs",
    md: "px-2 py-0.5 text-sm",
    lg: "px-2.5 py-1 text-sm",
  };
</script>

<div
  {...props}
  class={[
    "rounded-full border font-medium",
    colors[color],
    sizes[size],
    className,
  ]}
>
  {@render children?.()}
</div>
