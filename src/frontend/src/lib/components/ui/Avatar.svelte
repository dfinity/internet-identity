<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Props = HTMLAttributes<HTMLDivElement> & { size?: keyof typeof sizes };

  const { children, class: className, size = "md", ...props }: Props = $props();

  const sizes = {
    sm: "size-8 text-sm",
    md: "size-10 text-sm",
    lg: "size-12 text-md",
    xl: "size-14 text-md",
  };
</script>

<div
  {...props}
  class={[
    "flex shrink-0 items-center justify-center rounded-full border font-semibold",
    "border-gray-light-300 text-gray-light-400 bg-gray-light-200",
    "dark:border-gray-light-600 dark:bg-gray-dark-800 dark:text-gray-light-500",
    sizes[size],
    className,
  ]}
>
  {@render children?.()}
</div>
