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
    "bg-bg-primary text-fg-secondary border-border-secondary flex shrink-0 items-center justify-center rounded-lg border",
    sizes[size],
    className,
  ]}
>
  {@render children?.()}
</div>
