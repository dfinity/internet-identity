<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Color = "surface";
  type Size = "sm" | "md" | "lg";

  type Props = HTMLAttributes<HTMLDivElement> & {
    color?: Color;
    size?: Size;
  };

  const {
    children,
    class: className,
    color = "surface",
    size = "md",
    ...props
  }: Props = $props();
</script>

<div
  {...props}
  class={[
    "rounded-full border font-medium",
    {
      surface: "border-border-tertiary text-text-secondary bg-bg-primary",
    }[color],
    {
      sm: "px-2 py-0.5 text-xs",
      md: "px-2 py-0.5 text-sm",
      lg: "px-2.5 py-1 text-sm",
    }[size],
    className,
  ]}
>
  {@render children?.()}
</div>
