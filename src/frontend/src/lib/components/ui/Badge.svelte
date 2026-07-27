<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Color = "surface" | "success";
  type Size = "sm" | "md" | "lg";

  type Props = HTMLAttributes<HTMLSpanElement> & {
    color?: Color;
    size?: Size;
    dot?: boolean;
  };

  const {
    children,
    class: className,
    color = "surface",
    size = "md",
    dot = false,
    ...props
  }: Props = $props();
</script>

<span
  {...props}
  class={[
    "rounded-full border",
    dot ? "inline-flex items-center gap-1" : "inline-block",
    {
      surface:
        "border-border-tertiary bg-bg-primary text-text-secondary font-medium",
      success:
        "border-bg-success-secondary bg-bg-success-primary text-fg-success-primary font-semibold",
    }[color],
    dot
      ? {
          sm: "py-0.5 ps-1.5 pe-2 text-xs",
          md: "py-0.5 ps-1.5 pe-2 text-sm",
          lg: "py-1 ps-2 pe-2.5 text-sm",
        }[size]
      : {
          sm: "px-2 py-0.5 text-xs",
          md: "px-2 py-0.5 text-sm",
          lg: "px-2.5 py-1 text-sm",
        }[size],
    className,
  ]}
>
  {#if dot}
    <div class="size-1.5 shrink-0 rounded-full bg-current"></div>
  {/if}
  {@render children?.()}
</span>
