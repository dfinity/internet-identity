<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Size = "sm" | "md" | "lg" | "xl";
  type Variant = "info" | "success" | "warning" | "error";

  type Props = HTMLAttributes<HTMLDivElement> & {
    element?: HTMLElement;
    size?: Size;
    variant?: Variant;
  };

  let {
    children,
    class: className,
    size = "md",
    variant = "info",
    element = $bindable(),
    ...props
  }: Props = $props();
</script>

<div
  {...props}
  bind:this={element}
  class={[
    "flex shrink-0 items-center justify-center",
    {
      info: "bg-bg-primary text-fg-secondary ring-border-secondary rounded-lg ring",
      success: "bg-bg-success-primary text-fg-success-primary rounded-full",
      warning: "bg-bg-warning-primary text-fg-warning-primary rounded-full",
      error: "bg-bg-error-primary text-fg-error-primary rounded-full",
    }[variant],
    {
      sm: "size-8",
      md: "size-10",
      lg: "size-12",
      xl: "size-14",
    }[size],
    className,
  ]}
>
  {@render children?.()}
</div>
