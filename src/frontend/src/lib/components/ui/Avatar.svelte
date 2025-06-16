<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";

  type Size = "sm" | "md" | "lg" | "xl";

  type Props = HTMLAttributes<HTMLDivElement> & { size?: Size };

  const { children, class: className, size = "md", ...props }: Props = $props();
</script>

<div
  {...props}
  class={[
    "bg-bg-secondary border-border-secondary text-fg-disabled flex shrink-0 items-center justify-center rounded-full border font-semibold",
    {
      sm: "size-8 text-sm",
      md: "size-10 text-sm",
      lg: "text-md size-12",
      xl: "text-md size-14",
    }[size],
    className,
  ]}
>
  {@render children?.()}
</div>
