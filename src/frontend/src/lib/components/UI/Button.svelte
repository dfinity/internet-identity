<script lang="ts">
  const {
    children,
    class: classes,
    variant = "primary",
    ...rest
  } = $props<{
    children: () => any;
    class?: string;
    variant?: "primary" | "secondary" | "text-only" | "dashed";
    [key: string]: any;
  }>();

  //TODO: @apply is failing and i don't have time to debug
  const variantClasses =
    variant === "primary"
      ? "preset-filled bg-ii-background-primary-dark text-ii-background-primary-light dark:bg-ii-background-primary-light dark:text-ii-background-primary-dark"
      : variant === "secondary"
        ? "border-ii-text-primary-dark dark:border-ii-text-primary-light preset-outlined"
        : variant === "text-only"
          ? ""
          : variant === "dashed"
            ? "border-dashed preset-outlined border-ii-text-primary-dark dark:border-ii-text-primary-light"
            : "";
</script>

<button
  type="button"
  class={`btn box-border ${classes} ${variantClasses}`}
  {...rest}
>
  {@render children()}
</button>
