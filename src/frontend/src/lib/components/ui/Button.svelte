<script lang="ts">
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import type {
    HTMLAnchorAttributes,
    HTMLButtonAttributes,
  } from "svelte/elements";

  type Variant = "primary" | "secondary" | "tertiary";
  type Size = "sm" | "md" | "lg" | "xl";

  type Props = HTMLButtonAttributes &
    HTMLAnchorAttributes & {
      element?: HTMLElement;
      variant?: Variant;
      size?: Size;
      iconOnly?: boolean;
      danger?: boolean;
    };

  let {
    children,
    element = $bindable(),
    class: className,
    variant = "primary",
    size = "md",
    iconOnly,
    danger,
    ...props
  }: Props = $props();
</script>

<ButtonOrAnchor
  bind:element
  {...props}
  class={[
    "box-border flex items-center justify-center justify-self-start rounded-md font-semibold whitespace-nowrap opacity-100 not-disabled:cursor-pointer",
    {
      primary: [
        danger
          ? "bg-bg-error-solid text-white"
          : "bg-bg-brand-solid text-text-primary-inversed",
        danger
          ? "not-disabled:hover:bg-bg-error-solid_hover"
          : "not-disabled:hover:bg-bg-brand-solid_hover",
        "disabled:bg-bg-disabled disabled:text-fg-disabled",
      ],
      secondary: [
        "bg-bg-primary border",
        danger
          ? "border-border-error_subtle text-text-error-primary"
          : "border-border-secondary text-fg-primary",
        danger
          ? "not-disabled:hover:bg-bg-error-primary text-text-error-primary_hover"
          : "not-disabled:hover:bg-bg-primary_hover",
        "disabled:border-border-disabled disabled:text-fg-disabled",
      ],
      tertiary: [
        danger ? "text-text-error-primary" : "text-fg-primary",
        danger
          ? "not-disabled:hover:bg-bg-error-primary text-text-error-primary_hover"
          : "not-disabled:hover:bg-bg-primary_hover",
        "disabled:text-fg-disabled",
      ],
    }[variant],
    "focus-visible:ring-offset-bg-primary outline-none focus-visible:ring-2 focus-visible:ring-offset-2",
    danger
      ? "focus-visible:ring-focus-ring-error"
      : "focus-visible:ring-focus-ring",
    {
      sm: iconOnly ? "size-9" : "h-9 gap-1.5 px-3 text-sm",
      md: iconOnly ? "size-10" : "h-10 gap-1.5 px-3.5 text-sm",
      lg: iconOnly ? "size-11" : "text-md h-11 gap-2.5 px-4",
      xl: iconOnly ? "size-12" : "text-md h-12 gap-2.5 px-4.5",
    }[size],
    className,
  ]}
>
  {@render children?.()}
</ButtonOrAnchor>
