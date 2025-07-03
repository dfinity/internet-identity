<script lang="ts">
  import ButtonOrAnchor from "$lib/components/ButtonOrAnchor.svelte";
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
    };

  let {
    children,
    element = $bindable(),
    class: className,
    variant = "primary",
    size = "md",
    iconOnly,
    ...props
  }: Props = $props();
</script>

<ButtonOrAnchor
  bind:element
  {...props}
  class={[
    "box-border flex items-center justify-center justify-self-start rounded-md font-semibold whitespace-nowrap opacity-100",
    {
      primary: [
        "bg-bg-brand-solid text-text-primary-inversed",
        "not-disabled:hover:bg-bg-brand-solid_hover",
        "disabled:bg-bg-disabled disabled:text-fg-disabled",
      ],
      secondary: [
        "bg-bg-primary border-border-secondary text-fg-primary border",
        "not-disabled:hover:bg-bg-primary_hover",
        "disabled:border-border-disabled disabled:text-fg-disabled",
      ],
      tertiary: [
        "text-fg-primary",
        "not-disabled:hover:bg-bg-primary_hover",
        "disabled:text-fg-disabled",
      ],
    }[variant],
    "focus-visible:ring-focus-ring focus-visible:ring-offset-bg-primary outline-none focus-visible:ring-2 focus-visible:ring-offset-2",
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
