<script lang="ts">
  // TODO: Deprecate this component, use classes directly on <button> and <a>
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
    "btn",
    {
      primary: "btn-primary",
      secondary: "btn-secondary",
      tertiary: "btn-tertiary",
    }[variant],
    {
      sm: "btn-sm",
      md: "btn-md",
      lg: "btn-lg",
      xl: "btn-xl",
    }[size],
    danger && "btn-danger",
    iconOnly && "btn-icon",
    className,
  ]}
>
  {@render children?.()}
</ButtonOrAnchor>
