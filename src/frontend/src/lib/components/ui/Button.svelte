<script lang="ts">
  import ButtonOrAnchor from "$lib/components/utils/ButtonOrAnchor.svelte";
  import type {
    HTMLAnchorAttributes,
    HTMLButtonAttributes,
  } from "svelte/elements";

  type Props = HTMLButtonAttributes &
    HTMLAnchorAttributes & {
      element?: HTMLElement;
      variant?: keyof typeof variants;
      size?: keyof typeof sizes;
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

  const focus = [
    // Focus base/light/dark
    "focus-visible:ring-2 focus-visible:ring-offset-2 outline-none",
    "focus-visible:ring-gray-light-500 focus-visible:ring-offset-white",
    "dark:focus-visible:ring-gray-dark-300 dark:focus-visible:ring-offset-gray-dark-950",
  ];
  const variants = {
    primary: [
      // Default light/dark
      "bg-gray-light-900 text-gray-light-50",
      "dark:bg-gray-dark-25 dark:text-gray-dark-900",
      // Hover light/dark
      "not-disabled:hover:bg-gray-dark-700",
      "dark:not-disabled:hover:bg-gray-light-200",
      // Disabled light/dark
      "disabled:bg-gray-light-200 disabled:text-gray-light-400",
      "disabled:bg-gray-dark-700 dark:disabled:text-gray-light-500",
    ],
    secondary: [
      "border",
      "bg-white border-gray-light-300 text-gray-light-900",
      "dark:bg-gray-dark-950 dark:border-gray-light-600 dark:text-gray-dark-25",
      "not-disabled:hover:bg-gray-light-200",
      "dark:not-disabled:hover:bg-gray-light-900",
      "disabled:text-gray-light-400 disabled:border-gray-light-200",
      "dark:disabled:text-gray-light-500 dark:disabled:border-gray-light-800",
    ],
    tertiary: [
      "text-gray-light-900",
      "dark:text-gray-dark-25",
      "not-disabled:hover:bg-gray-light-200",
      "dark:not-disabled:hover:bg-gray-light-700/50",
      "disabled:text-gray-light-400",
      "dark:disabled:text-gray-light-500",
    ],
  };
  const sizes = {
    sm: iconOnly ? "size-9" : "px-3 text-sm gap-1.5 h-9",
    md: iconOnly ? "size-10" : "px-3.5 text-sm gap-1.5 h-10",
    lg: iconOnly ? "size-11" : "px-4 text-md gap-2.5 h-11",
    xl: iconOnly ? "size-12" : "px-4.5 text-md gap-2.5 h-12",
  };
</script>

<ButtonOrAnchor
  bind:element
  {...props}
  class={[
    "box-border flex items-center justify-center justify-self-start rounded-md font-semibold whitespace-nowrap opacity-100",
    variants[variant],
    focus,
    sizes[size],
    className,
  ]}
>
  {@render children?.()}
</ButtonOrAnchor>
