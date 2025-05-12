<script lang="ts">
  import type { HTMLInputAttributes } from "svelte/elements";
  import { CheckIcon, MinusIcon } from "@lucide/svelte";

  type Props = Omit<HTMLInputAttributes, "type" | "size"> & {
    size?: keyof typeof sizes;
    indeterminate?: boolean;
  };

  let {
    class: className,
    checked = $bindable(),
    size = "md",
    indeterminate,
    ...props
  }: Props = $props();

  const sizes = {
    sm: "size-4",
    md: "size-5",
  };
</script>

<label
  class={[
    "relative flex items-center justify-center rounded-sm border",
    "border-gray-light-400 text-gray-dark-25 bg-white",
    "dark:border-gray-light-500 dark:text-gray-dark-950 dark:bg-gray-dark-950",
    // Checked base/light/dark
    checked && "border-none",
    checked && "!bg-gray-light-900",
    checked && "dark:!bg-gray-dark-25",
    // Hover light/dark
    "hover:bg-gray-light-200",
    "dark:hover:bg-gray-light-700/50",
    // Disabled light/dark
    "has-disabled:border-gray-light-300 has-disabled:bg-gray-light-50",
    "dark:has-disabled:border-gray-dark-700 dark:has-disabled:bg-gray-dark-900",
    // Checked disabled light/dark
    checked &&
      "has-disabled:!bg-gray-light-300 has-disabled:!text-gray-light-400/50",
    checked &&
      "dark:has-disabled:!bg-gray-dark-700 dark:has-disabled:!text-gray-light-700",
    // Focus base/light/dark
    "outline-none has-focus-visible:ring-2 has-focus-visible:ring-offset-2",
    "has-focus-visible:ring-gray-light-500 has-focus-visible:ring-offset-white",
    "dark:has-focus-visible::ring-gray-dark-300 dark:has-focus-visible:ring-offset-gray-dark-950",
    sizes[size],
    className,
  ]}
>
  <input {...props} bind:checked type="checkbox" class={"sr-only"} />
  {#if checked}
    {#if indeterminate}
      <MinusIcon
        size={{ sm: "0.75rem", md: "1rem" }[size]}
        strokeWidth="0.2rem"
        class="absolute top-1/2 left-1/2 -translate-1/2"
      />
    {:else}
      <CheckIcon
        size={{ sm: "0.75rem", md: "1rem" }[size]}
        strokeWidth="0.2rem"
        class="absolute top-1/2 left-1/2 -translate-1/2"
      />
    {/if}
  {/if}
</label>
