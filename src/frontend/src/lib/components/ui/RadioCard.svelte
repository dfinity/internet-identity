<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import Checkbox from "$lib/components/ui/Checkbox.svelte";

  type Props = HTMLAttributes<HTMLDivElement> & {
    checked?: boolean;
    checkIcon?: boolean;
  };

  const {
    children,
    class: className,
    checked,
    checkIcon,
    ...props
  }: Props = $props();
</script>

<div
  role="radio"
  aria-checked={checked}
  {...props}
  class={[
    // Base/light/dark
    "relative flex items-center justify-start gap-3 rounded-md p-3 text-sm font-semibold ring-1 ring-inset not-dark:shadow-xs",
    "ring-gray-light-300 text-gray-light-900 bg-white",
    "dark:ring-gray-light-600 dark:bg-gray-dark-950 dark:text-gray-dark-25",
    // Checked base/light/dark
    checked && "ring-2",
    checked && "!ring-gray-light-950",
    checked && "dark:!ring-gray-dark-25 ring-2",
    // Hover light/dark
    !checked && "hover:bg-gray-light-100",
    !checked && "dark:hover:bg-gray-dark-900",
    className,
  ]}
>
  <div
    class={[
      // Focus base/light/dark
      "pointer-events-none absolute inset-0 rounded-md in-focus-visible:ring-2 in-focus-visible:ring-offset-2",
      "in-focus-visible:ring-gray-light-500 in-focus-visible:ring-offset-white",
      "dark:in-focus-visible:ring-gray-dark-300 dark:in-focus-visible:ring-offset-gray-dark-950",
    ]}
  ></div>
  {@render children?.()}
  {#if checked && checkIcon}
    <Checkbox
      checked
      size="md"
      class="pointer-events-none mr-1 !rounded-full"
      tabindex={-1}
      aria-hidden="true"
    />
  {/if}
</div>
