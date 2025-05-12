<script lang="ts">
  import type {
    ChangeEventHandler,
    HTMLInputAttributes,
  } from "svelte/elements";
  import { DotIcon } from "@lucide/svelte";

  type Props = Omit<HTMLInputAttributes, "type" | "size"> & {
    size?: keyof typeof sizes;
  };

  let {
    class: className,
    value,
    group = $bindable(),
    size = "md",
    ...props
  }: Props = $props();

  const sizes = {
    sm: "size-4",
    md: "size-5",
  };

  const handleChange: ChangeEventHandler<HTMLInputElement> = (event) => {
    console.log("event", event);
    group = event.currentTarget.getAttribute("value");
  };
</script>

<label
  class={[
    "relative flex items-center justify-center rounded-full border",
    "border-gray-light-400 text-gray-dark-25 bg-white",
    "dark:border-gray-light-500 dark:text-gray-dark-950 dark:bg-gray-dark-950",
    // Checked base/light/dark
    value === group && "border-none",
    value === group && "!bg-gray-light-900",
    value === group && "dark:!bg-gray-dark-25",
    // Hover light/dark
    "hover:bg-gray-light-200",
    "dark:hover:bg-gray-light-700/50",
    // Disabled light/dark
    "has-disabled:border-gray-light-300 has-disabled:bg-gray-light-50",
    "dark:has-disabled:border-gray-dark-700 dark:has-disabled:bg-gray-dark-900",
    // Checked disabled light/dark
    value === group &&
      "has-disabled:!bg-gray-light-300 has-disabled:!text-gray-light-400/50",
    value === group &&
      "dark:has-disabled:!bg-gray-dark-700 dark:has-disabled:!text-gray-light-700",
    // Focus base/light/dark
    "outline-none has-focus-visible:ring-2 has-focus-visible:ring-offset-2",
    "has-focus-visible:ring-gray-light-500 has-focus-visible:ring-offset-white",
    "dark:has-focus-visible::ring-gray-dark-300 dark:has-focus-visible:ring-offset-gray-dark-950",
    sizes[size],
    className,
  ]}
>
  <input {...props} {value} bind:group type="radio" class={"sr-only"} />
  {#if value === group}
    <DotIcon
      size={{ sm: "0.75rem", md: "1rem" }[size]}
      strokeWidth="0.5rem"
      class="absolute top-1/2 left-1/2 -translate-1/2"
    />
  {/if}
</label>
