<script lang="ts">
  import type { ClassValue, HTMLInputAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";

  type Props = Omit<HTMLInputAttributes, "size"> & {
    element?: HTMLInputElement;
    label?: string;
    hint?: string;
    inputClass?: ClassValue;
    size?: keyof typeof sizes;
    error?: string;
  };

  const id = $props.id();
  let {
    class: className,
    inputClass,
    element = $bindable(),
    label,
    hint,
    value = $bindable(),
    size = "md",
    error,
    ...restProps
  }: Props = $props();

  const sizes = {
    sm: "px-3 h-10",
    md: "px-3.5 h-11",
  };
</script>

<div class={["flex flex-col gap-1.5", className]}>
  {#if nonNullish(label)}
    <label
      for={id}
      class="text-gray-light-700 dark:text-gray-dark-50 text-sm font-medium"
    >
      {label}
    </label>
  {/if}
  <div class="relative flex">
    <input
      {id}
      bind:this={element}
      bind:value
      {...restProps}
      class={[
        // Default base/light/dark
        "text-md flex-1 rounded-lg border border-none p-0 opacity-100 ring outline-none ring-inset",
        "placeholder:text-gray-light-400 text-gray-light-900 bg-white not-dark:shadow-xs",
        nonNullish(error) ? "ring-error-300" : "ring-gray-light-300",
        "dark:placeholder:text-gray-dark-100 dark:bg-gray-dark-950 dark:text-gray-dark-25",
        nonNullish(error) ? "dark:ring-error-500" : "dark:ring-gray-light-600",
        // Focus base/light/dark
        "focus:ring-2",
        nonNullish(error)
          ? "focus:ring-error-300"
          : "focus:ring-gray-light-950",
        nonNullish(error)
          ? "dark:focus:ring-error-400"
          : "dark:focus:ring-gray-dark-25",
        // Disabled light/dark
        "disabled:!ring-gray-light-200 disabled:bg-gray-light-50 disabled:text-gray-light-400 disabled:placeholder:text-gray-light-400/80",
        "dark:disabled:!ring-gray-light-800 dark:disabled:bg-gray-dark-900 disabled:text-gray-dark-100 dark:disabled:placeholder:text-gray-dark-200",
        sizes[size],
        inputClass,
      ]}
    />
  </div>
  {#if nonNullish(error) || nonNullish(hint)}
    <div
      class={[
        "text-sm",
        nonNullish(error)
          ? "text-error-600 dark:text-error-400"
          : "text-gray-light-600 dark:text-gray-dark-100",
      ]}
    >
      {error ?? hint}
    </div>
  {/if}
</div>
