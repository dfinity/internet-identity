<script lang="ts">
  import type { ClassValue, HTMLInputAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";

  type Size = "sm" | "md";

  type Props = Omit<HTMLInputAttributes, "size"> & {
    element?: HTMLInputElement;
    label?: string;
    hint?: string;
    inputClass?: ClassValue;
    size?: Size;
    error?: string;
    errorBorder?: boolean;
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
    errorBorder = nonNullish(error),
    ...restProps
  }: Props = $props();
</script>

<div class={["flex flex-col gap-1.5", className]}>
  {#if nonNullish(label)}
    <label for={id} class="text-text-secondary text-sm font-medium">
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
        "text-md bg-bg-primary text-text-primary placeholder:text-text-placeholder flex-1 rounded-lg border border-none p-0 opacity-100 ring outline-none ring-inset not-dark:shadow-xs",
        errorBorder ? "ring-border-error_subtle" : "ring-border-secondary",
        "focus:ring-2",
        errorBorder ? "focus:ring-border-error" : "focus:ring-border-brand",
        "disabled:!ring-border-disabled disabled:bg-bg-disabled_subtle disabled:text-text-disabled disabled:placeholder:text-text-disabled",
        {
          sm: "h-10 px-3",
          md: "h-11 px-3.5",
        }[size],
        inputClass,
      ]}
    />
  </div>
  {#if nonNullish(error) || nonNullish(hint)}
    <div
      class={[
        "text-sm",
        nonNullish(error) ? "text-text-error-primary" : "text-text-tertiary",
      ]}
    >
      {error ?? hint}
    </div>
  {/if}
</div>
