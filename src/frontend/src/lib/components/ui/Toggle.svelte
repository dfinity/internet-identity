<script lang="ts">
  import type { HTMLInputAttributes } from "svelte/elements";
  import { nonNullish } from "@dfinity/utils";

  type Size = "sm" | "md";

  type Props = Omit<HTMLInputAttributes, "type" | "size"> & {
    size?: Size;
    label?: string;
    hint?: string;
  };

  let {
    checked = $bindable(),
    class: className,
    size = "md",
    label,
    hint,
    ...props
  }: Props = $props();
</script>

<label
  class={[
    "flex w-max max-w-full flex-row items-start",
    { sm: "gap-2", md: "gap-3" }[size],
    className,
  ]}
>
  <input
    {...props}
    bind:checked
    type="checkbox"
    class="peer absolute z-1 h-1 w-1 opacity-0"
    role="switch"
  />
  <div
    class={[
      "shrink-0 cursor-pointer rounded-full p-0.5 transition-colors duration-200",
      "bg-bg-tertiary dark:bg-bg-quaternary/60",
      "peer-checked:bg-bg-brand-solid dark:peer-checked:bg-fg-tertiary",
      "after:block after:rounded-full after:bg-white after:shadow-xs after:transition-transform after:duration-200",
      "peer-checked:after:translate-x-[100%]",
      "peer-disabled:bg-bg-disabled peer-disabled:after:bg-surface-light-50  dark:peer-disabled:after:bg-surface-dark-400",
      "peer-focus-visible:ring-focus-ring peer-focus-visible:ring-offset-bg-primary outline-none peer-focus-visible:ring-2 peer-focus-visible:ring-offset-2",
      {
        sm: "h-5 w-9 after:size-4",
        md: "h-6 w-11 after:size-5",
      }[size],
    ]}
  ></div>
  {#if nonNullish(label) || nonNullish(hint)}
    <div class="flex flex-col">
      {#if nonNullish(label)}
        <p
          class={[
            "text-text-secondary font-medium select-none",
            { sm: "text-sm", md: "text-base" }[size],
          ]}
        >
          {label}
        </p>
      {/if}
      {#if nonNullish(hint)}
        <p
          class={[
            "text-text-tertiary select-none",
            { sm: "text-sm", md: "text-base" }[size],
          ]}
        >
          {hint}
        </p>
      {/if}
    </div>
  {/if}
</label>
