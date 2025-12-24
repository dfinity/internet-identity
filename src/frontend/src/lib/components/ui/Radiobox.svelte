<script lang="ts">
  import type { HTMLInputAttributes } from "svelte/elements";
  import { DotIcon } from "@lucide/svelte";
  import { nonNullish } from "@dfinity/utils";

  type Size = "sm" | "md";

  type Props = Omit<HTMLInputAttributes, "type" | "size"> & {
    size?: Size;
    label?: string;
    hint?: string;
  };

  let {
    class: className,
    value,
    group = $bindable(),
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
    {value}
    bind:group
    type="radio"
    class="peer absolute z-1 h-1 w-1 opacity-0"
  />
  <div
    class={[
      "relative flex items-center justify-center rounded-full border",
      "border-border-primary text-fg-primary-inversed bg-bg-primary",
      "hover:bg-bg-primary_hover",
      "peer-checked:bg-bg-brand-solid peer-checked:hover:bg-bg-brand-solid_hover peer-checked:border-none",
      "peer-disabled:border-border-disabled peer-disabled:bg-bg-disabled_subtle peer-disabled:text-fg-disabled_subtle",
      "peer-focus-visible:ring-focus-ring peer-focus-visible:ring-offset-bg-primary outline-none peer-focus-visible:ring-2 peer-focus-visible:ring-offset-2",
      {
        sm: "size-4",
        md: "size-5",
      }[size],
      (nonNullish(label) || nonNullish(hint)) && "mt-0.5",
    ]}
  >
    {#if value === group}
      <DotIcon
        size={{ sm: "0.75rem", md: "1rem" }[size]}
        strokeWidth="0.5rem"
        class="absolute top-1/2 left-1/2 -translate-1/2"
      />
    {/if}
  </div>
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
