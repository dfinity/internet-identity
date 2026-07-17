<script lang="ts">
  import type { ClassValue, HTMLInputAttributes } from "svelte/elements";
  import type { Snippet } from "svelte";

  type Size = "sm" | "md";

  type Props = Omit<HTMLInputAttributes, "size"> & {
    element?: HTMLInputElement;
    label?: string;
    hint?: string | Snippet;
    inputClass?: ClassValue;
    size?: Size;
    error?: string | Snippet;
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
    errorBorder = error !== undefined,
    ...restProps
  }: Props = $props();
</script>

<div class={["flex flex-col gap-1.5", className]}>
  {#if label !== undefined}
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
        "bg-bg-primary text-text-primary placeholder:text-text-placeholder flex-1 rounded-lg border border-none p-0 text-base opacity-100 ring outline-none ring-inset not-dark:shadow-xs",
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
  {#if error !== undefined || hint !== undefined}
    <div
      class={[
        "text-sm",
        error !== undefined ? "text-text-error-primary" : "text-text-tertiary",
      ]}
    >
      {#if error !== undefined}
        {#if typeof error === "string"}
          {error}
        {:else}
          {@render error()}
        {/if}
      {:else if hint !== undefined}
        {#if typeof hint === "string"}
          {hint}
        {:else}
          {@render hint()}
        {/if}
      {/if}
    </div>
  {/if}
</div>
