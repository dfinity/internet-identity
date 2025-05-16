<script lang="ts">
  import type { HTMLInputAttributes } from "svelte/elements";
  import { DotIcon } from "@lucide/svelte";

  type Size = "sm" | "md";

  type Props = Omit<HTMLInputAttributes, "type" | "size"> & {
    size?: Size;
  };

  let {
    class: className,
    value,
    group = $bindable(),
    size = "md",
    ...props
  }: Props = $props();
</script>

<label
  class={[
    "relative flex items-center justify-center rounded-sm border",
    "border-border-primary text-fg-primary-inversed bg-bg-primary",
    "hover:bg-bg-primary_hover",
    "has-checked:bg-bg-brand-solid has-checked:hover:bg-bg-brand-solid_hover has-checked:border-none",
    "has-disabled:border-border-disabled has-disabled:bg-bg-disabled_subtle has-disabled:text-fg-disabled_subtle",
    "has-focus-visible:ring-focus-ring has-focus-visible:ring-offset-bg-primary outline-none has-focus-visible:ring-2 has-focus-visible:ring-offset-2",
    {
      sm: "size-4",
      md: "size-5",
    }[size],
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
