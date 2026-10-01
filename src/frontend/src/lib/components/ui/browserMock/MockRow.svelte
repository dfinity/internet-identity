<script lang="ts">
  import type { Snippet } from "svelte";

  interface Props {
    /** The row's label, as the browser words it. */
    label?: string;
    /** Leading icon, where the browser shows one. */
    icon?: Snippet;
    /** Trailing control: a toggle, a segmented picker, a chevron. */
    trailing?: Snippet;
    /** Dimmed, for the rows around the one the step is about. */
    muted?: boolean;
    class?: string;
  }

  const {
    label,
    icon,
    trailing,
    muted = false,
    class: className,
  }: Props = $props();
</script>

<div
  class={[
    "text-text-primary flex h-7 flex-row items-center gap-2 rounded-md px-2",
    muted && "opacity-45",
    className,
  ]}
>
  {#if icon !== undefined}
    <span class="flex size-3.5 shrink-0 items-center justify-center">
      {@render icon()}
    </span>
  {/if}
  {#if label !== undefined}
    <span class="truncate">{label}</span>
  {/if}
  {#if trailing !== undefined}
    <span class="ms-auto flex shrink-0 items-center">{@render trailing()}</span>
  {/if}
</div>
