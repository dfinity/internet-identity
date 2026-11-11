<script lang="ts">
  import type { Snippet } from "svelte";

  const {
    address,
    class: className,
    children,
  }: {
    address: string;
    class?: string;
    /** What sits ahead of the address: the controls this browser puts there. */
    children: Snippet;
  } = $props();
</script>

<div
  class={[
    "bg-surface-light-200 dark:bg-surface-dark-700 flex h-9 items-center rounded-full",
    className,
  ]}
>
  {@render children()}
  <span class="ml-5 text-[10px] whitespace-nowrap">{address}</span>
</div>
