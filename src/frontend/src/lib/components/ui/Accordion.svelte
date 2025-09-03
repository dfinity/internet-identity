<script lang="ts">
  import { PlusCircle, MinusCircle } from "@lucide/svelte";

  export let header: string;
  let open = false;
  let contentRef: HTMLDivElement;
</script>

<div
  class="flex flex-col gap-1 rounded-2xl p-5 md:bg-transparent"
  class:bg-bg-secondary={open}
  class:sm:bg-bg-secondary={open}
>
  <button
    class="flex w-full items-center justify-between gap-2 text-left font-medium focus:outline-none"
    on:click={() => (open = !open)}
    aria-expanded={open}
  >
    <span class="text-text-primary flex-1 text-base md:text-xl">{header}</span>
    {#if open}
      <MinusCircle class="text-text-placeholder min-size-4 md:min-size-5" />
    {:else}
      <PlusCircle class="text-text-placeholder min-size-4 md:min-size-5" />
    {/if}
  </button>

  <div
    class="overflow-hidden transition-[max-height] duration-300 ease-in-out"
    style:max-height={open ? `${contentRef?.scrollHeight}px` : "0px"}
    aria-hidden={!open}
  >
    <div bind:this={contentRef}>
      <slot />
    </div>
  </div>
</div>
