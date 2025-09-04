<script lang="ts">
  import type { HTMLAttributes } from "svelte/elements";
  import { PlusCircle, MinusCircle } from "@lucide/svelte";
  import { nonNullish } from "@dfinity/utils";

  type Props = HTMLAttributes<HTMLDivElement> & {
    header: string;
  };

  const { children, header }: Props = $props();
  let open = $state(false);
  let contentRef = $state<HTMLDivElement>();
</script>

<button
  class="flex flex-col gap-1 rounded-2xl p-5 text-left md:bg-transparent"
  class:bg-bg-secondary={open}
  class:sm:bg-bg-secondary={open}
  onclick={() => (open = !open)}
>
  <div
    class="flex w-full items-center justify-between gap-2 font-medium focus:outline-none"
    aria-expanded={open}
  >
    <span class="text-text-primary flex-1 text-base md:text-xl">{header}</span>
    {#if open}
      <MinusCircle class="text-text-placeholder min-size-4 md:min-size-5" />
    {:else}
      <PlusCircle class="text-text-placeholder min-size-4 md:min-size-5" />
    {/if}
  </div>

  <div
    class="overflow-hidden transition-[max-height] duration-300 ease-in-out"
    style:max-height={open ? `${contentRef?.scrollHeight}px` : "0px"}
    aria-hidden={!open}
  >
    {#if nonNullish(children)}
      <div bind:this={contentRef}>
        {@render children()}
      </div>
    {/if}
  </div>
</button>
