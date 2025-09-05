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

<div
  class="flex cursor-pointer flex-col gap-1 rounded-2xl pb-5 md:bg-transparent"
  class:bg-bg-secondary={open}
  class:sm:bg-bg-secondary={open}
  role="button"
  tabindex="0"
  aria-expanded={open}
  aria-controls="accordion-content"
  onclick={() => (open = !open)}
  onkeydown={(e) => (e.key === "Enter" || e.key === " ") && (open = !open)}
>
  <div
    class="flex w-full items-center justify-between gap-2 px-5 pt-5 text-left font-medium"
  >
    <span class="text-text-primary flex-1 text-base md:text-xl">{header}</span>
    {#if open}
      <MinusCircle class="text-text-placeholder min-size-4 md:min-size-5" />
    {:else}
      <PlusCircle class="text-text-placeholder min-size-4 md:min-size-5" />
    {/if}
  </div>

  <div
    id="accordion-content"
    class="overflow-hidden px-5 transition-[max-height] duration-300 ease-in-out"
    style:max-height={open ? `${contentRef?.scrollHeight}px` : "0px"}
    aria-hidden={!open}
  >
    {#if nonNullish(children)}
      <div bind:this={contentRef}>
        {@render children()}
      </div>
    {/if}
  </div>
</div>
