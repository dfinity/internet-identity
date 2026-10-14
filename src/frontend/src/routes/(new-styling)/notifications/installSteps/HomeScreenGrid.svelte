<script lang="ts">
  import type { Snippet } from "svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";

  const {
    label,
    blanks,
    icon,
  }: {
    /** The ringed app's name, under its tile. */
    label: string;
    /** How many apps sit before it, drawn as blanks: the design shows six before the
     *  installed app and four before Settings. */
    blanks: number;
    icon: Snippet;
  } = $props();

  const others = $derived(Array.from({ length: blanks }));
</script>

<Surface aria-hidden="true" class="text-text-primary px-3.5 pt-4 pb-3.5">
  <div class="grid grid-cols-4 justify-items-center gap-y-3.5">
    {#each others as _, index (index)}
      <div class="flex flex-col items-center gap-1.5 opacity-45">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 h-10 w-10 rounded-[10px]"
        ></div>
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 h-[5px] w-7 rounded-full"
        ></div>
      </div>
    {/each}
    <div class="flex flex-col items-center gap-1.5">
      <ToolbarHighlight shape="app">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 text-text-primary flex h-10 w-10 items-center justify-center rounded-[10px]"
        >
          {@render icon()}
        </div>
      </ToolbarHighlight>
      <span class="text-[9px] whitespace-nowrap">{label}</span>
    </div>
  </div>
</Surface>
