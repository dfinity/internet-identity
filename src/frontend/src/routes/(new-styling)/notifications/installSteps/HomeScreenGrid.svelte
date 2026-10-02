<script lang="ts">
  // The Home Screen, with the installed app ringed. Both routes end here.
  import { t } from "$lib/stores/locale.store";
  import Logo from "$lib/components/ui/Logo.svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";

  /** The apps already there, drawn as blanks: six is what the design shows. */
  const others = Array.from({ length: 6 });
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
          <Logo width="24" height="12" class="shrink-0" />
        </div>
      </ToolbarHighlight>
      <span class="text-[9px] whitespace-nowrap">{$t`Internet Identity`}</span>
    </div>
  </div>
</Surface>
