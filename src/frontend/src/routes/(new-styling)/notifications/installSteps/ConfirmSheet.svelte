<script lang="ts">
  // The Add to Home Screen sheet: both routes end at the same confirmation.
  import XIcon from "@lucide/svelte/icons/x";
  import { t } from "$lib/stores/locale.store";
  import ControlCircle from "$lib/components/ui/browserMock/ControlCircle.svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
</script>

<Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
  <div class="flex items-center gap-2 pb-2.5">
    <div class="opacity-45">
      <div class="relative shrink-0">
        <ControlCircle
          class="bg-surface-light-200 dark:bg-surface-dark-700 h-8 w-8"
          ><XIcon class="size-3.5 shrink-0" /></ControlCircle
        >
      </div>
    </div>
    <span class="flex-1 text-center text-[11px] font-semibold whitespace-nowrap"
      >{$t`Add to Home Screen`}</span
    >
    <ToolbarHighlight>
      <div
        class="bg-bg-brand-solid text-text-primary-inversed flex h-6 items-center rounded-full px-3 text-[10px] font-semibold"
      >
        {$t`Add`}
      </div>
    </ToolbarHighlight>
  </div>
  <div class="flex items-center gap-2.5 pt-2 pr-2 pb-2 pl-0.5 opacity-45">
    <span class="flex-1 text-[11px]">{$t`Open as Web App`}</span>
    <!-- Drawn on, because the step is to leave it on. -->
    <div class="bg-bg-brand-solid relative h-4 w-7 rounded-full">
      <div
        class="bg-surface-light-50 dark:bg-surface-dark-900 absolute top-0.5 left-[14px] h-3 w-3 rounded-full"
      ></div>
    </div>
  </div>
</Surface>
