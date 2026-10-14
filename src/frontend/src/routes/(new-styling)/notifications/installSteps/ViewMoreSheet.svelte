<script lang="ts">
  // The share sheet's second row, where both browsers' routes pass through "View More".
  import ChevronDownIcon from "@lucide/svelte/icons/chevron-down";
  import CopyIcon from "@lucide/svelte/icons/copy";
  import GlassesIcon from "@lucide/svelte/icons/glasses";
  import { t } from "$lib/stores/locale.store";
  import ControlCircle from "$lib/components/ui/browserMock/ControlCircle.svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import BookmarkIcon from "./icons/BookmarkIcon.svelte";
</script>

<Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
  <div class="flex items-start gap-2 pt-1 pb-0.5">
    <div class="flex min-w-0 flex-1 flex-col items-center gap-1.5 opacity-45">
      <div class="relative shrink-0">
        <ControlCircle
          class="bg-surface-light-200 dark:bg-surface-dark-700 h-8 w-8"
          ><CopyIcon class="size-3.5 shrink-0" /></ControlCircle
        >
      </div>
      <span class="text-[9px] whitespace-nowrap">{$t`Copy`}</span>
    </div>
    <div class="flex min-w-0 flex-1 flex-col items-center gap-1.5 opacity-45">
      <div class="relative shrink-0">
        <ControlCircle
          class="bg-surface-light-200 dark:bg-surface-dark-700 h-8 w-8"
          ><BookmarkIcon
            width="14"
            height="14"
            class="shrink-0"
          /></ControlCircle
        >
      </div>
      <span class="text-[9px] whitespace-nowrap">{$t`Bookmark`}</span>
    </div>
    <div class="flex min-w-0 flex-1 flex-col items-center gap-1.5 opacity-45">
      <div class="relative shrink-0">
        <ControlCircle
          class="bg-surface-light-200 dark:bg-surface-dark-700 h-8 w-8"
          ><GlassesIcon class="size-3.5 shrink-0" /></ControlCircle
        >
      </div>
      <span class="text-[9px] whitespace-nowrap">{$t`Reading List`}</span>
    </div>
    <div class="flex min-w-0 flex-1 flex-col items-center gap-1.5">
      <ToolbarHighlight class="shrink-0">
        <ControlCircle
          class="bg-surface-light-200 dark:bg-surface-dark-700 h-8 w-8"
          ><ChevronDownIcon class="size-3.5 shrink-0" /></ControlCircle
        >
      </ToolbarHighlight>
      <span class="text-[9px] whitespace-nowrap">{$t`View More`}</span>
    </div>
  </div>
</Surface>
