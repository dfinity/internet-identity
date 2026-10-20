<script lang="ts">
  import ChevronLeftIcon from "@lucide/svelte/icons/chevron-left";
  import EllipsisIcon from "@lucide/svelte/icons/ellipsis";
  import PlusIcon from "@lucide/svelte/icons/plus";
  import RotateCwIcon from "@lucide/svelte/icons/rotate-cw";
  import SearchIcon from "@lucide/svelte/icons/search";
  import ShareIcon from "@lucide/svelte/icons/share";
  import SquarePlusIcon from "@lucide/svelte/icons/square-plus";
  import { t } from "$lib/stores/locale.store";
  import ControlCircle from "$lib/components/ui/browserMock/ControlCircle.svelte";
  import IosPanel from "../IosPanel.svelte";
  import Step from "$lib/components/ui/browserMock/Step.svelte";
  import Steps from "$lib/components/ui/browserMock/Steps.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import BookmarkIcon from "./icons/BookmarkIcon.svelte";
  import MarkupIcon from "./icons/MarkupIcon.svelte";
  import ViewMoreSheet from "./ViewMoreSheet.svelte";

  const { host }: { host: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Open the menu`}>
    <IosPanel>
      <div class="flex items-center gap-2">
        <div class="relative shrink-0">
          <ControlCircle
            class="bg-surface-light-200 dark:bg-surface-dark-700 h-8 w-8"
            ><ChevronLeftIcon class="size-3.5 shrink-0" /></ControlCircle
          >
        </div>
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-8 min-w-0 flex-1 items-center justify-center gap-2 rounded-full px-3"
        >
          <span class="flex-1 text-center text-[10px]">{host}</span>
          <RotateCwIcon class="size-3 shrink-0" />
        </div>
        <ToolbarHighlight class="shrink-0">
          <ControlCircle
            class="bg-surface-light-200 dark:bg-surface-dark-700 h-8 w-8"
            ><EllipsisIcon class="size-3.5 shrink-0" /></ControlCircle
          >
        </ToolbarHighlight>
      </div>
    </IosPanel>
  </Step>

  <Step n={2} title={$t`Tap Share`}>
    <IosPanel>
      <ToolbarHighlight class="mt-1.5 mb-1">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-8 items-center gap-2.5 rounded-full px-2.5"
        >
          <ShareIcon class="size-[13px] shrink-0" />
          <span class="flex-1 text-[11px]">{$t`Share`}</span>
        </div>
      </ToolbarHighlight>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <BookmarkIcon width="13" height="13" class="shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Add to Bookmarks`}</span>
      </div>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <PlusIcon class="size-[13px] shrink-0" />
        <span class="flex-1 text-[11px]">{$t`New Tab`}</span>
      </div>
    </IosPanel>
  </Step>

  <Step n={3} title={$t`Tap View More`}>
    <ViewMoreSheet />
  </Step>

  <Step n={4} title={$t`Tap Add to Home Screen`}>
    <IosPanel>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <SearchIcon class="size-[13px] shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Find on Page`}</span>
      </div>
      <ToolbarHighlight class="mt-1.5 mb-1">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-8 items-center gap-2.5 rounded-full px-2.5"
        >
          <SquarePlusIcon class="size-[13px] shrink-0" />
          <span class="flex-1 text-[11px]">{$t`Add to Home Screen`}</span>
        </div>
      </ToolbarHighlight>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <MarkupIcon width="13" height="13" class="shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Markup`}</span>
      </div>
    </IosPanel>
  </Step>
</Steps>
