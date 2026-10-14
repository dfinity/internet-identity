<script lang="ts">
  import ChevronLeftIcon from "@lucide/svelte/icons/chevron-left";
  import EllipsisIcon from "@lucide/svelte/icons/ellipsis";
  import PlusIcon from "@lucide/svelte/icons/plus";
  import RotateCwIcon from "@lucide/svelte/icons/rotate-cw";
  import SearchIcon from "@lucide/svelte/icons/search";
  import ShareIcon from "@lucide/svelte/icons/share";
  import SquarePlusIcon from "@lucide/svelte/icons/square-plus";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Logo from "$lib/components/ui/Logo.svelte";
  import { NOTIFICATION_APP_NAME } from "../appName";
  import ControlCircle from "$lib/components/ui/browserMock/ControlCircle.svelte";
  import Step from "$lib/components/ui/browserMock/Step.svelte";
  import Steps from "$lib/components/ui/browserMock/Steps.svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import BookmarkIcon from "./icons/BookmarkIcon.svelte";
  import MarkupIcon from "./icons/MarkupIcon.svelte";
  import ConfirmSheet from "./ConfirmSheet.svelte";
  import HomeScreenGrid from "./HomeScreenGrid.svelte";
  import ViewMoreSheet from "./ViewMoreSheet.svelte";

  const { host }: { host: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Open the menu`}>
    {#snippet instruction()}
      <Trans>Tap the highlighted <b>more</b> button.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
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
    </Surface>
  </Step>

  <Step n={2} title={$t`Share the page`}>
    {#snippet instruction()}
      <Trans>Tap <b>Share</b>.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
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
    </Surface>
  </Step>

  <Step n={3} title={$t`Show more options`}>
    {#snippet instruction()}
      <Trans>Tap <b>View More</b>.</Trans>
    {/snippet}
    <ViewMoreSheet />
  </Step>

  <Step n={4} title={$t`Add to Home Screen`}>
    {#snippet instruction()}
      <Trans>Tap <b>Add to Home Screen</b>.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
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
    </Surface>
  </Step>

  <Step n={5} title={$t`Confirm`}>
    {#snippet instruction()}
      <Trans>Keep <b>Open as Web App</b> on, then tap <b>Add</b>.</Trans>
    {/snippet}
    <ConfirmSheet />
  </Step>

  <Step n={6} title={$t`Open the app`}>
    {#snippet instruction()}
      <Trans>Open it from your <b>Home Screen</b>.</Trans>
    {/snippet}
    <HomeScreenGrid label={NOTIFICATION_APP_NAME} blanks={6}>
      {#snippet icon()}<Logo
          width="24"
          height="12"
          class="shrink-0"
        />{/snippet}
    </HomeScreenGrid>
  </Step>
</Steps>
