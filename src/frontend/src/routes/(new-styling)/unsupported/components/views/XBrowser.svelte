<script lang="ts">
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import {
    ChevronLeftIcon,
    PanelTopIcon,
    RotateCwIcon,
    XIcon,
  } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import Toolbar from "../ui/Toolbar.svelte";
  import ToolbarButton from "../ui/ToolbarButton.svelte";
  import ToolbarHighlight from "../ui/ToolbarHighlight.svelte";
</script>

<div class="flex min-h-[100dvh] flex-col">
  <div class="mx-auto mt-auto flex max-w-100 flex-col px-6 sm:mb-auto sm:py-6">
    <div class="limited-height mb-4">
      <FeaturedIcon size="md">
        <PanelTopIcon class="size-5" />
      </FeaturedIcon>
    </div>
    <h1 class="text-text-primary mb-3 text-2xl font-medium">
      {$t`Open in browser to continue`}
    </h1>
    <p class="text-text-tertiary mb-5 text-base font-medium">
      <Trans>
        It looks like you're trying to sign in from a social app. Follow these
        steps to continue:
      </Trans>
    </p>
    <ol class="flex flex-col gap-3">
      <li
        class="bg-surface-light-200 dark:bg-surface-dark-700 rounded-xl px-3 pt-5 pb-4"
      >
        <div class="mb-3 flex flex-row items-center gap-3">
          <div
            class="bg-fg-primary-inversed text-fg-secondary size-8 rounded-full text-center leading-8"
            aria-hidden="true"
          >
            1
          </div>
          <h2 class="text-text-primary text-lg">{$t`Return to application`}</h2>
        </div>
        <p class="text-text-primary mb-5 text-sm">
          <Trans>
            Tap on the highlighted <b>back</b> button.
          </Trans>
        </p>
        <Toolbar aria-hidden="true">
          <ToolbarButton>
            <XIcon class="size-4" />
          </ToolbarButton>
          <ToolbarHighlight>
            <ToolbarButton>
              <ChevronLeftIcon class="size-4" />
            </ToolbarButton>
          </ToolbarHighlight>
          <ToolbarButton class="mx-auto px-3 text-[8px]">
            {window.location.host}
          </ToolbarButton>
          <ToolbarButton class="invisible" />
          <ToolbarButton>
            <RotateCwIcon class="size-4" />
          </ToolbarButton>
        </Toolbar>
      </li>
      <li
        class="bg-surface-light-200 dark:bg-surface-dark-700 rounded-xl px-3 pt-5 pb-4"
      >
        <div class="mb-3 flex flex-row items-center gap-3">
          <div
            class="bg-fg-primary-inversed text-fg-secondary size-8 rounded-full text-center leading-8"
            aria-hidden="true"
          >
            2
          </div>
          <h2 class="text-text-primary text-lg">{$t`Open in browser`}</h2>
        </div>
        <p class="text-text-primary mb-5 text-sm">
          <Trans>
            Tap on the <b>address bar</b> and <b>open in browser</b>.
          </Trans>
        </p>
        <Toolbar aria-hidden="true">
          <ToolbarButton>
            <XIcon class="size-4" />
          </ToolbarButton>
          <ToolbarButton>
            <ChevronLeftIcon class="size-4" />
          </ToolbarButton>
          <ToolbarHighlight class="mx-auto">
            <ToolbarButton class="px-3 text-[8px]">example.com</ToolbarButton>
          </ToolbarHighlight>
          <ToolbarButton class="invisible" />
          <ToolbarButton>
            <RotateCwIcon class="size-4" />
          </ToolbarButton>
        </Toolbar>
      </li>
    </ol>
    <div class="max-sm:h-20"></div>
  </div>
</div>

<style>
  @media (max-height: 640px) {
    .limited-height {
      display: none;
    }
  }
</style>
