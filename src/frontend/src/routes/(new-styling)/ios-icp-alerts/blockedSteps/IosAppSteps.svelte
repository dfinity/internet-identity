<script lang="ts">
  import BellIcon from "@lucide/svelte/icons/bell";
  import ChevronRightIcon from "@lucide/svelte/icons/chevron-right";
  import ImageIcon from "@lucide/svelte/icons/image";
  import Volume1Icon from "@lucide/svelte/icons/volume-1";
  import { t } from "$lib/stores/locale.store";
  import Logo from "$lib/components/ui/Logo.svelte";
  import IosPanel from "../IosPanel.svelte";
  import Step from "$lib/components/ui/browserMock/Step.svelte";
  import Steps from "$lib/components/ui/browserMock/Steps.svelte";
  import Toggle from "$lib/components/ui/browserMock/Toggle.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import HomeScreenGrid from "../installSteps/HomeScreenGrid.svelte";
  import IosSettingsIcon from "./icons/IosSettingsIcon.svelte";
  import ListIcon from "./icons/ListIcon.svelte";
  import WallpaperIcon from "./icons/WallpaperIcon.svelte";

  /** What this app is called on the Home Screen and in Settings, which is where these
   *  steps send the user. */
  const { appName }: { appName: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Open Settings`}>
    <HomeScreenGrid label={$t`Settings`} blanks={4}>
      {#snippet icon()}
        <IosSettingsIcon width="32" height="32" class="shrink-0" />
      {/snippet}
    </HomeScreenGrid>
  </Step>

  <Step n={2} title={$t`Open notifications`}>
    <IosPanel>
      <div class="px-0.5 pb-2 text-[12px] font-semibold">{$t`Settings`}</div>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <WallpaperIcon width="13" height="13" class="shrink-0" />
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">{$t`Wallpaper`}</span>
        </div>
        <ChevronRightIcon class="size-3 shrink-0" />
      </div>
      <ToolbarHighlight class="mt-1.5 mb-1">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-8 items-center gap-2.5 rounded-full px-2.5"
        >
          <BellIcon class="size-[13px] shrink-0" />
          <div class="flex min-w-0 flex-1 flex-col">
            <span class="text-[11px]">{$t`Notifications`}</span>
          </div>
          <ChevronRightIcon class="size-3 shrink-0" />
        </div>
      </ToolbarHighlight>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <Volume1Icon class="size-[13px] shrink-0" />
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">{$t`Sounds & Haptics`}</span>
        </div>
        <ChevronRightIcon class="size-3 shrink-0" />
      </div>
    </IosPanel>
  </Step>

  <Step n={3} title={$t`Find ${appName}`}>
    <IosPanel>
      <div class="px-0.5 pb-2 text-[12px] font-semibold">
        {$t`Notifications`}
      </div>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <div
          class="flex h-[18px] w-[18px] shrink-0 items-center justify-center"
        >
          <ImageIcon class="size-[13px] shrink-0" />
        </div>
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">{$t`Photos`}</span>
          <span class="text-text-tertiary text-[10px]"
            >{$t`Banners, Sounds, Badges`}</span
          >
        </div>
        <ChevronRightIcon class="size-3 shrink-0" />
      </div>
      <ToolbarHighlight class="mt-1.5 mb-1">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-10 items-center gap-2.5 rounded-full px-2.5"
        >
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-[18px] w-[18px] shrink-0 items-center justify-center rounded-[5px]"
          >
            <Logo width="12" height="6" class="shrink-0" />
          </div>
          <div class="flex min-w-0 flex-1 flex-col">
            <span class="text-[11px]">{appName}</span>
            <span class="text-text-tertiary text-[10px]">{$t`Off`}</span>
          </div>
          <ChevronRightIcon class="size-3 shrink-0" />
        </div>
      </ToolbarHighlight>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <div
          class="flex h-[18px] w-[18px] shrink-0 items-center justify-center"
        >
          <ListIcon width="13" height="13" class="shrink-0" />
        </div>
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">{$t`Reminders`}</span>
          <span class="text-text-tertiary text-[10px]"
            >{$t`Banners, Sounds, Badges`}</span
          >
        </div>
        <ChevronRightIcon class="size-3 shrink-0" />
      </div>
    </IosPanel>
  </Step>

  <Step n={4} title={$t`Allow notifications`}>
    <IosPanel>
      <div class="flex items-center justify-between px-0.5 pb-2">
        <span class="text-[12px] font-semibold">{appName}</span>
      </div>
      <div class="flex items-center gap-2.5 pt-2 pr-2 pb-2 pl-0.5">
        <span class="flex-1 text-[11px]">{$t`Allow Notifications`}</span>
        <ToolbarHighlight>
          <Toggle />
        </ToolbarHighlight>
      </div>
    </IosPanel>
  </Step>
</Steps>
