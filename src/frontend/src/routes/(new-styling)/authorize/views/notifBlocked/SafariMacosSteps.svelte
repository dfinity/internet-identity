<script lang="ts">
  import BellIcon from "@lucide/svelte/icons/bell";
  import ChevronsUpDownIcon from "@lucide/svelte/icons/chevrons-up-down";
  import GlobeIcon from "@lucide/svelte/icons/globe";
  import HandIcon from "@lucide/svelte/icons/hand";
  import SettingsIcon from "@lucide/svelte/icons/settings";
  import ShieldHalfIcon from "@lucide/svelte/icons/shield-half";
  import UserRoundIcon from "@lucide/svelte/icons/user-round";
  import VideoIcon from "@lucide/svelte/icons/video";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import LabelSwap from "$lib/components/ui/browserMock/LabelSwap.svelte";
  import Step from "$lib/components/ui/browserMock/Step.svelte";
  import Steps from "$lib/components/ui/browserMock/Steps.svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import AppleIcon from "./icons/AppleIcon.svelte";
  import MacosSettingsIcon from "./icons/MacosSettingsIcon.svelte";

  const { host }: { host: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Open Safari settings`}>
    {#snippet instruction()}
      <Trans>Select <b>Safari</b> › <b>Settings…</b> in the menu bar.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-2.5 pt-2 pb-3">
      <div class="flex items-center gap-3 text-[10px]">
        <AppleIcon width="11" height="11" class="ml-1 shrink-0" />
        <span
          class="bg-surface-light-300 dark:bg-surface-dark-600 -mx-2 rounded-full px-2 py-0.5 font-bold"
          >Safari</span
        >
        <span>{$t`File`}</span>
        <span>{$t`Edit`}</span>
        <span>{$t`View`}</span>
        <span>{$t`History`}</span>
        <span>{$t`Bookmarks`}</span>
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 mt-1 ml-[19px] w-[190px] rounded-[10px] px-[3px] py-[5px]"
      >
        <div class="px-[5px] py-1 text-[10px]">{$t`About Safari`}</div>
        <div class="px-[5px] py-1 text-[10px]">{$t`Safari Extensions…`}</div>
        <div
          class="bg-surface-light-300 dark:bg-surface-dark-600 mx-[5px] my-[3px] h-px"
        ></div>
        <ToolbarHighlight shape="menu">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex items-center gap-1.5 rounded-[5px] px-[5px] py-1 text-[10px]"
          >
            <MacosSettingsIcon width="11" height="11" class="shrink-0" />
            <span class="flex-1">{$t`Settings…`}</span>
            <span class="opacity-70">⌘ ,</span>
          </div>
        </ToolbarHighlight>
        <div class="flex items-center gap-1.5 px-[5px] py-1 text-[10px]">
          <ShieldHalfIcon class="size-[11px] shrink-0" />{$t`Privacy Report…`}
        </div>
        <div
          class="bg-surface-light-300 dark:bg-surface-dark-600 mx-[5px] my-[3px] h-px"
        ></div>
        <div class="px-[5px] py-1 text-[10px]">{$t`Clear History…`}</div>
      </div>
    </Surface>
  </Step>

  <Step n={2} title={$t`Go to Websites`}>
    {#snippet instruction()}
      <Trans>Select the <b>Websites</b> tab.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 py-2">
      <div class="flex items-center justify-between px-2">
        <div class="flex flex-col items-center gap-[3px] text-[9px] opacity-45">
          <SettingsIcon class="size-3.5 shrink-0" />
          <span>{$t`General`}</span>
        </div>
        <div class="flex flex-col items-center gap-[3px] text-[9px] opacity-45">
          <HandIcon class="size-3.5 shrink-0" />
          <span>{$t`Privacy`}</span>
        </div>
        <ToolbarHighlight>
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex flex-col items-center gap-0.5 rounded-full px-3 py-1 text-[9px]"
          >
            <GlobeIcon class="size-3.5 shrink-0" />
            <span>{$t`Websites`}</span>
          </div>
        </ToolbarHighlight>
        <div class="flex flex-col items-center gap-[3px] text-[9px] opacity-45">
          <UserRoundIcon class="size-3.5 shrink-0" />
          <span>{$t`Profiles`}</span>
        </div>
      </div>
    </Surface>
  </Step>

  <Step n={3} title={$t`Allow notifications`}>
    {#snippet instruction()}
      <Trans>Select <b>Notifications</b>, set {host} to <b>Allow</b>.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
      <div class="flex gap-2.5">
        <div class="flex w-24 shrink-0 flex-col gap-0.5 text-[10px]">
          <div class="flex items-center gap-1.5 px-1.5 py-[5px] opacity-45">
            <VideoIcon class="size-[11px] shrink-0" />{$t`Camera`}
          </div>
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex items-center gap-1.5 rounded-md px-1.5 py-[5px]"
          >
            <BellIcon class="size-[11px] shrink-0" />{$t`Notifications`}
          </div>
        </div>
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex min-w-0 flex-1 items-center rounded-lg pt-2 pr-2 pb-2 pl-2.5"
        >
          <span class="flex-1 text-[10px]">{host}</span>
          <ToolbarHighlight>
            <div
              class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-5 items-center gap-1 rounded-full pr-[5px] pl-2 text-[10px]"
            >
              <LabelSwap from={$t`Deny`} to={$t`Allow`} />
              <ChevronsUpDownIcon class="size-[9px] shrink-0" />
            </div>
          </ToolbarHighlight>
        </div>
      </div>
    </Surface>
  </Step>
</Steps>
