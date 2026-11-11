<script lang="ts">
  import ArrowLeftIcon from "@lucide/svelte/icons/arrow-left";
  import BellOffIcon from "@lucide/svelte/icons/bell-off";
  import EllipsisVerticalIcon from "@lucide/svelte/icons/ellipsis-vertical";
  import ExternalLinkIcon from "@lucide/svelte/icons/external-link";
  import HistoryIcon from "@lucide/svelte/icons/history";
  import LockIcon from "@lucide/svelte/icons/lock";
  import Settings2Icon from "@lucide/svelte/icons/settings-2";
  import SlidersHorizontalIcon from "@lucide/svelte/icons/sliders-horizontal";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import AddressBar from "$lib/components/ui/browserMock/AddressBar.svelte";
  import ControlCircle from "$lib/components/ui/browserMock/ControlCircle.svelte";
  import Divider from "$lib/components/ui/browserMock/Divider.svelte";
  import Step from "$lib/components/ui/browserMock/Step.svelte";
  import Steps from "$lib/components/ui/browserMock/Steps.svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import Toggle from "$lib/components/ui/browserMock/Toggle.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import TabCountIcon from "./icons/TabCountIcon.svelte";

  const { host, address }: { host: string; address: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Open site settings`}>
    {#snippet instruction()}
      <Trans>Tap the highlighted <b>site settings</b> icon.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="p-2.5">
      <div class="text-text-primary flex items-center gap-3 pl-1.5">
        <AddressBar {address} class="min-w-0 flex-1 pr-1 pl-2">
          <ToolbarHighlight>
            <ControlCircle class="h-7 w-7">
              <Settings2Icon class="size-3.5 shrink-0" />
            </ControlCircle>
          </ToolbarHighlight>
        </AddressBar>
        <TabCountIcon class="size-3.5 shrink-0" />
        <EllipsisVerticalIcon class="mr-1 size-3.5 shrink-0" />
      </div>
    </Surface>
  </Step>

  <Step n={2} title={$t`Open permissions`}>
    {#snippet instruction()}
      <Trans>Tap <b>Permissions</b>.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
      <div class="flex items-center justify-center px-0.5 pb-2">
        <span class="text-[12px] font-semibold">{host}</span>
      </div>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <LockIcon class="size-[13px] shrink-0" />
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">{$t`Connection is secure`}</span>
        </div>
      </div>
      <Divider class="my-1" />
      <ToolbarHighlight class="mt-1.5 mb-1">
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex h-10 items-center gap-2.5 rounded-full px-2.5"
        >
          <SlidersHorizontalIcon class="size-[13px] shrink-0" />
          <div class="flex min-w-0 flex-1 flex-col">
            <span class="text-[11px]">{$t`Permissions`}</span>
            <span class="text-text-tertiary text-[10px]"
              >{$t`Notifications blocked`}</span
            >
          </div>
        </div>
      </ToolbarHighlight>
      <Divider class="my-1" />
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <HistoryIcon class="size-[13px] shrink-0" />
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">{$t`Last visited yesterday`}</span>
        </div>
      </div>
    </Surface>
  </Step>

  <Step n={3} title={$t`Open notification settings`}>
    {#snippet instruction()}
      <Trans>Tap <b>Notifications</b>.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
      <div class="flex items-center justify-center px-0.5 pb-2">
        <span class="text-[12px] font-semibold">{host}</span>
      </div>
      <div class="flex items-center gap-2.5 px-0.5 pt-1 pb-2">
        <ArrowLeftIcon class="size-[13px] shrink-0" />
        <span class="text-[11px]">{$t`Permissions`}</span>
      </div>
      <div class="flex items-center gap-2.5 px-0.5 py-2">
        <BellOffIcon class="size-[13px] shrink-0" />
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">{$t`Notifications`}</span>
          <span class="text-text-tertiary text-[10px]">{$t`Not allowed`}</span>
        </div>
        <ToolbarHighlight class="mr-1.5">
          <ControlCircle class="h-6 w-6">
            <ExternalLinkIcon class="size-3 shrink-0" />
          </ControlCircle>
        </ToolbarHighlight>
      </div>
    </Surface>
  </Step>

  <Step n={4} title={$t`Allow notifications`}>
    {#snippet instruction()}
      <Trans>Turn on <b>Show notifications</b>.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
      <div class="flex items-center gap-2.5 px-0.5 pb-2.5">
        <ArrowLeftIcon class="size-[13px] shrink-0" />
        <span class="text-[12px] font-semibold">{host}</span>
      </div>
      <div class="flex items-center gap-2.5 pt-2 pr-2 pb-2 pl-0.5">
        <span class="flex-1 text-[11px]">{$t`Show notifications`}</span>
        <ToolbarHighlight>
          <Toggle />
        </ToolbarHighlight>
      </div>
    </Surface>
  </Step>
</Steps>
