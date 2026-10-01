<script lang="ts">
  import EllipsisVerticalIcon from "@lucide/svelte/icons/ellipsis-vertical";
  import LockIcon from "@lucide/svelte/icons/lock";
  import MessageSquareTextIcon from "@lucide/svelte/icons/message-square-text";
  import PlusIcon from "@lucide/svelte/icons/plus";
  import ShieldCheckIcon from "@lucide/svelte/icons/shield-check";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Logo from "$lib/components/ui/Logo.svelte";
  import AddressBar from "$lib/components/ui/browserMock/AddressBar.svelte";
  import ControlCircle from "$lib/components/ui/browserMock/ControlCircle.svelte";
  import LabelSwap from "$lib/components/ui/browserMock/LabelSwap.svelte";
  import Step from "$lib/components/ui/browserMock/Step.svelte";
  import Steps from "$lib/components/ui/browserMock/Steps.svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import TabCountIcon from "./icons/TabCountIcon.svelte";

  const { host, address }: { host: string; address: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Open site settings`}>
    {#snippet instruction()}
      <Trans>Tap the highlighted <b>shield</b> icon.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="p-2.5">
      <div class="text-text-primary flex items-center gap-3 pl-1.5">
        <AddressBar {address} class="min-w-0 flex-1 pr-1 pl-2">
          <ToolbarHighlight>
            <ControlCircle class="h-7 w-7">
              <ShieldCheckIcon class="size-3.5 shrink-0" />
            </ControlCircle>
          </ToolbarHighlight>
        </AddressBar>
        <PlusIcon class="size-3.5 shrink-0" />
        <TabCountIcon class="size-3.5 shrink-0" />
        <EllipsisVerticalIcon class="mr-1 size-3.5 shrink-0" />
      </div>
    </Surface>
  </Step>

  <Step n={2} title={$t`Allow notifications`}>
    {#snippet instruction()}
      <Trans>Tap <b>Blocked</b> to allow.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3 pt-2 pb-3.5">
      <div
        class="bg-surface-light-300 dark:bg-surface-dark-600 mx-auto mb-2.5 h-[3px] w-7 rounded-full"
      ></div>
      <div class="flex items-center gap-2.5 px-0.5 pb-3">
        <div
          class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-6 w-6 shrink-0 items-center justify-center rounded-md"
        >
          <Logo width="16" height="8" class="shrink-0" />
        </div>
        <div class="flex flex-col">
          <span class="text-[11px] font-semibold">Internet Identity</span>
          <span class="text-text-tertiary text-[10px]">{host}</span>
        </div>
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 flex items-center gap-2.5 rounded-xl px-2.5 py-2 opacity-45"
      >
        <LockIcon class="size-[13px] shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Secure connection`}</span>
      </div>
      <div class="px-0.5 pt-3 pb-1.5 text-[10px] font-semibold">
        {$t`Permissions`}
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 flex items-center gap-2.5 rounded-xl px-2.5 py-2.5"
      >
        <MessageSquareTextIcon class="size-[13px] shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Notification`}</span>
        <ToolbarHighlight class="mr-1">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 rounded-full px-1.5 py-0.5 text-[10px]"
          >
            <LabelSwap from={$t`Blocked`} to={$t`Allowed`} />
          </div>
        </ToolbarHighlight>
      </div>
    </Surface>
  </Step>
</Steps>
