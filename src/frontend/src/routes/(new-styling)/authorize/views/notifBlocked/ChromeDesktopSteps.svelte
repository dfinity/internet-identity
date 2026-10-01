<script lang="ts">
  import ArrowLeftIcon from "@lucide/svelte/icons/arrow-left";
  import ArrowRightIcon from "@lucide/svelte/icons/arrow-right";
  import BellIcon from "@lucide/svelte/icons/bell";
  import ChevronRightIcon from "@lucide/svelte/icons/chevron-right";
  import LockIcon from "@lucide/svelte/icons/lock";
  import RotateCwIcon from "@lucide/svelte/icons/rotate-cw";
  import Settings2Icon from "@lucide/svelte/icons/settings-2";
  import XIcon from "@lucide/svelte/icons/x";
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

  const { host, address }: { host: string; address: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Open site settings`}>
    {#snippet instruction()}
      <Trans>Select the highlighted <b>site settings</b> icon.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="p-2.5">
      <div class="text-text-primary flex items-center gap-3 pl-1.5">
        <ArrowLeftIcon class="size-3.5 shrink-0" />
        <ArrowRightIcon class="size-3.5 shrink-0 opacity-40" />
        <RotateCwIcon class="size-3.5 shrink-0" />
        <AddressBar {address} class="min-w-0 flex-1 pr-1 pl-2">
          <ToolbarHighlight>
            <ControlCircle class="h-7 w-7">
              <Settings2Icon class="size-3.5 shrink-0" />
            </ControlCircle>
          </ToolbarHighlight>
        </AddressBar>
      </div>
    </Surface>
  </Step>

  <Step n={2} title={$t`Allow notifications`}>
    {#snippet instruction()}
      <Trans>Turn on <b>Notifications</b>.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
      <div class="flex items-center justify-between px-0.5 pb-2">
        <span class="text-[12px] font-semibold">{host}</span>
        <XIcon class="size-3 shrink-0" />
      </div>
      <div class="flex items-center gap-2.5 pt-2 pr-2 pb-2 pl-0.5 opacity-45">
        <LockIcon class="size-[13px] shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Connection is secure`}</span>
        <ChevronRightIcon class="size-3 shrink-0" />
      </div>
      <Divider class="my-1" />
      <div class="flex items-center gap-2.5 pt-2 pr-2 pb-2 pl-0.5">
        <BellIcon class="size-[13px] shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Notifications`}</span>
        <ToolbarHighlight>
          <Toggle />
        </ToolbarHighlight>
      </div>
    </Surface>
  </Step>
</Steps>
