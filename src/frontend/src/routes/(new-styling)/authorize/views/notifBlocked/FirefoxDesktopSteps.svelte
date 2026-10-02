<script lang="ts">
  import MessageSquareOffIcon from "@lucide/svelte/icons/message-square-off";
  import Settings2Icon from "@lucide/svelte/icons/settings-2";
  import ShieldCheckIcon from "@lucide/svelte/icons/shield-check";
  import XIcon from "@lucide/svelte/icons/x";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import AddressBar from "$lib/components/ui/browserMock/AddressBar.svelte";
  import ControlCircle from "$lib/components/ui/browserMock/ControlCircle.svelte";
  import Divider from "$lib/components/ui/browserMock/Divider.svelte";
  import Step from "$lib/components/ui/browserMock/Step.svelte";
  import Steps from "$lib/components/ui/browserMock/Steps.svelte";
  import Surface from "$lib/components/ui/browserMock/Surface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";

  const { host, address }: { host: string; address: string } = $props();
</script>

<Steps>
  <Step n={1} title={$t`Open site permissions`}>
    {#snippet instruction()}
      <Trans>Select the highlighted <b>permissions</b> icon.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="p-2.5">
      <AddressBar {address} class="text-text-primary pr-3 pl-2.5">
        <ShieldCheckIcon class="size-3.5 shrink-0" />
        <ToolbarHighlight class="ml-2">
          <ControlCircle class="h-7 gap-1.5 px-2">
            <Settings2Icon class="size-[13px] shrink-0" />
            <MessageSquareOffIcon class="size-[13px] shrink-0" />
          </ControlCircle>
        </ToolbarHighlight>
      </AddressBar>
    </Surface>
  </Step>

  <Step n={2} title={$t`Clear the block`}>
    {#snippet instruction()}
      <Trans>Select <b>Blocked ×</b> to clear it.</Trans>
    {/snippet}
    <Surface aria-hidden="true" class="text-text-primary px-3.5 pt-3 pb-3.5">
      <div class="px-0.5 pb-2.5 text-center text-[12px] font-semibold">
        {$t`Permissions for ${host}`}
      </div>
      <Divider class="mb-1" />
      <div class="flex items-center gap-2.5 px-0.5 py-2.5">
        <MessageSquareOffIcon class="size-[13px] shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Send notifications`}</span>
        <ToolbarHighlight>
          <div
            class="border-surface-light-300 dark:border-surface-dark-600 flex h-[22px] items-center gap-1 rounded-full border pr-1.5 pl-[9px] text-[10px]"
          >
            {$t`Blocked`}<XIcon class="size-2.5 shrink-0" />
          </div>
        </ToolbarHighlight>
      </div>
    </Surface>
  </Step>
</Steps>
