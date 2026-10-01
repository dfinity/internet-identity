<script lang="ts">
  import {
    ArrowLeftIcon,
    ArrowRightIcon,
    BellIcon,
    ChevronRightIcon,
    LockIcon,
    RotateCwIcon,
    SlidersHorizontalIcon,
    XIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import MockSurface from "$lib/components/ui/browserMock/MockSurface.svelte";
  import MockField from "$lib/components/ui/browserMock/MockField.svelte";
  import MockGlyph from "$lib/components/ui/browserMock/MockGlyph.svelte";
  import MockToggle from "$lib/components/ui/browserMock/MockToggle.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import StepCard from "./StepCard.svelte";

  const { host, address }: { host: string; address: string } = $props();
</script>

<StepCard n={1} title={$t`Open site settings`}>
  {#snippet instruction()}
    <Trans>Select the highlighted <b>site settings</b> icon.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockSurface class="p-2.5">
      <div class="flex items-center gap-3 ps-1.5">
        <ArrowLeftIcon class="size-4 shrink-0" />
        <ArrowRightIcon class="size-4 shrink-0" />
        <RotateCwIcon class="size-4 shrink-0" />
        <MockField class="h-9 min-w-0 flex-1 rounded-full ps-2 pe-1">
          <ToolbarHighlight>
            <MockGlyph><SlidersHorizontalIcon class="size-3.5" /></MockGlyph>
          </ToolbarHighlight>
          <span class="ms-5 text-[10px] whitespace-nowrap">{address}</span>
        </MockField>
      </div>
    </MockSurface>
  {/snippet}
</StepCard>

<StepCard n={2} title={$t`Allow notifications`}>
  {#snippet instruction()}
    <Trans>Turn on <b>Notifications</b>.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockSurface class="px-3.5 pt-3 pb-3.5">
      <div class="flex items-center justify-between px-0.5 pb-2">
        <span class="text-xs font-semibold">{host}</span>
        <XIcon class="size-3.5" />
      </div>
      <div class="flex items-center gap-2.5 py-2 ps-0.5 pe-2 opacity-45">
        <LockIcon class="size-3.5 shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Connection is secure`}</span>
        <ChevronRightIcon class="size-3.5 shrink-0" />
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 my-1 h-px"
      ></div>
      <div class="flex items-center gap-2.5 py-2 ps-0.5 pe-2">
        <BellIcon class="size-3.5 shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Notifications`}</span>
        <ToolbarHighlight>
          <MockToggle animate />
        </ToolbarHighlight>
      </div>
    </MockSurface>
  {/snippet}
</StepCard>
