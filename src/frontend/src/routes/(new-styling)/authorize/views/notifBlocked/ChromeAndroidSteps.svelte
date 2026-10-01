<script lang="ts">
  import {
    BellIcon,
    ChevronRightIcon,
    LockIcon,
    MoreVerticalIcon,
    SlidersHorizontalIcon,
    SquareIcon,
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
    <Trans>Tap the highlighted <b>site settings</b> icon.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockSurface class="p-2.5">
      <div class="flex items-center gap-3 ps-1.5">
        <MockField class="h-9 min-w-0 flex-1 rounded-full ps-2 pe-1">
          <ToolbarHighlight>
            <MockGlyph><SlidersHorizontalIcon class="size-3.5" /></MockGlyph>
          </ToolbarHighlight>
          <span class="ms-5 truncate text-[10px]">{address}</span>
        </MockField>
        <SquareIcon class="size-4 shrink-0" />
        <MoreVerticalIcon class="size-4 shrink-0" />
      </div>
    </MockSurface>
  {/snippet}
</StepCard>

<StepCard n={2} title={$t`Open permissions`}>
  {#snippet instruction()}
    <Trans>Tap <b>Permissions</b>.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockSurface class="px-3.5 pt-3 pb-3.5">
      <div class="flex items-center justify-center px-0.5 pb-2">
        <span class="text-xs font-semibold">{host}</span>
      </div>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <LockIcon class="size-3.5 shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Connection is secure`}</span>
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 my-1 h-px"
      ></div>
      <ToolbarHighlight class="my-1.5">
        <MockField class="h-10 gap-2.5 rounded-full px-2.5">
          <BellIcon class="size-3.5 shrink-0" />
          <div class="flex min-w-0 flex-1 flex-col">
            <span class="text-[11px]">{$t`Permissions`}</span>
            <span class="text-text-tertiary text-[10px]">
              {$t`Notifications blocked`}
            </span>
          </div>
          <ChevronRightIcon class="size-3.5 shrink-0" />
        </MockField>
      </ToolbarHighlight>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 my-1 h-px"
      ></div>
      <div class="flex items-center gap-2.5 px-2.5 py-2 opacity-45">
        <SquareIcon class="size-3.5 shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Last visited yesterday`}</span>
      </div>
    </MockSurface>
  {/snippet}
</StepCard>

<StepCard n={3} title={$t`Open notification settings`}>
  {#snippet instruction()}
    <Trans>Tap <b>Notifications</b>.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockSurface class="px-3.5 pt-3 pb-3.5">
      <div class="flex items-center justify-center px-0.5 pb-2">
        <span class="text-xs font-semibold">{host}</span>
      </div>
      <div class="flex items-center gap-2.5 px-0.5 pt-1 pb-2">
        <ChevronRightIcon class="size-3.5 shrink-0" />
        <span class="text-[11px]">{$t`Permissions`}</span>
      </div>
      <div class="flex items-center gap-2.5 px-0.5 py-2">
        <BellIcon class="size-3.5 shrink-0" />
        <div class="flex min-w-0 flex-1 flex-col">
          <span class="text-[11px]">{$t`Notifications`}</span>
          <span class="text-text-tertiary text-[10px]">{$t`Not allowed`}</span>
        </div>
        <ToolbarHighlight class="me-1.5">
          <MockGlyph class="size-6"
            ><ChevronRightIcon class="size-3" /></MockGlyph
          >
        </ToolbarHighlight>
      </div>
    </MockSurface>
  {/snippet}
</StepCard>

<StepCard n={4} title={$t`Allow notifications`}>
  {#snippet instruction()}
    <!-- Android's own wording, which is not the browser's. -->
    <Trans>Turn on <b>Show notifications</b>.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockSurface class="px-3.5 pt-3 pb-3.5">
      <div class="flex items-center gap-2.5 px-0.5 pb-2.5">
        <BellIcon class="size-3.5 shrink-0" />
        <span class="text-xs font-semibold">{host}</span>
      </div>
      <div class="flex items-center gap-2.5 py-2 ps-0.5 pe-2">
        <span class="flex-1 text-[11px]">{$t`Show notifications`}</span>
        <ToolbarHighlight>
          <MockToggle animate />
        </ToolbarHighlight>
      </div>
    </MockSurface>
  {/snippet}
</StepCard>
