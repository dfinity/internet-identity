<script lang="ts">
  import {
    BellOffIcon,
    ShieldIcon,
    SlidersHorizontalIcon,
    XIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import MockSurface from "$lib/components/ui/browserMock/MockSurface.svelte";
  import MockField from "$lib/components/ui/browserMock/MockField.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import StepCard from "./StepCard.svelte";

  const { host, address }: { host: string; address: string } = $props();
</script>

<StepCard n={1} title={$t`Open site permissions`}>
  {#snippet instruction()}
    <Trans>Select the highlighted <b>permissions</b> icon.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockSurface class="p-2.5">
      <!-- Firefox puts the permissions button inside the address bar, beside the
           shield, rather than giving the site its own icon. -->
      <MockField class="h-9 rounded-full ps-2.5 pe-3">
        <ShieldIcon class="size-3.5 shrink-0" />
        <ToolbarHighlight class="ms-2">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-7 items-center gap-1.5 rounded-full px-2"
          >
            <BellOffIcon class="size-3" />
            <SlidersHorizontalIcon class="size-3" />
          </div>
        </ToolbarHighlight>
        <span class="ms-5 text-[10px] whitespace-nowrap">{address}</span>
      </MockField>
    </MockSurface>
  {/snippet}
</StepCard>

<StepCard n={2} title={$t`Clear the block`}>
  {#snippet instruction()}
    <Trans>Select <b>Blocked ×</b> to clear it.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockSurface class="px-3.5 pt-3 pb-3.5">
      <div class="px-0.5 pb-2.5 text-center text-xs font-semibold">
        {$t`Permissions for ${host}`}
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 mb-1 h-px"
      ></div>
      <div class="flex items-center gap-2.5 px-0.5 py-2.5">
        <BellOffIcon class="size-3.5 shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Send notifications`}</span>
        <ToolbarHighlight>
          <div
            class="border-surface-light-300 dark:border-surface-dark-600 flex h-[22px] items-center gap-1 rounded-full border ps-2.5 pe-1.5 text-[10px]"
          >
            {$t`Blocked`}
            <XIcon class="size-2.5" />
          </div>
        </ToolbarHighlight>
      </div>
    </MockSurface>
  {/snippet}
</StepCard>
