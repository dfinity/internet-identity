<script lang="ts">
  import {
    BellIcon,
    LockIcon,
    MoreVerticalIcon,
    ShieldIcon,
    SquareIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import MockSurface from "$lib/components/ui/browserMock/MockSurface.svelte";
  import MockField from "$lib/components/ui/browserMock/MockField.svelte";
  import MockGlyph from "$lib/components/ui/browserMock/MockGlyph.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import StepCard from "./StepCard.svelte";

  const { host, address }: { host: string; address: string } = $props();
</script>

<StepCard n={1} title={$t`Open site settings`}>
  {#snippet instruction()}
    <Trans>Tap the highlighted <b>shield</b> icon.</Trans>
  {/snippet}
  {#snippet mock()}
    <!-- Firefox on Android keeps the address bar at the bottom of the screen. -->
    <MockSurface class="p-2.5">
      <div class="flex items-center gap-3 ps-1.5">
        <MockField class="h-9 min-w-0 flex-1 rounded-full ps-2 pe-1">
          <ToolbarHighlight>
            <MockGlyph><ShieldIcon class="size-3.5" /></MockGlyph>
          </ToolbarHighlight>
          <span class="ms-5 truncate text-[10px]">{address}</span>
        </MockField>
        <SquareIcon class="size-4 shrink-0" />
        <MoreVerticalIcon class="size-4 shrink-0" />
      </div>
    </MockSurface>
  {/snippet}
</StepCard>

<StepCard n={2} title={$t`Allow notifications`}>
  {#snippet instruction()}
    <Trans>Tap <b>Blocked</b> to allow.</Trans>
  {/snippet}
  {#snippet mock()}
    <!-- A sheet from the bottom, which is where Firefox puts the site's panel. -->
    <MockSurface class="px-3 pt-2 pb-3.5">
      <div
        class="bg-surface-light-300 dark:bg-surface-dark-600 mx-auto mb-2.5 h-[3px] w-7 rounded-full"
      ></div>
      <div class="flex items-center gap-2.5 px-0.5 pb-3">
        <MockGlyph class="size-6 rounded-md">
          <BellIcon class="size-3" />
        </MockGlyph>
        <div class="flex flex-col">
          <span class="text-[11px] font-semibold">{$t`Internet Identity`}</span>
          <span class="text-text-tertiary text-[10px]">{host}</span>
        </div>
      </div>
      <MockField class="gap-2.5 rounded-xl px-2.5 py-2 opacity-45">
        <LockIcon class="size-3.5 shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Secure connection`}</span>
      </MockField>
      <div class="px-0.5 pt-3 pb-1.5 text-[10px] font-semibold">
        {$t`Permissions`}
      </div>
      <MockField class="gap-2.5 rounded-xl p-2.5">
        <BellIcon class="size-3.5 shrink-0" />
        <span class="flex-1 text-[11px]">{$t`Notification`}</span>
        <ToolbarHighlight class="me-1">
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 rounded-full px-1.5 py-0.5 text-[10px]"
          >
            <!-- The label the tap changes, played out in place. -->
            <span class="inline-grid">
              <span class="mock-out [grid-area:1/1]">{$t`Blocked`}</span>
              <span class="mock-in opacity-0 [grid-area:1/1]">
                {$t`Allowed`}
              </span>
            </span>
          </div>
        </ToolbarHighlight>
      </MockField>
    </MockSurface>
  {/snippet}
</StepCard>

<style>
  .mock-out {
    animation: mock-out 3s ease-in-out infinite;
  }
  .mock-in {
    animation: mock-in 3s ease-in-out infinite;
  }
  @keyframes mock-out {
    0%,
    35% {
      opacity: 1;
    }
    45%,
    90% {
      opacity: 0;
    }
    100% {
      opacity: 1;
    }
  }
  @keyframes mock-in {
    0%,
    35% {
      opacity: 0;
    }
    45%,
    90% {
      opacity: 1;
    }
    100% {
      opacity: 0;
    }
  }
  /* Still, the label names what the step tells the user to look for, not what it
     becomes: there is no movement to explain the swap. */
  @media (prefers-reduced-motion: reduce) {
    .mock-out {
      animation: none;
      opacity: 1;
    }
    .mock-in {
      animation: none;
      opacity: 0;
    }
  }
</style>
