<script lang="ts">
  import {
    AppleIcon,
    BellIcon,
    CameraIcon,
    ChevronDownIcon,
    GlobeIcon,
    SettingsIcon,
    ShieldIcon,
    UserIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import MockSurface from "$lib/components/ui/browserMock/MockSurface.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import StepCard from "./StepCard.svelte";

  const { host }: { host: string } = $props();
</script>

<StepCard n={1} title={$t`Open Safari settings`}>
  {#snippet instruction()}
    <Trans>Select <b>Safari</b> › <b>Settings…</b> in the menu bar.</Trans>
  {/snippet}
  {#snippet mock()}
    <!-- Safari has no per-site control in the address bar, so the route starts in the
         menu bar. -->
    <MockSurface class="px-2.5 pt-2 pb-3">
      <div class="flex items-center gap-3 text-[10px]">
        <AppleIcon class="size-3 shrink-0" />
        <span
          class="bg-surface-light-200 dark:bg-surface-dark-700 -mx-2 rounded px-2 py-0.5 font-semibold"
        >
          {$t`Safari`}
        </span>
        <span>{$t`File`}</span>
        <span>{$t`Edit`}</span>
        <span>{$t`View`}</span>
      </div>
      <div
        class="bg-surface-light-200 dark:bg-surface-dark-700 ms-[19px] mt-1 w-[190px] rounded-[10px] px-[3px] py-[5px]"
      >
        <div class="px-[5px] py-1 text-[10px]">{$t`About Safari`}</div>
        <div class="px-[5px] py-1 text-[10px]">{$t`Safari Extensions…`}</div>
        <div
          class="bg-surface-light-300 dark:bg-surface-dark-600 mx-[5px] my-[3px] h-px"
        ></div>
        <ToolbarHighlight>
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex items-center gap-1.5 rounded-[5px] px-[5px] py-1 text-[10px]"
          >
            <SettingsIcon class="size-2.5 shrink-0" />
            <span class="flex-1">{$t`Settings…`}</span>
            <span class="opacity-70">⌘ ,</span>
          </div>
        </ToolbarHighlight>
        <div class="flex items-center gap-1.5 px-[5px] py-1 text-[10px]">
          <ShieldIcon class="size-2.5 shrink-0" />
          {$t`Privacy Report…`}
        </div>
        <div
          class="bg-surface-light-300 dark:bg-surface-dark-600 mx-[5px] my-[3px] h-px"
        ></div>
        <div class="px-[5px] py-1 text-[10px]">{$t`Clear History…`}</div>
      </div>
    </MockSurface>
  {/snippet}
</StepCard>

<StepCard n={2} title={$t`Go to Websites`}>
  {#snippet instruction()}
    <Trans>Select the <b>Websites</b> tab.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockSurface class="px-3.5 py-2">
      <div class="flex items-center justify-between px-2">
        <div class="flex flex-col items-center gap-[3px] text-[9px] opacity-45">
          <SettingsIcon class="size-3.5" />
          <span>{$t`General`}</span>
        </div>
        <div class="flex flex-col items-center gap-[3px] text-[9px] opacity-45">
          <ShieldIcon class="size-3.5" />
          <span>{$t`Privacy`}</span>
        </div>
        <ToolbarHighlight>
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex flex-col items-center gap-0.5 rounded-full px-3 py-1 text-[9px]"
          >
            <GlobeIcon class="size-3.5" />
            <span>{$t`Websites`}</span>
          </div>
        </ToolbarHighlight>
        <div class="flex flex-col items-center gap-[3px] text-[9px] opacity-45">
          <UserIcon class="size-3.5" />
          <span>{$t`Profiles`}</span>
        </div>
      </div>
    </MockSurface>
  {/snippet}
</StepCard>

<StepCard n={3} title={$t`Allow notifications`}>
  {#snippet instruction()}
    <Trans
      >Select <b>Notifications</b>, then set this site to <b>Allow</b>.</Trans
    >
  {/snippet}
  {#snippet mock()}
    <MockSurface class="px-3.5 pt-3 pb-3.5">
      <div class="flex gap-2.5">
        <div class="flex w-24 shrink-0 flex-col gap-0.5 text-[10px]">
          <div class="flex items-center gap-1.5 px-1.5 py-[5px] opacity-45">
            <CameraIcon class="size-2.5 shrink-0" />
            {$t`Camera`}
          </div>
          <div
            class="bg-surface-light-300 dark:bg-surface-dark-600 flex items-center gap-1.5 rounded-md px-1.5 py-[5px]"
          >
            <BellIcon class="size-2.5 shrink-0" />
            {$t`Notifications`}
          </div>
        </div>
        <div
          class="bg-surface-light-200 dark:bg-surface-dark-700 flex min-w-0 flex-1 items-center rounded-lg py-2 ps-2.5 pe-2"
        >
          <span class="flex-1 truncate text-[10px]">{host}</span>
          <ToolbarHighlight>
            <div
              class="bg-surface-light-300 dark:bg-surface-dark-600 flex h-5 items-center gap-1 rounded-full ps-2 pe-[5px] text-[10px]"
            >
              <!-- The setting the step changes, played out in place. -->
              <span class="inline-grid">
                <span class="mock-out [grid-area:1/1]">{$t`Deny`}</span>
                <span class="mock-in opacity-0 [grid-area:1/1]"
                  >{$t`Allow`}</span
                >
              </span>
              <ChevronDownIcon class="size-2.5" />
            </div>
          </ToolbarHighlight>
        </div>
      </div>
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
