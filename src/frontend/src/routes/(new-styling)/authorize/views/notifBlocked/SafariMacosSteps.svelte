<script lang="ts">
  import {
    BellIcon,
    CameraIcon,
    GlobeIcon,
    SettingsIcon,
    ShieldIcon,
  } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import MockPanel from "$lib/components/ui/browserMock/MockPanel.svelte";
  import MockRow from "$lib/components/ui/browserMock/MockRow.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import StepCard from "./StepCard.svelte";

  const { host }: { host: string } = $props();
</script>

<StepCard n={1} title={$t`Open Safari settings`}>
  {#snippet instruction()}
    <Trans>Select <b>Safari</b> then <b>Settings…</b> in the menu bar.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockPanel class="gap-1">
      <div
        class="text-text-primary flex flex-row items-center gap-3 px-2 pb-1 opacity-60"
      >
        <span class="font-semibold">{$t`Safari`}</span>
        <span>{$t`File`}</span>
        <span>{$t`Edit`}</span>
        <span>{$t`View`}</span>
      </div>
      <MockRow label={$t`About Safari`} muted />
      <ToolbarHighlight class="rounded-md">
        <MockRow label={$t`Settings…`}>
          {#snippet trailing()}
            <span class="text-text-tertiary">⌘ ,</span>
          {/snippet}
        </MockRow>
      </ToolbarHighlight>
      <MockRow label={$t`Clear History…`} muted />
    </MockPanel>
  {/snippet}
</StepCard>

<StepCard n={2} title={$t`Go to Websites`}>
  {#snippet instruction()}
    <Trans>Select the <b>Websites</b> tab.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockPanel>
      <div class="flex flex-row items-center gap-1">
        <MockRow label={$t`General`} muted>
          {#snippet icon()}<SettingsIcon class="size-3" />{/snippet}
        </MockRow>
        <MockRow label={$t`Privacy`} muted>
          {#snippet icon()}<ShieldIcon class="size-3" />{/snippet}
        </MockRow>
        <ToolbarHighlight class="rounded-md">
          <MockRow label={$t`Websites`}>
            {#snippet icon()}<GlobeIcon class="size-3" />{/snippet}
          </MockRow>
        </ToolbarHighlight>
      </div>
    </MockPanel>
  {/snippet}
</StepCard>

<StepCard n={3} title={$t`Allow notifications`}>
  {#snippet instruction()}
    <Trans
      >Select <b>Notifications</b>, then set this site to <b>Allow</b>.</Trans
    >
  {/snippet}
  {#snippet mock()}
    <MockPanel class="flex-row gap-2">
      <div class="flex w-1/3 flex-col">
        <MockRow label={$t`Camera`} muted>
          {#snippet icon()}<CameraIcon class="size-3" />{/snippet}
        </MockRow>
        <MockRow label={$t`Notifications`}>
          {#snippet icon()}<BellIcon class="size-3" />{/snippet}
        </MockRow>
      </div>
      <div class="flex flex-1 flex-col justify-center">
        <MockRow label={host}>
          {#snippet trailing()}
            <ToolbarHighlight class="rounded-md">
              <span
                class="border-border-tertiary flex overflow-hidden rounded-md border"
              >
                <span class="text-text-tertiary px-1.5 py-0.5">{$t`Deny`}</span>
                <span
                  class="bg-bg-brand-solid text-text-primary-inversed px-1.5 py-0.5"
                >
                  {$t`Allow`}
                </span>
              </span>
            </ToolbarHighlight>
          {/snippet}
        </MockRow>
      </div>
    </MockPanel>
  {/snippet}
</StepCard>
