<script lang="ts">
  import { BellOffIcon, ShieldIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Toolbar from "$lib/components/ui/browserMock/Toolbar.svelte";
  import ToolbarButton from "$lib/components/ui/browserMock/ToolbarButton.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import MockPanel from "$lib/components/ui/browserMock/MockPanel.svelte";
  import MockRow from "$lib/components/ui/browserMock/MockRow.svelte";
  import StepCard from "./StepCard.svelte";

  const { host }: { host: string } = $props();
</script>

<StepCard n={1} title={$t`Open site settings`}>
  {#snippet instruction()}
    <Trans>Tap the highlighted <b>shield</b> icon.</Trans>
  {/snippet}
  {#snippet mock()}
    <!-- Firefox on Android keeps the address bar at the bottom of the screen. -->
    <Toolbar>
      <ToolbarHighlight>
        <ToolbarButton><ShieldIcon class="size-4" /></ToolbarButton>
      </ToolbarHighlight>
      <ToolbarButton class="mx-auto px-3 text-[8px]">{host}</ToolbarButton>
    </Toolbar>
  {/snippet}
</StepCard>

<StepCard n={2} title={$t`Allow notifications`}>
  {#snippet instruction()}
    <Trans>Tap <b>Blocked</b> to allow.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockPanel>
      <MockRow label={host} class="font-semibold" muted />
      <MockRow label={$t`Notifications`}>
        {#snippet icon()}<BellOffIcon class="size-3" />{/snippet}
        {#snippet trailing()}
          <ToolbarHighlight class="rounded-md">
            <span
              class="text-text-secondary border-border-tertiary rounded-md border px-1.5 py-0.5"
            >
              {$t`Blocked`}
            </span>
          </ToolbarHighlight>
        {/snippet}
      </MockRow>
    </MockPanel>
  {/snippet}
</StepCard>
