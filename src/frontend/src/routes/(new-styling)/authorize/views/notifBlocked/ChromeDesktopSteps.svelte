<script lang="ts">
  import { BellIcon, LockIcon, SlidersHorizontalIcon } from "@lucide/svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";
  import Toolbar from "$lib/components/ui/browserMock/Toolbar.svelte";
  import ToolbarButton from "$lib/components/ui/browserMock/ToolbarButton.svelte";
  import ToolbarHighlight from "$lib/components/ui/browserMock/ToolbarHighlight.svelte";
  import MockPanel from "$lib/components/ui/browserMock/MockPanel.svelte";
  import MockRow from "$lib/components/ui/browserMock/MockRow.svelte";
  import MockToggle from "$lib/components/ui/browserMock/MockToggle.svelte";
  import StepCard from "./StepCard.svelte";

  const { host }: { host: string } = $props();
</script>

<StepCard n={1} title={$t`Open site settings`}>
  {#snippet instruction()}
    <Trans>Select the highlighted <b>site settings</b> icon.</Trans>
  {/snippet}
  {#snippet mock()}
    <Toolbar>
      <ToolbarHighlight>
        <ToolbarButton><SlidersHorizontalIcon class="size-4" /></ToolbarButton>
      </ToolbarHighlight>
      <ToolbarButton class="mx-auto px-3 text-[8px]">{host}</ToolbarButton>
    </Toolbar>
  {/snippet}
</StepCard>

<StepCard n={2} title={$t`Allow notifications`}>
  {#snippet instruction()}
    <Trans>Turn on <b>Notifications</b>.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockPanel>
      <MockRow label={host} class="font-semibold" />
      <MockRow label={$t`Connection is secure`} muted>
        {#snippet icon()}<LockIcon class="size-3" />{/snippet}
      </MockRow>
      <MockRow label={$t`Notifications`}>
        {#snippet icon()}<BellIcon class="size-3" />{/snippet}
        {#snippet trailing()}<MockToggle animate />{/snippet}
      </MockRow>
    </MockPanel>
  {/snippet}
</StepCard>
