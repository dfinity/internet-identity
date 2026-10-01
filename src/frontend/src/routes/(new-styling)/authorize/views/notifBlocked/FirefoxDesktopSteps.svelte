<script lang="ts">
  import { BellOffIcon, ShieldIcon, XIcon } from "@lucide/svelte";
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

<StepCard n={1} title={$t`Open site permissions`}>
  {#snippet instruction()}
    <Trans>Select the highlighted <b>permissions</b> icon.</Trans>
  {/snippet}
  {#snippet mock()}
    <Toolbar>
      <ToolbarButton><ShieldIcon class="size-4" /></ToolbarButton>
      <ToolbarHighlight>
        <ToolbarButton><BellOffIcon class="size-4" /></ToolbarButton>
      </ToolbarHighlight>
      <ToolbarButton class="mx-auto px-3 text-[8px]">{host}</ToolbarButton>
    </Toolbar>
  {/snippet}
</StepCard>

<StepCard n={2} title={$t`Clear the block`}>
  {#snippet instruction()}
    <Trans>Select <b>Blocked ×</b> to clear it.</Trans>
  {/snippet}
  {#snippet mock()}
    <MockPanel>
      <MockRow label={$t`Permissions`} class="font-semibold" muted />
      <MockRow label={$t`Send Notifications`}>
        {#snippet icon()}<BellOffIcon class="size-3" />{/snippet}
        {#snippet trailing()}
          <span
            class="text-text-secondary border-border-tertiary flex items-center gap-1 rounded-md border px-1.5 py-0.5"
          >
            {$t`Blocked`}
            <XIcon class="size-2.5" />
          </span>
        {/snippet}
      </MockRow>
    </MockPanel>
  {/snippet}
</StepCard>
