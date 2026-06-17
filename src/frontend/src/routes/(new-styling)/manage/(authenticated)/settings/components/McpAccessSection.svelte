<script lang="ts">
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import McpLogo from "$lib/components/ui/McpLogo.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import {
    mcpAccessStore,
    isMcpAccessEnabledStore,
  } from "$lib/stores/mcp-access.store";
  import McpConfirmDialog from "./McpConfirmDialog.svelte";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();
  const titleId = $props.id();

  const enabledStore = $derived(isMcpAccessEnabledStore(identityNumber));
  const enabled = $derived($enabledStore);

  let showConfirm = $state(false);

  const handleToggle = (event: Event) => {
    if (!(event.currentTarget instanceof HTMLInputElement)) {
      return;
    }
    if (event.currentTarget.checked) {
      showConfirm = true;
    } else {
      mcpAccessStore.disable(identityNumber);
    }
  };

  const handleConfirm = () => {
    mcpAccessStore.enable(identityNumber);
    showConfirm = false;
  };
</script>

<section
  class="border-border-secondary bg-bg-secondary flex flex-row items-start gap-4 rounded-xl border p-5"
>
  <span
    class="border-border-tertiary text-fg-secondary bg-bg-primary flex size-10 shrink-0 items-center justify-center rounded-lg border"
    aria-hidden="true"
  >
    <McpLogo class="size-5" />
  </span>

  <div class="flex flex-1 flex-col gap-1">
    <div class="flex min-h-[1.5rem] flex-row items-center gap-2">
      <h3 id={titleId} class="text-text-primary text-base font-semibold">
        {$t`MCP access`}
      </h3>
      {#if enabled}
        <Badge color="success" size="sm" dot>
          {$t`Enabled`}
        </Badge>
      {/if}
    </div>
    <p class="text-text-tertiary text-sm">
      {#if enabled}
        <Trans>
          AI assistants on this device can ask to sign you in to apps.
        </Trans>
      {:else}
        <Trans>
          Let AI assistants on this device sign in to apps using your identity.
        </Trans>
      {/if}
    </p>
  </div>

  <div class="shrink-0">
    <Toggle
      checked={enabled}
      onchange={handleToggle}
      aria-labelledby={titleId}
    />
  </div>
</section>

{#if showConfirm}
  <McpConfirmDialog
    onConfirm={handleConfirm}
    onClose={() => (showConfirm = false)}
  />
{/if}
