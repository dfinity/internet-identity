<script lang="ts">
  import { TerminalIcon } from "@lucide/svelte";
  import Badge from "$lib/components/ui/Badge.svelte";
  import Toggle from "$lib/components/ui/Toggle.svelte";
  import { Trans } from "$lib/components/locale";
  import { t } from "$lib/stores/locale.store";
  import {
    cliAccessStore,
    isCliAccessEnabledStore,
  } from "$lib/stores/cli-access.store";
  import CliConfirmDialog from "./CliConfirmDialog.svelte";

  interface Props {
    identityNumber: bigint;
  }

  const { identityNumber }: Props = $props();

  const enabledStore = $derived(isCliAccessEnabledStore(identityNumber));
  const enabled = $derived($enabledStore);

  let showConfirm = $state(false);

  const handleToggle = (next: boolean) => {
    if (next) {
      showConfirm = true;
    } else {
      cliAccessStore.disable(identityNumber);
    }
  };

  const handleConfirm = () => {
    cliAccessStore.enable(identityNumber);
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
    <TerminalIcon class="size-5" />
  </span>

  <div class="flex flex-1 flex-col gap-1">
    <div class="flex min-h-[1.5rem] flex-row items-center gap-2">
      <h3 class="text-text-primary text-base font-semibold">
        {$t`CLI access`}
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
          Command-line tools on this device can ask to sign you in to apps.
        </Trans>
      {:else}
        <Trans>
          Let command-line tools on this device sign in to apps using your
          identity.
        </Trans>
      {/if}
    </p>
  </div>

  <div class="shrink-0">
    <Toggle
      checked={enabled}
      onchange={(e) => handleToggle((e.target as HTMLInputElement).checked)}
    />
  </div>
</section>

{#if showConfirm}
  <CliConfirmDialog
    onConfirm={handleConfirm}
    onClose={() => (showConfirm = false)}
  />
{/if}
