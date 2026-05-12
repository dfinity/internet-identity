<script lang="ts">
  import ConfirmDeviceIllustration from "$lib/components/illustrations/ConfirmDeviceIllustration.svelte";
  import { CopyIcon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    confirmationCode?: string;
  }

  const { confirmationCode }: Props = $props();

  let copied = $state(false);

  const handleCopyCode = async () => {
    if (confirmationCode === undefined) {
      return;
    }
    await navigator.clipboard.writeText(confirmationCode);
    copied = true;
    await waitFor(700);
    copied = false;
  };
</script>

<ConfirmDeviceIllustration class="text-text-primary mt-4 mb-8 h-32" />
<h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
  {$t`Confirm this device`}
</h1>
<p
  class="text-text-tertiary mb-8 text-base font-medium sm:text-center sm:text-balance"
>
  <Trans>
    To confirm it's you, enter below code on your
    <b class="text-text-primary">existing device</b>.
  </Trans>
</p>

<Tooltip label={$t`Code copied to clipboard`} hidden={!copied} manual>
  <button
    class="btn btn-secondary btn-xl"
    onclick={handleCopyCode}
    disabled={confirmationCode === undefined}
    aria-label={$t`Confirmation code`}
  >
    {#if confirmationCode !== undefined}
      <span>
        {confirmationCode}
      </span>
      <CopyIcon class="size-5" />
    {:else}
      <ProgressRing />
      <span>{$t`Generating code...`}</span>
    {/if}
  </button>
</Tooltip>
