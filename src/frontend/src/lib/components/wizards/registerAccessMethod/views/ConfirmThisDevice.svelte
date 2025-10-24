<script lang="ts">
  import ConfirmDeviceIllustration from "$lib/components/illustrations/ConfirmDeviceIllustration.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { CopyIcon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Button from "$lib/components/ui/Button.svelte";
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
    if (isNullish(confirmationCode)) {
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
  <Button
    onclick={handleCopyCode}
    variant="secondary"
    size="xl"
    disabled={isNullish(confirmationCode)}
    aria-label={$t`Confirmation code`}
  >
    {#if nonNullish(confirmationCode)}
      <span>
        {confirmationCode}
      </span>
      <CopyIcon class="size-5" />
    {:else}
      <ProgressRing />
      <span>{$t`Generating code...`}</span>
    {/if}
  </Button>
</Tooltip>
