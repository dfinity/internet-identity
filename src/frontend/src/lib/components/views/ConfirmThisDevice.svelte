<script lang="ts">
  import ConfirmDeviceIllustration from "$lib/components/illustrations/ConfirmDeviceIllustration.svelte";
  import { isNullish, nonNullish } from "@dfinity/utils";
  import { CopyIcon } from "@lucide/svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";

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
  Confirm this device
</h1>
<p
  class="text-md text-text-tertiary mb-8 font-medium sm:text-center sm:text-balance"
>
  To confirm it's you, enter below code on your
  <b class="text-text-primary">existing device</b>.
</p>

<Tooltip label="Code copied to clipboard" hidden={!copied} arrow={false} manual>
  <Button
    onclick={handleCopyCode}
    variant="secondary"
    size="xl"
    disabled={isNullish(confirmationCode)}
    aria-label="Confirmation code"
  >
    {#if nonNullish(confirmationCode)}
      <span>
        {confirmationCode}
      </span>
      <CopyIcon size="1.25rem" />
    {:else}
      <ProgressRing />
      <span>Generating code...</span>
    {/if}
  </Button>
</Tooltip>
