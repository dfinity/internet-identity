<script lang="ts">
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    createPasskey: () => Promise<"cancelled" | void>;
    continueOnAnotherDevice: () => void;
    isUsingPasskeys?: boolean;
  }

  const { createPasskey, continueOnAnotherDevice, isUsingPasskeys }: Props =
    $props();

  let isCreatingPasskey = $state(false);
  let isCancelled = $state(false);

  const handleCreatePasskey = async () => {
    isCreatingPasskey = true;
    const result = await createPasskey();
    isCreatingPasskey = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(4000);
      isCancelled = false;
    }
  };
</script>

<div class="mt-4 mb-6 flex flex-col">
  <PasskeyIllustration class="text-text-primary mb-8 h-32" />
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    {#if isUsingPasskeys}
      {$t`Add another passkey`}
    {:else}
      {$t`Add a passkey`}
    {/if}
  </h1>
  <p
    class="text-text-tertiary text-base font-medium text-balance sm:text-center"
  >
    <Trans>
      With passkeys, you can now use your fingerprint, face, or screen lock to
      quickly and securely confirm it’s really you.
    </Trans>
  </p>
</div>
<div class="flex flex-col gap-3">
  <Tooltip
    label={$t`Interaction canceled. Please try again.`}
    hidden={!isCancelled}
    manual
  >
    <Button
      onclick={handleCreatePasskey}
      size="lg"
      disabled={isCreatingPasskey}
    >
      {#if isCreatingPasskey}
        <ProgressRing />
        <span>{$t`Creating passkey...`}</span>
      {:else}
        <span>{$t`Create passkey`}</span>
      {/if}
    </Button>
  </Tooltip>
  <Button
    onclick={continueOnAnotherDevice}
    variant="tertiary"
    size="lg"
    disabled={isCreatingPasskey}
  >
    {$t`Continue on another device`}
  </Button>
</div>
