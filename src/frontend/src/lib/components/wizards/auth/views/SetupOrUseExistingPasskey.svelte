<script lang="ts">
  import { fly } from "svelte/transition";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

  interface Props {
    setupNew: () => void;
    useExisting: () => Promise<void | "cancelled">;
  }

  const { setupNew, useExisting }: Props = $props();

  let isAuthenticating = $state(false);
  let isCancelled = $state(false);

  const handleUseExisting = async () => {
    isAuthenticating = true;
    const result = await useExisting();
    isAuthenticating = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(4000);
      isCancelled = false;
    }
  };
</script>

<div class="mt-4 mb-8 flex flex-col" in:fly={{ duration: 200, x: -10 }}>
  <PasskeyIllustration class="text-text-primary mb-8 h-32" />
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    {$t`Simplify your sign-in`}
  </h1>

  <p
    class="text-text-tertiary text-base font-medium text-balance sm:text-center"
  >
    <Trans>
      Create an identity with a passkey, using biometrics or a security key.
      Your data never leaves your device.
    </Trans>
  </p>
</div>
<div class="flex flex-col gap-3">
  <button
    class="btn btn-primary btn-lg"
    onclick={setupNew}
    disabled={isAuthenticating}
  >
    {$t`Create new identity`}
  </button>
  <Tooltip
    label={$t`Interaction canceled. Please try again.`}
    hidden={!isCancelled}
    manual
  >
    <button
      class="btn btn-secondary btn-lg"
      onclick={handleUseExisting}
      disabled={isAuthenticating}
    >
      {#if isAuthenticating}
        <ProgressRing />
        <span>{$t`Authenticating...`}</span>
      {:else}
        <span>{$t`Use existing identity`}</span>
      {/if}
    </button>
  </Tooltip>
</div>
