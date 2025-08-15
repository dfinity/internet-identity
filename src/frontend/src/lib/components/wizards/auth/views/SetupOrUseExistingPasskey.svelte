<script lang="ts">
  import { fly } from "svelte/transition";
  import Button from "$lib/components/ui/Button.svelte";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { CONTINUE_FROM_ANOTHER_DEVICE } from "$lib/state/featureFlags";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";

  interface Props {
    setupNew: () => void;
    useExisting: () => Promise<void | "cancelled">;
    continueFromAnotherDevice: () => void;
  }

  const { setupNew, useExisting, continueFromAnotherDevice }: Props = $props();

  let isAuthenticating = $state(false);
  let isCancelled = $state(false);

  const handleUseExisting = async () => {
    isAuthenticating = true;
    const result = await useExisting();
    isAuthenticating = false;

    if (result === "cancelled") {
      isCancelled = true;
      await waitFor(2000);
      isCancelled = false;
    }
  };
</script>

<div class="mt-4 mb-8 flex flex-col" in:fly={{ duration: 200, x: -10 }}>
  <PasskeyIllustration class="text-text-primary mb-8 h-32" />
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    Simplify your sign-in
  </h1>
  <p class="text-md text-text-tertiary font-medium text-balance sm:text-center">
    With passkeys, you can now use your fingerprint, face, or screen lock to
    quickly and securely confirm it’s really you.
  </p>
</div>
<div class="flex flex-col gap-3">
  <Button onclick={setupNew} size="lg" disabled={isAuthenticating}>
    Set up a new Passkey
  </Button>
  <Tooltip
    label="Interaction canceled. Please try again."
    hidden={!isCancelled}
    manual
  >
    <Button
      onclick={handleUseExisting}
      variant="secondary"
      size="lg"
      disabled={isAuthenticating}
    >
      {#if isAuthenticating}
        <ProgressRing />
        <span>Authenticating...</span>
      {:else}
        <span>Use an existing Passkey</span>
      {/if}
    </Button>
  </Tooltip>
  {#if $CONTINUE_FROM_ANOTHER_DEVICE}
    <Button
      onclick={continueFromAnotherDevice}
      variant="tertiary"
      disabled={isAuthenticating}
      size="lg"
    >
      Continue from another device
    </Button>
  {/if}
</div>
