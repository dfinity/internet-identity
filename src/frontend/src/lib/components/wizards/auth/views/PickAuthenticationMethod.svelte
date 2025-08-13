<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { canisterConfig } from "$lib/globals";
  import { ENABLE_MIGRATE_FLOW } from "$lib/state/featureFlags";
  import { waitFor } from "$lib/utils/utils";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";

  interface Props {
    setupOrUseExistingPasskey: () => void;
    continueWithGoogle: () => Promise<void>;
    migrate: () => void;
  }

  const { setupOrUseExistingPasskey, continueWithGoogle, migrate }: Props =
    $props();

  let isAuthenticating = $state(false);
  let isCancelled = $state(false);

  const handleContinueWithGoogle = async () => {
    isAuthenticating = true;
    try {
      await continueWithGoogle();
      isAuthenticating = false;
    } catch {
      isAuthenticating = false;
      isCancelled = true;
      await waitFor(1000);
      isCancelled = false;
    }
  };

  const supportsPasskeys = nonNullish(window.PublicKeyCredential);
  const showGoogleButton = canisterConfig.openid_google?.[0]?.[0];
</script>

<div class="flex flex-col items-stretch gap-6">
  {#if !supportsPasskeys}
    <Alert
      title="Passkeys not available here"
      description="Passkeys are unavailable on this device or browser. Please choose
        another sign-in method to continue."
    />
  {/if}
  <div class="flex flex-col items-stretch gap-3">
    <Button
      onclick={setupOrUseExistingPasskey}
      disabled={!supportsPasskeys || isAuthenticating}
      size="xl"
    >
      <PasskeyIcon />
      Continue with Passkey
    </Button>
    {#if showGoogleButton}
      <Tooltip
        label="Interaction canceled. Please try again."
        hidden={!isCancelled}
        manual
      >
        <Button
          onclick={handleContinueWithGoogle}
          variant="secondary"
          disabled={isAuthenticating}
          size="xl"
        >
          {#if isAuthenticating}
            <ProgressRing />
            <span>Authenticating with Google...</span>
          {:else}
            <GoogleIcon />
            <span>Continue with Google</span>
          {/if}
        </Button>
      </Tooltip>
    {/if}
  </div>
  {#if $ENABLE_MIGRATE_FLOW}
    <div class="flex flex-row items-center justify-between">
      <p class="text-md text-text-secondary">Still have an identity number?</p>
      <button
        onclick={migrate}
        class="text-md text-text-primary font-semibold outline-0 focus-visible:underline"
      >
        Upgrade
      </button>
    </div>
  {/if}
</div>
