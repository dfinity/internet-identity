<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { canisterConfig } from "$lib/globals";
  import { CONTINUE_FROM_ANOTHER_DEVICE } from "$lib/state/featureFlags";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { MegaphoneIcon } from "@lucide/svelte";

  interface Props {
    setupOrUseExistingPasskey: () => void;
    continueWithGoogle: () => Promise<void>;
    continueFromAnotherDevice: () => void;
  }

  const {
    setupOrUseExistingPasskey,
    continueWithGoogle,
    continueFromAnotherDevice,
  }: Props = $props();

  let isAuthenticating = $state(false);

  const handleContinueWithGoogle = async () => {
    isAuthenticating = true;
    try {
      await continueWithGoogle();
    } finally {
      isAuthenticating = false;
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
    {/if}
    <Button
      href="/migrate"
      variant="tertiary"
      disabled={isAuthenticating}
      size="xl"
    >
      <MegaphoneIcon size="1.25rem" />
      <span>Upgrade your legacy identity</span>
    </Button>
    {#if $CONTINUE_FROM_ANOTHER_DEVICE}
      <Button
        onclick={continueFromAnotherDevice}
        variant="tertiary"
        disabled={isAuthenticating}
        size="xl"
      >
        Continue from another device
      </Button>
    {/if}
  </div>
</div>
