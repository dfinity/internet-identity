<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { canisterConfig } from "$lib/globals";
  import { handleError } from "$lib/components/utils/error";

  interface Props {
    setupOrUseExistingPasskey: () => void;
    continueWithGoogle: () => Promise<void>;
    onError?: (error: unknown) => void;
  }

  const {
    setupOrUseExistingPasskey,
    continueWithGoogle,
    onError = handleError,
  }: Props = $props();

  let googleLoading = $state(false);

  const handleContinueWithGoogle = async () => {
    googleLoading = true;
    try {
      await continueWithGoogle();
    } catch (error) {
      onError(error);
    } finally {
      googleLoading = false;
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
      disabled={!supportsPasskeys || googleLoading}
      size="xl"
    >
      <PasskeyIcon />
      Continue with Passkey
    </Button>
    {#if showGoogleButton}
      <Button
        onclick={handleContinueWithGoogle}
        variant="secondary"
        disabled={googleLoading}
        size="xl"
      >
        {#if googleLoading}
          <ProgressRing />
          <span>Authenticating with Google...</span>
        {:else}
          <GoogleIcon />
          <span>Continue with Google</span>
        {/if}
      </Button>
    {/if}
  </div>
</div>
