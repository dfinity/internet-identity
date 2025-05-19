<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";

  interface Props {
    setupOrUseExistingPasskey: () => void;
    continueWithGoogle: () => Promise<void>;
  }

  const { setupOrUseExistingPasskey, continueWithGoogle }: Props = $props();

  let googleLoading = $state(false);

  const handleContinueWithGoogle = async () => {
    googleLoading = true;
    try {
      await continueWithGoogle();
    } finally {
      googleLoading = false;
    }
  };

  const supportsPasskeys = nonNullish(window.PublicKeyCredential);
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
    <Button
      onclick={handleContinueWithGoogle}
      variant="secondary"
      size="xl"
      disabled={googleLoading}
    >
      {#if googleLoading}
        <ProgressRing />
        <span>Authenticating with Google...</span>
      {:else}
        <GoogleIcon />
        <span>Continue with Google</span>
      {/if}
    </Button>
  </div>
</div>
