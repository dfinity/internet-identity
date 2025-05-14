<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";

  interface Props {
    setupOrUseExistingPasskey: () => void;
    continueWithGoogle: () => void;
  }

  const { setupOrUseExistingPasskey, continueWithGoogle }: Props = $props();

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
      disabled={!supportsPasskeys}
      size="xl"
    >
      <PasskeyIcon />
      Continue with Passkey
    </Button>
    <Button onclick={continueWithGoogle} variant="secondary" size="xl">
      <GoogleIcon />
      Continue with Google
    </Button>
  </div>
</div>
