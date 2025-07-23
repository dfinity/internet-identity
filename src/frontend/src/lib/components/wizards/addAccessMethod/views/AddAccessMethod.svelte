<script lang="ts">
  import ShieldIllustration from "$lib/components/illustrations/ShieldIllustration.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import { nonNullish } from "@dfinity/utils";

  interface Props {
    linkGoogleAccount: () => Promise<void>;
    continueWithPasskey: () => void;
  }

  const { linkGoogleAccount, continueWithPasskey }: Props = $props();

  let authenticating = $state(false);

  const isPasskeySupported = nonNullish(window.PublicKeyCredential);

  const handleContinueWithGoogle = async () => {
    authenticating = true;
    try {
      await linkGoogleAccount();
    } finally {
      authenticating = false;
    }
  };
</script>

<div class="mt-4 mb-6 flex flex-col">
  <div class={["self-center", !isPasskeySupported && "illustration"]}>
    <ShieldIllustration class="text-text-primary mb-8 h-24" />
  </div>
  <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
    Add access method
  </h1>
  <p class="text-md text-text-tertiary font-medium text-balance sm:text-center">
    Add another way to sign in with a passkey or Google account for secure
    access.
  </p>
</div>
<div class="flex flex-col items-stretch gap-6">
  {#if !isPasskeySupported}
    <Alert
      title="Passkeys not available here"
      description="Passkeys are unavailable on this device or browser. Please choose
        another access method to continue."
      direction="horizontal"
    />
  {/if}
  <div class="flex flex-col items-stretch gap-3">
    <Button
      onclick={continueWithPasskey}
      disabled={!isPasskeySupported || authenticating}
      size="xl"
    >
      <PasskeyIcon />
      Continue with Passkey
    </Button>
    <Button
      onclick={handleContinueWithGoogle}
      variant="secondary"
      disabled={authenticating}
      size="xl"
    >
      {#if authenticating}
        <ProgressRing />
        <span>Authenticating with Google...</span>
      {:else}
        <GoogleIcon />
        <span>Continue with Google</span>
      {/if}
    </Button>
  </div>
</div>

<style>
  @media (max-height: 700px) {
    /*noinspection CssUnusedSymbol*/
    .illustration {
      display: none !important;
    }
  }
</style>
