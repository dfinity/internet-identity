<script lang="ts">
  import ShieldIllustration from "$lib/components/illustrations/ShieldIllustration.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import Alert from "$lib/components/ui/Alert.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import { nonNullish } from "@dfinity/utils";
  import { handleError } from "$lib/components/utils/error";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { AddAccessMethodFlow } from "$lib/flows/addAccessMethodFlow.svelte";
  import type {
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import PasskeyIllustration from "$lib/components/illustrations/PasskeyIllustration.svelte";
  import { CopyIcon } from "@lucide/svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";

  interface Props {
    onGoogleLinked: (credential: OpenIdCredential) => void;
    onPasskeyRegistered: (credential: AuthnMethodData) => void;
    onClose: () => void;
    onError?: (error: unknown) => void;
    isMaxOpenIdCredentialsReached?: boolean;
    isUsingPasskeys?: boolean;
  }

  const {
    onGoogleLinked,
    onPasskeyRegistered,
    onClose,
    onError = (error) => {
      onClose();
      handleError(error);
    },
    isMaxOpenIdCredentialsReached,
    isUsingPasskeys,
  }: Props = $props();

  const addAccessMethodFlow = new AddAccessMethodFlow({
    isMaxOpenIdCredentialsReached,
  });

  const isPasskeySupported = nonNullish(window.PublicKeyCredential);

  const handleContinueWithGoogle = async () => {
    try {
      onGoogleLinked(await addAccessMethodFlow.linkGoogleAccount());
      onClose();
    } catch (error) {
      onError(error);
    }
  };
  const handleCreatePasskey = async () => {
    try {
      onPasskeyRegistered(await addAccessMethodFlow.createPasskey());
      onClose();
    } catch (error) {
      onError(error);
    }
  };
  const handleCopyLink = () =>
    navigator.clipboard.writeText(addAccessMethodFlow.newDeviceLink.href);
</script>

<Dialog {onClose}>
  {#if addAccessMethodFlow.view === "chooseMethod"}
    <div class="mt-4 mb-6 flex flex-col">
      <div class={["self-center", !isPasskeySupported && "illustration"]}>
        <ShieldIllustration class="text-text-primary mb-8 h-24" />
      </div>
      <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
        Add access method
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
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
          onclick={addAccessMethodFlow.continueWithPasskey}
          disabled={!isPasskeySupported ||
            addAccessMethodFlow.isGoogleAuthenticating}
          size="xl"
        >
          <PasskeyIcon />
          Continue with Passkey
        </Button>
        <Button
          onclick={handleContinueWithGoogle}
          variant="secondary"
          disabled={addAccessMethodFlow.isGoogleAuthenticating}
          size="xl"
        >
          {#if addAccessMethodFlow.isGoogleAuthenticating}
            <ProgressRing />
            <span>Authenticating with Google...</span>
          {:else}
            <GoogleIcon />
            <span>Continue with Google</span>
          {/if}
        </Button>
      </div>
    </div>

    {#if addAccessMethodFlow.isSystemOverlayVisible}
      <SystemOverlayBackdrop />
    {/if}
  {:else if addAccessMethodFlow.view === "addPasskey"}
    <div class="mt-4 mb-6 flex flex-col">
      <PasskeyIllustration class="text-text-primary mb-8 h-32" />
      <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
        {#if isUsingPasskeys}
          Add another passkey
        {:else}
          Add a passkey
        {/if}
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
        With passkeys, you can now use your fingerprint, face, or screen lock to
        quickly and securely confirm it’s really you.
      </p>
    </div>
    <div class="flex flex-col gap-3">
      <Button
        onclick={handleCreatePasskey}
        size="lg"
        disabled={addAccessMethodFlow.isCreatingPasskey}
      >
        {#if addAccessMethodFlow.isCreatingPasskey}
          <ProgressRing />
          <span>Creating passkey...</span>
        {:else}
          <span>Create passkey</span>
        {/if}
      </Button>
      <Button
        onclick={addAccessMethodFlow.continueOnAnotherDevice}
        variant="tertiary"
        size="lg"
        disabled={addAccessMethodFlow.isCreatingPasskey}
      >
        Continue on another device
      </Button>
    </div>
  {:else if addAccessMethodFlow.view === "continueOnAnotherDevice"}
    <div class="mt-4 mb-6 flex flex-col">
      <PasskeyIllustration class="text-text-primary mb-8 h-32" />
      <h1 class="text-text-primary mb-3 text-2xl font-medium sm:text-center">
        Continue on another device
      </h1>
      <p
        class="text-md text-text-tertiary font-medium text-balance sm:text-center"
      >
        Scan the above QR code with your <b>new device</b> or enter the URL manually.
      </p>
    </div>
    <Button onclick={handleCopyLink} variant="secondary" size="lg">
      <span>
        {addAccessMethodFlow.newDeviceLink.host +
          addAccessMethodFlow.newDeviceLink.pathname}
      </span>
      <CopyIcon size="1.25rem" />
    </Button>
  {/if}
</Dialog>

<style>
  @media (max-height: 700px) {
    /*noinspection CssUnusedSymbol*/
    .illustration {
      display: none !important;
    }
  }
</style>
