<script lang="ts">
  import { handleError } from "$lib/components/utils/error";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { AddAccessMethodFlow } from "$lib/flows/addAccessMethodFlow.svelte.js";
  import type {
    AuthnMethodConfirmationError,
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import AuthorizeNewDevice from "$lib/components/views/AuthorizeNewDevice.svelte";
  import ContinueOnAnotherDevice from "$lib/components/views/ContinueOnAnotherDevice.svelte";
  import ContinueOnNewDevice from "$lib/components/views/ContinueOnNewDevice.svelte";
  import AddAccessMethod from "$lib/components/views/AddAccessMethod.svelte";
  import AddPasskey from "$lib/components/views/AddPasskey.svelte";
  import { isCanisterError } from "$lib/utils/utils";

  interface Props {
    onGoogleLinked: (credential: OpenIdCredential) => void;
    onPasskeyRegistered: (credential: AuthnMethodData) => void;
    onOtherDeviceRegistered: () => void;
    onClose: () => void;
    onError?: (error: unknown) => void;
    isMaxOpenIdCredentialsReached?: boolean;
    isUsingPasskeys?: boolean;
  }

  const {
    onGoogleLinked,
    onPasskeyRegistered,
    onOtherDeviceRegistered,
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
  const handleConfirmDevice = async (confirmationCode: string) => {
    try {
      await addAccessMethodFlow.confirmDevice(confirmationCode);
      onOtherDeviceRegistered();
      onClose();
    } catch (error) {
      if (
        isCanisterError<AuthnMethodConfirmationError>(error) &&
        error.type === "WrongCode"
      ) {
        // Handle this error in child view instead
        throw error;
      }
      onError(error);
    }
  };
  const handleClose = () => {
    addAccessMethodFlow.exitRegistrationMode();
    onClose();
  };
</script>

<Dialog onClose={handleClose}>
  {#if addAccessMethodFlow.view === "chooseMethod"}
    <AddAccessMethod
      continueWithPasskey={addAccessMethodFlow.continueWithPasskey}
      linkGoogleAccount={handleContinueWithGoogle}
    />
  {:else if addAccessMethodFlow.view === "addPasskey"}
    <AddPasskey
      createPasskey={handleCreatePasskey}
      continueOnAnotherDevice={addAccessMethodFlow.continueOnAnotherDevice}
      {isUsingPasskeys}
    />
  {:else if addAccessMethodFlow.view === "continueOnAnotherDevice"}
    <ContinueOnAnotherDevice url={addAccessMethodFlow.newDeviceLink} />
  {:else if addAccessMethodFlow.view === "confirmationCode"}
    <AuthorizeNewDevice
      confirm={handleConfirmDevice}
      restart={addAccessMethodFlow.continueOnAnotherDevice}
    />
  {:else if addAccessMethodFlow.view === "continueOnNewDevice"}
    <ContinueOnNewDevice />
  {/if}

  <!-- Rendered within dialog to be on top of it -->
  {#if addAccessMethodFlow.isSystemOverlayVisible}
    <SystemOverlayBackdrop />
  {/if}
</Dialog>
