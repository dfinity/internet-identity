<script lang="ts">
  import { handleError } from "$lib/components/utils/error";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { AddAccessMethodFlow } from "$lib/flows/addAccessMethodFlow.svelte.js";
  import type {
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import AddAccessMethod from "$lib/components/wizards/addAccessMethod/views/AddAccessMethod.svelte";
  import AddPasskey from "$lib/components/wizards/addAccessMethod/views/AddPasskey.svelte";
  import { ConfirmAccessMethodWizard } from "$lib/components/wizards/confirmAccessMethod";

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

  let isContinueOnAnotherDeviceVisible = $state(false);

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
  const handleOtherDeviceRegistered = async () => {
    onOtherDeviceRegistered();
    onClose();
  };
</script>

<Dialog {onClose}>
  {#if isContinueOnAnotherDeviceVisible}
    <ConfirmAccessMethodWizard
      onConfirm={handleOtherDeviceRegistered}
      {onError}
    />
  {:else if addAccessMethodFlow.view === "chooseMethod"}
    <AddAccessMethod
      continueWithPasskey={addAccessMethodFlow.continueWithPasskey}
      linkGoogleAccount={handleContinueWithGoogle}
    />
  {:else if addAccessMethodFlow.view === "addPasskey"}
    <AddPasskey
      createPasskey={handleCreatePasskey}
      continueOnAnotherDevice={() => (isContinueOnAnotherDeviceVisible = true)}
      {isUsingPasskeys}
    />
  {/if}

  <!-- Rendered within dialog to be on top of it -->
  {#if addAccessMethodFlow.isSystemOverlayVisible}
    <SystemOverlayBackdrop />
  {/if}
</Dialog>
