<script lang="ts">
  import { RegisterAccessMethodFlow } from "$lib/flows/registerAccessMethodFlow.svelte";
  import ConfirmThisDevice from "$lib/components/wizards/registerAccessMethod/views/ConfirmThisDevice.svelte";
  import ConfirmYourSignIn from "$lib/components/wizards/registerAccessMethod/views/ConfirmYourSignIn.svelte";
  import { onMount } from "svelte";
  import ContinueFromExistingDevice from "$lib/components/wizards/registerAccessMethod/views/ContinueFromExistingDevice.svelte";
  import WaitingForExistingDevice from "$lib/components/wizards/registerAccessMethod/views/WaitingForExistingDevice.svelte";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";

  interface Props {
    registrationId?: string;
    onRegistered: (identityNumber: bigint) => void;
    onError: (error: unknown) => void;
  }

  let { registrationId, onRegistered, onError }: Props = $props();

  const registerAccessMethodFlow = new RegisterAccessMethodFlow();

  const handleCreatePasskey = async () => {
    try {
      onRegistered(await registerAccessMethodFlow.createPasskey());
    } catch (error) {
      // Cancelling the WebAuthn prompt must not tear down the whole dialog.
      // Stay on the confirm view so the user can retry "Create passkey" (or
      // dismiss the modal to return to the picker). Only real errors — which
      // the parent surfaces by closing the dialog — propagate.
      if (isWebAuthnCancelError(error)) {
        return;
      }
      onError(error);
    }
  };

  onMount(async () => {
    try {
      await registerAccessMethodFlow.waitForExistingDevice(registrationId);
    } catch (error) {
      onError(error);
    }
  });
</script>

{#if registerAccessMethodFlow.view === "continueFromExistingDevice"}
  {#if registerAccessMethodFlow.existingDeviceLink}
    <ContinueFromExistingDevice
      url={registerAccessMethodFlow.existingDeviceLink}
    />
  {:else}
    <WaitingForExistingDevice />
  {/if}
{:else if registerAccessMethodFlow.view === "confirmDevice"}
  <ConfirmThisDevice
    confirmationCode={registerAccessMethodFlow.confirmationCode}
  />
{:else if registerAccessMethodFlow.view === "confirmSignIn"}
  <ConfirmYourSignIn
    name={registerAccessMethodFlow.identityName}
    createPasskey={handleCreatePasskey}
  />
{/if}
