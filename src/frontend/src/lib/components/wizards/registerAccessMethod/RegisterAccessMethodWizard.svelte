<script lang="ts">
  import { RegisterAccessMethodFlow } from "$lib/flows/registerAccessMethodFlow.svelte";
  import ConfirmThisDevice from "$lib/components/wizards/registerAccessMethod/views/ConfirmThisDevice.svelte";
  import ConfirmYourSignIn from "$lib/components/wizards/registerAccessMethod/views/ConfirmYourSignIn.svelte";
  import { onMount } from "svelte";
  import ContinueOnExistingDevice from "$lib/components/wizards/registerAccessMethod/views/ContinueOnExistingDevice.svelte";
  import WaitingForExistingDevice from "$lib/components/wizards/registerAccessMethod/views/WaitingForExistingDevice.svelte";

  interface Props {
    registrationId?: string;
    onRegistered: () => void;
    onError: (error: unknown) => void;
  }

  let { registrationId, onRegistered, onError }: Props = $props();

  const registerAccessMethodFlow = new RegisterAccessMethodFlow();

  const handleCreatePasskey = async () => {
    try {
      await registerAccessMethodFlow.createPasskey();
      onRegistered();
    } catch (error) {
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

{#if registerAccessMethodFlow.view === "continueOnExistingDevice"}
  {#if registerAccessMethodFlow.existingDeviceLink}
    <ContinueOnExistingDevice
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
