<script lang="ts">
  import { RegisterAccessMethodFlow } from "$lib/flows/registerAccessMethodFlow.svelte";
  import ConfirmThisDevice from "$lib/components/wizards/registerAccessMethod/views/ConfirmThisDevice.svelte";
  import ConfirmYourSignIn from "$lib/components/wizards/registerAccessMethod/views/ConfirmYourSignIn.svelte";
  import { onMount } from "svelte";
  import ContinueFromExistingDevice from "$lib/components/wizards/registerAccessMethod/views/ContinueFromExistingDevice.svelte";
  import WaitingForExistingDevice from "$lib/components/wizards/registerAccessMethod/views/WaitingForExistingDevice.svelte";
  import { canisterConfig } from "$lib/globals";

  interface Props {
    registrationId?: string;
    onRegistered: (identityNumber: bigint) => void;
    onError: (error: unknown) => void;
  }

  let { registrationId, onRegistered, onError }: Props = $props();

  const registerAccessMethodFlow = new RegisterAccessMethodFlow(
    canisterConfig.feature_flag_continue_from_another_device[0] === true,
  );

  const handleCreatePasskey = async () => {
    try {
      onRegistered(await registerAccessMethodFlow.createPasskey());
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
