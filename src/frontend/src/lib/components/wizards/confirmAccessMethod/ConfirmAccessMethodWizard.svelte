<script lang="ts">
  import { onMount } from "svelte";
  import WaitingForNewDevice from "$lib/components/wizards/confirmAccessMethod/views/WaitingForNewDevice.svelte";
  import { isCanisterError } from "$lib/utils/utils";
  import type { AuthnMethodConfirmationError } from "$lib/generated/internet_identity_types";
  import ContinueOnNewDevice from "$lib/components/wizards/confirmAccessMethod/views/ContinueOnNewDevice.svelte";
  import EnterConfirmationCode from "$lib/components/wizards/confirmAccessMethod/views/EnterConfirmationCode.svelte";
  import FinishOnNewDevice from "$lib/components/wizards/confirmAccessMethod/views/FinishOnNewDevice.svelte";
  import { ConfirmAccessMethodFlow } from "$lib/flows/confirmAccessMethodFlow.svelte";

  interface Props {
    registrationId?: string;
    onConfirm: () => void;
    onRestart?: () => void;
    onError: (error: unknown) => void;
  }

  const { registrationId, onConfirm, onRestart, onError }: Props = $props();

  const confirmAccessMethodFlow = new ConfirmAccessMethodFlow();

  const handleEnterRegistrationMode = async () => {
    try {
      await confirmAccessMethodFlow.enterRegistrationMode(registrationId);
    } catch (error) {
      onError(error);
    }
  };
  // Always generate a fresh registrationId on user-initiated restart,
  // even if the wizard was mounted with one (e.g. via /activate#<id>).
  // Reusing the same id leaves #newDeviceLink unset and traps the dialog
  // in the WaitingForNewDevice view.
  const handleRestart = async () => {
    onRestart?.();
    try {
      await confirmAccessMethodFlow.enterRegistrationMode();
    } catch (error) {
      onError(error);
    }
  };
  const handleConfirmDevice = async (confirmationCode: string) => {
    try {
      await confirmAccessMethodFlow.confirmDevice(confirmationCode);
      onConfirm();
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

  onMount(() => {
    void handleEnterRegistrationMode();
    return () => void confirmAccessMethodFlow.exitRegistrationMode();
  });
</script>

{#if confirmAccessMethodFlow.view === "continueOnNewDevice"}
  {#if confirmAccessMethodFlow.newDeviceLink}
    <ContinueOnNewDevice url={confirmAccessMethodFlow.newDeviceLink} />
  {:else}
    <WaitingForNewDevice />
  {/if}
{:else if confirmAccessMethodFlow.view === "enterConfirmationCode"}
  <EnterConfirmationCode
    confirm={handleConfirmDevice}
    restart={handleRestart}
  />
{:else if confirmAccessMethodFlow.view === "finishOnNewDevice"}
  <FinishOnNewDevice />
{/if}
