<script lang="ts">
  import PickAuthenticationMethod from "$lib/components/views/PickAuthenticationMethod.svelte";
  import { nonNullish } from "@dfinity/utils";
  import SolveCaptcha from "$lib/components/views/SolveCaptcha.svelte";
  import SetupOrUseExistingPasskey from "$lib/components/views/SetupOrUseExistingPasskey.svelte";
  import CreatePasskey from "$lib/components/views/CreatePasskey.svelte";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";

  interface Props {
    onSuccess: (identityNumber: bigint) => void;
    onCancel: () => void;
  }

  const { onCancel, onSuccess }: Props = $props();

  const authFlow = new AuthFlow({
    onSignIn: onSuccess,
    onSignUp: (identityNumber) => {
      toaster.success({
        title: "You're all set. Your identity has been created.",
        duration: 4000,
        closable: false,
      });
      onSuccess(identityNumber);
    },
    onError: (error) => {
      handleError(error);
      onCancel();
    },
  });
</script>

{#if nonNullish(authFlow.captcha)}
  <SolveCaptcha {...authFlow.captcha} />
{:else if authFlow.view === "chooseMethod"}
  <h1 class="text-text-primary mb-2 text-2xl font-medium">
    Use another identity
  </h1>
  <p class="text-text-secondary mb-6 self-start text-sm">Choose method</p>
  <PickAuthenticationMethod
    setupOrUseExistingPasskey={authFlow.setupOrUseExistingPasskey}
    continueWithGoogle={authFlow.continueWithGoogle}
  />
{:else if authFlow.view === "setupOrUseExistingPasskey"}
  <SetupOrUseExistingPasskey
    setupNew={authFlow.setupNewPasskey}
    useExisting={authFlow.continueWithExistingPasskey}
  />
{:else if authFlow.view === "setupNewPasskey"}
  <CreatePasskey create={authFlow.createPasskey} />
{/if}

{#if authFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}
