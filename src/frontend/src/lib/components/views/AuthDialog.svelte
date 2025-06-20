<script lang="ts">
  import PickAuthenticationMethod from "$lib/components/views/PickAuthenticationMethod.svelte";
  import { nonNullish } from "@dfinity/utils";
  import SolveCaptcha from "$lib/components/views/SolveCaptcha.svelte";
  import SetupOrUseExistingPasskey from "$lib/components/views/SetupOrUseExistingPasskey.svelte";
  import CreatePasskey from "$lib/components/views/CreatePasskey.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { AuthFlow, type AuthFlowOptions } from "$lib/flows/authFlow.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { handleError } from "$lib/components/utils/error";

  interface Props extends AuthFlowOptions {
    title: string;
    subtitle: string;
    onClose: () => void;
    onError?: (error: unknown) => void;
  }

  const {
    title,
    subtitle,
    onClose,
    onError = (error) => {
      onClose();
      handleError(error);
    },
    onSignIn,
    onSignUp,
  }: Props = $props();

  const authFlow = new AuthFlow({ onSignIn, onSignUp });
</script>

<Dialog {onClose}>
  {#if nonNullish(authFlow.captcha)}
    <SolveCaptcha {...authFlow.captcha} />
  {:else if authFlow.view === "chooseMethod"}
    <h1 class="text-text-primary mb-2 text-2xl font-medium">
      {title}
    </h1>
    <p class="text-text-secondary mb-6 self-start text-sm">{subtitle}</p>
    <PickAuthenticationMethod
      setupOrUseExistingPasskey={authFlow.setupOrUseExistingPasskey}
      continueWithGoogle={authFlow.continueWithGoogle}
      {onError}
    />
  {:else if authFlow.view === "setupOrUseExistingPasskey"}
    <SetupOrUseExistingPasskey
      setupNew={authFlow.setupNewPasskey}
      useExisting={authFlow.continueWithExistingPasskey}
      {onError}
    />
  {:else if authFlow.view === "setupNewPasskey"}
    <CreatePasskey create={authFlow.createPasskey} {onError} />
  {/if}
  {#if authFlow.systemOverlay}
    <SystemOverlayBackdrop />
  {/if}
</Dialog>
