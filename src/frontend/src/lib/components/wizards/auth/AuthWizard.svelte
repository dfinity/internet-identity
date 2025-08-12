<script lang="ts">
  import { nonNullish } from "@dfinity/utils";
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import type { Snippet } from "svelte";
  import SolveCaptcha from "$lib/components/wizards/auth/views/SolveCaptcha.svelte";
  import PickAuthenticationMethod from "$lib/components/wizards/auth/views/PickAuthenticationMethod.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import SetupOrUseExistingPasskey from "$lib/components/wizards/auth/views/SetupOrUseExistingPasskey.svelte";
  import CreatePasskey from "$lib/components/wizards/auth/views/CreatePasskey.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { RegisterAccessMethodWizard } from "$lib/components/wizards/registerAccessMethod";
  import { canisterConfig } from "$lib/globals";
  import { MigrationWizard } from "$lib/components/wizards/migration";

  interface Props {
    isAuthenticating?: boolean;
    onSignIn: (identityNumber: bigint) => void;
    onSignUp: (identityNumber: bigint) => void;
    onOtherDevice?: (identityNumber: bigint) => void; // TODO: Remove once we can sign in directly
    onMigration?: (identityNumber: bigint) => void;
    onError: (error: unknown) => void;
    withinDialog?: boolean;
    children?: Snippet;
  }

  let {
    isAuthenticating = $bindable(),
    onSignIn,
    onSignUp,
    onOtherDevice = () => {},
    onMigration = () => {},
    onError,
    withinDialog = false,
    children,
  }: Props = $props();

  const authFlow = new AuthFlow();

  let isContinueFromAnotherDeviceVisible = $state(false);
  let isMigrating = $state(false);

  const handleContinueWithExistingPasskey = async () => {
    isAuthenticating = true;
    try {
      onSignIn(await authFlow.continueWithExistingPasskey());
    } catch (error) {
      onError(error);
    } finally {
      isAuthenticating = false;
    }
  };
  const handleCreatePasskey = async (name: string) => {
    isAuthenticating = true;
    try {
      onSignUp(await authFlow.createPasskey(name));
    } catch (error) {
      onError(error);
    } finally {
      isAuthenticating = false;
    }
  };
  const handleContinueWithGoogle = async () => {
    isAuthenticating = true;
    try {
      const { identityNumber, type } = await authFlow.continueWithGoogle();
      (type === "signUp" ? onSignUp : onSignIn)(identityNumber);
    } catch (error) {
      onError(error);
    } finally {
      isAuthenticating = false;
    }
  };
  const handleRegistered = async (identityNumber: bigint) => {
    if (canisterConfig.feature_flag_continue_from_another_device[0] === true) {
      onSignIn(identityNumber);
    } else {
      onOtherDevice(identityNumber);
    }
  };
</script>

{#snippet dialogContent()}
  {#if authFlow.view === "setupOrUseExistingPasskey"}
    <SetupOrUseExistingPasskey
      setupNew={authFlow.setupNewPasskey}
      useExisting={handleContinueWithExistingPasskey}
      continueFromAnotherDevice={() =>
        (isContinueFromAnotherDeviceVisible = true)}
    />
  {:else if authFlow.view === "setupNewPasskey"}
    <CreatePasskey create={handleCreatePasskey} />
  {/if}
{/snippet}

{#if isContinueFromAnotherDeviceVisible}
  <RegisterAccessMethodWizard onRegistered={handleRegistered} {onError} />
{:else if isMigrating}
  {#if !withinDialog}
    <Dialog onClose={() => (isMigrating = false)}>
      <MigrationWizard onSuccess={onMigration} />
    </Dialog>
  {:else}
    <MigrationWizard onSuccess={onMigration} />
  {/if}
{:else if nonNullish(authFlow.captcha)}
  <SolveCaptcha {...authFlow.captcha} />
{:else}
  {#if authFlow.view === "chooseMethod" || !withinDialog}
    {@render children?.()}
    <PickAuthenticationMethod
      setupOrUseExistingPasskey={authFlow.setupOrUseExistingPasskey}
      continueWithGoogle={handleContinueWithGoogle}
      migrate={() => (isMigrating = true)}
    />
  {/if}
  {#if authFlow.view !== "chooseMethod"}
    {#if !withinDialog}
      <Dialog
        onClose={authFlow.chooseMethod}
        showCloseButton={!isAuthenticating}
        closeOnOutsideClick={!isAuthenticating}
      >
        {@render dialogContent()}
      </Dialog>
    {:else}
      {@render dialogContent()}
    {/if}
  {/if}
{/if}

{#if authFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}
