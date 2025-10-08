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
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
  import { isOpenIdCancelError } from "$lib/utils/openID";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import CreateIdentity from "$lib/components/wizards/auth/views/CreateIdentity.svelte";
  import { AUTH_FLOW_UPDATES } from "$lib/state/featureFlags";

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

  const handleContinueWithExistingPasskey = async (): Promise<
    void | "cancelled"
  > => {
    isAuthenticating = true;
    try {
      onSignIn(await authFlow.continueWithExistingPasskey());
    } catch (error) {
      if (isWebAuthnCancelError(error)) {
        isContinueFromAnotherDeviceVisible = true;
        return;
      }
      onError(error); // Propagate unhandled errors to parent component
    } finally {
      isAuthenticating = false;
    }
  };

  const handleCreatePasskey = async (
    name: string,
  ): Promise<void | "cancelled"> => {
    isAuthenticating = true;
    try {
      const result = await authFlow.submitNameAndContinue(name);
      onSignUp(result.identityNumber);
    } catch (error) {
      if (isWebAuthnCancelError(error)) {
        return "cancelled";
      }
      onError(error); // Propagate unhandled errors to parent component
    } finally {
      isAuthenticating = false;
    }
  };

  const handleContinueWithGoogle = async (): Promise<void | "cancelled"> => {
    isAuthenticating = true;
    try {
      const { identityNumber, type } = await authFlow.continueWithGoogle();
      (type === "signUp" ? onSignUp : onSignIn)(identityNumber);
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        return "cancelled";
      }
      onError(error); // Propagate unhandled errors to parent component
    } finally {
      isAuthenticating = false;
    }
  };

  const handleContinueWithOpenId = async (
    config: OpenIdConfig,
  ): Promise<void | "cancelled"> => {
    isAuthenticating = true;
    try {
      const result = await authFlow.continueWithOpenId(config);
      if (result.type === "signIn") {
        onSignIn(result.identityNumber);
      } else if (nonNullish(result.name)) {
        onSignUp(await authFlow.completeOpenIdRegistration(result.name));
      }
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        return "cancelled";
      }
      onError(error); // Propagate unhandled errors to parent component
    } finally {
      isAuthenticating = false;
    }
  };

  const handleCompleteOpenIdRegistration = async (
    name: string,
  ): Promise<void> => {
    try {
      onSignUp(await authFlow.completeOpenIdRegistration(name));
    } catch (error) {
      onError(error); // Propagate unhandled errors to parent component
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
    />
  {:else if authFlow.view === "setupNewPasskey"}
    <CreatePasskey
      create={handleCreatePasskey}
      buttonLabel={$AUTH_FLOW_UPDATES ? "Create Identity" : "Create Passkey"}
    />
  {:else if authFlow.view === "setupNewIdentity"}
    <CreateIdentity create={handleCompleteOpenIdRegistration} />
  {/if}
{/snippet}

{#if isContinueFromAnotherDeviceVisible}
  {#if !withinDialog}
    <Dialog onClose={() => (isContinueFromAnotherDeviceVisible = false)}>
      <RegisterAccessMethodWizard onRegistered={handleRegistered} {onError} />
    </Dialog>
  {:else}
    <RegisterAccessMethodWizard onRegistered={handleRegistered} {onError} />
  {/if}
{:else if isMigrating}
  {#if !withinDialog}
    <Dialog onClose={() => (isMigrating = false)}>
      <MigrationWizard onSuccess={onMigration} {onError} />
    </Dialog>
  {:else}
    <MigrationWizard onSuccess={onMigration} {onError} />
  {/if}
{:else if nonNullish(authFlow.captcha)}
  <SolveCaptcha {...authFlow.captcha} />
{:else}
  {#if authFlow.view === "chooseMethod" || !withinDialog}
    {@render children?.()}
    <PickAuthenticationMethod
      setupOrUseExistingPasskey={authFlow.setupOrUseExistingPasskey}
      continueWithGoogle={handleContinueWithGoogle}
      continueWithOpenId={handleContinueWithOpenId}
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
