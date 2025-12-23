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
  import { MigrationWizard } from "$lib/components/wizards/migration";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
  import { isOpenIdCancelError } from "$lib/utils/openID";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import CreateIdentity from "$lib/components/wizards/auth/views/CreateIdentity.svelte";

  interface Props {
    onSignIn: (identityNumber: bigint) => Promise<void>;
    onSignUp: (identityNumber: bigint) => Promise<void>;
    onUpgrade: (identityNumber: bigint) => Promise<void>;
    onError: (error: unknown) => void;
    withinDialog?: boolean;
    children?: Snippet;
  }

  let {
    onSignIn,
    onSignUp,
    onUpgrade,
    onError,
    withinDialog = false,
    children,
  }: Props = $props();

  const authFlow = new AuthFlow();

  let isContinueFromAnotherDeviceVisible = $state(false);
  let isAuthenticating = $state(false);
  let isUpgrading = $state(false);

  const handleContinueWithExistingPasskey = async (): Promise<
    void | "cancelled"
  > => {
    try {
      isAuthenticating = true;
      await onSignIn(await authFlow.continueWithExistingPasskey());
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
    try {
      isAuthenticating = true;
      const result = await authFlow.submitNameAndContinue(name);
      await onSignUp(result.identityNumber);
    } catch (error) {
      if (isWebAuthnCancelError(error)) {
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
    try {
      isAuthenticating = true;
      const result = await authFlow.continueWithOpenId(config);
      if (result.type === "signIn") {
        await onSignIn(result.identityNumber);
      } else if (nonNullish(result.name)) {
        await onSignUp(await authFlow.completeOpenIdRegistration(result.name));
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
      isAuthenticating = true;
      await onSignUp(await authFlow.completeOpenIdRegistration(name));
    } catch (error) {
      onError(error); // Propagate unhandled errors to parent component
    } finally {
      isAuthenticating = false;
    }
  };

  const handleRegistered = async (identityNumber: bigint) => {
    await onSignIn(identityNumber);
  };
</script>

{#snippet dialogContent()}
  {#if authFlow.view === "setupOrUseExistingPasskey"}
    <SetupOrUseExistingPasskey
      setupNew={authFlow.setupNewPasskey}
      useExisting={handleContinueWithExistingPasskey}
      upgrade={() => (isUpgrading = true)}
    />
  {:else if authFlow.view === "setupNewPasskey"}
    <CreatePasskey create={handleCreatePasskey} />
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
{:else if isUpgrading}
  {#if !withinDialog}
    <Dialog onClose={() => (isUpgrading = false)}>
      <MigrationWizard onSuccess={onUpgrade} {onError} />
    </Dialog>
  {:else}
    <MigrationWizard onSuccess={onUpgrade} {onError} />
  {/if}
{:else if nonNullish(authFlow.captcha)}
  <SolveCaptcha {...authFlow.captcha} />
{:else}
  {#if authFlow.view === "chooseMethod" || !withinDialog}
    {@render children?.()}
    <PickAuthenticationMethod
      setupOrUseExistingPasskey={authFlow.setupOrUseExistingPasskey}
      continueWithOpenId={handleContinueWithOpenId}
    />
  {/if}
  {#if authFlow.view !== "chooseMethod"}
    {#if !withinDialog}
      <Dialog
        onClose={() => {
          if (isAuthenticating) {
            return;
          }
          authFlow.chooseMethod();
        }}
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
