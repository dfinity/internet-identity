<script lang="ts">
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
  import SignInWithSso from "$lib/components/wizards/auth/views/SignInWithSso.svelte";
  import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";

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
  // True while a deferred SSO registration is awaiting the name-entry view
  // (`setupNewIdentity`). Used by `handleCompleteOpenIdRegistration` to
  // dispatch to `completeSsoRegistration` instead of the direct-provider
  // counterpart. Plain `let` (no `$state`) — only read inside async event
  // handlers, never in the template, so reactivity isn't needed.
  let pendingSsoRegistration = false;

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
      } else if (result.name !== undefined) {
        await onSignUp(await authFlow.completeOpenIdRegistration(result.name));
      } else {
        // Deferred direct-OpenID sign-up: ensure the SSO flag is clear so
        // the name-entry view dispatches to `completeOpenIdRegistration`.
        pendingSsoRegistration = false;
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

  const handleContinueWithSso = async (
    result: SsoDiscoveryResult,
  ): Promise<void | "cancelled"> => {
    try {
      isAuthenticating = true;
      const authResult = await authFlow.continueWithSso(result);
      if (authResult.type === "signIn") {
        await onSignIn(authResult.identityNumber);
      } else if (authResult.name !== undefined) {
        await onSignUp(await authFlow.completeSsoRegistration(authResult.name));
      } else {
        // The SSO IdP didn't supply a name — the wizard now drives the
        // user to `setupNewIdentity`. Mark the deferred completion as
        // SSO so the name-entry callback dispatches to the SSO path.
        pendingSsoRegistration = true;
      }
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        return "cancelled";
      }
      onError(error);
    } finally {
      isAuthenticating = false;
    }
  };

  const handleCompleteOpenIdRegistration = async (
    name: string,
  ): Promise<void> => {
    try {
      isAuthenticating = true;
      if (pendingSsoRegistration) {
        await onSignUp(await authFlow.completeSsoRegistration(name));
        pendingSsoRegistration = false;
      } else {
        await onSignUp(await authFlow.completeOpenIdRegistration(name));
      }
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
  {:else if authFlow.view === "signInWithSso"}
    <SignInWithSso
      continueWithSso={handleContinueWithSso}
      goBack={authFlow.chooseMethod}
    />
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
{:else if authFlow.captcha !== undefined}
  <SolveCaptcha {...authFlow.captcha} />
{:else}
  {#if authFlow.view === "chooseMethod" || !withinDialog}
    {@render children?.()}
    <PickAuthenticationMethod
      setupOrUseExistingPasskey={authFlow.setupOrUseExistingPasskey}
      continueWithOpenId={handleContinueWithOpenId}
      signInWithSso={authFlow.signInWithSso}
    />
  {/if}
  {#if authFlow.view !== "chooseMethod"}
    {#if !withinDialog}
      <Dialog
        onClose={() => {
          if (isAuthenticating) {
            return;
          }
          pendingSsoRegistration = false;
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
