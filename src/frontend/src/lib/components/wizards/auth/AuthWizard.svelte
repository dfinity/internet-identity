<script lang="ts">
  import { AuthFlow } from "$lib/flows/authFlow.svelte";
  import type { Snippet } from "svelte";
  import SolveCaptcha from "$lib/components/wizards/auth/views/SolveCaptcha.svelte";
  import PickAuthenticationMethod from "$lib/components/wizards/auth/views/PickAuthenticationMethod.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import SetupOrUseExistingPasskey from "$lib/components/wizards/auth/views/SetupOrUseExistingPasskey.svelte";
  import CreatePasskey from "$lib/components/wizards/auth/views/CreatePasskey.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { MigrationWizard } from "$lib/components/wizards/migration";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
  import { isOpenIdCancelError } from "$lib/utils/openID";
  import ContinueOnAnotherDeviceView from "$lib/components/wizards/auth/views/ContinueOnAnotherDeviceView.svelte";
  import { toaster } from "$lib/components/utils/toaster";
  import { t } from "$lib/stores/locale.store";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import CreateIdentity from "$lib/components/wizards/auth/views/CreateIdentity.svelte";
  import SignInWithSso from "$lib/components/wizards/auth/views/SignInWithSso.svelte";
  import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import { get } from "svelte/store";

  interface OpenIdNotConnectedArgs {
    providerName: string;
    providerLogo?: string;
    userName?: string;
    userEmail?: string;
    resume: () => Promise<void>;
    // Signal the dispatcher that the user dismissed the disambiguation
    // dialog without committing — releases the picker's in-flight loader.
    cancel: () => void;
  }

  interface OpenIdAlreadyLinkedArgs {
    providerName: string;
    providerLogo?: string;
    userName?: string;
    userEmail?: string;
    signIn: () => Promise<void>;
    cancel: () => void;
  }

  type NewProvider =
    | { type: "passkey" }
    | { type: "openid"; logo: string; name: string }
    | { type: "sso"; name: string };

  interface MethodSwitchArgs {
    previous: LastUsedIdentity;
    newProvider: NewProvider;
    proceed: () => Promise<void>;
  }

  interface Props {
    onSignIn: (identityNumber: bigint) => Promise<void>;
    onSignUp: (identityNumber: bigint) => Promise<void>;
    onUpgrade: (identityNumber: bigint) => Promise<void>;
    onError: (error: unknown) => void;
    onOpenIdNotConnected?: (args: OpenIdNotConnectedArgs) => void;
    onOpenIdAlreadyLinked?: (args: OpenIdAlreadyLinkedArgs) => void;
    onMethodSwitch?: (args: MethodSwitchArgs) => void;
    onSwitchMode?: () => void;
    withinDialog?: boolean;
    mode?: "signin" | "signup" | "both";
    children?: Snippet;
  }

  let {
    onSignIn,
    onSignUp,
    onUpgrade,
    onError,
    onOpenIdNotConnected,
    onOpenIdAlreadyLinked,
    onMethodSwitch,
    onSwitchMode,
    withinDialog = false,
    mode = "both",
    children,
  }: Props = $props();

  const methodType = (
    m: LastUsedIdentity["authMethod"],
  ): NewProvider["type"] =>
    "passkey" in m ? "passkey" : "openid" in m ? "openid" : "sso";

  const maybeInterceptMethodSwitch = (
    identityNumber: bigint,
    newProvider: NewProvider,
    previousSnapshot: LastUsedIdentity | undefined,
  ): boolean => {
    if (onMethodSwitch === undefined || previousSnapshot === undefined) {
      return false;
    }
    if (methodType(previousSnapshot.authMethod) === newProvider.type) {
      return false;
    }
    onMethodSwitch({
      previous: previousSnapshot,
      newProvider,
      proceed: () => onSignIn(identityNumber),
    });
    return true;
  };

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
      const preSnapshot = { ...get(lastUsedIdentitiesStore).identities };
      const identityNumber = await authFlow.continueWithExistingPasskey();
      if (
        maybeInterceptMethodSwitch(
          identityNumber,
          { type: "passkey" },
          preSnapshot[identityNumber.toString()],
        )
      ) {
        return;
      }
      await onSignIn(identityNumber);
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
      const preSnapshot = { ...get(lastUsedIdentitiesStore).identities };
      const result = await authFlow.continueWithOpenId(config);
      if (result.type === "signIn") {
        if (mode === "signup" && onOpenIdAlreadyLinked !== undefined) {
          const identityNumber = result.identityNumber;
          // Stay in the in-flight state until the user commits (signIn) or
          // dismisses (cancel); this keeps the picker's loader on the
          // provider button instead of flashing back to the idle state
          // while the disambiguation dialog is open.
          return await new Promise<void | "cancelled">((resolve) => {
            onOpenIdAlreadyLinked({
              providerName: config.name,
              providerLogo: config.logo,
              userName: result.name,
              userEmail: result.email,
              signIn: async () => {
                try {
                  await onSignIn(identityNumber);
                  resolve();
                } catch (error) {
                  onError(error);
                  resolve();
                }
              },
              cancel: () => resolve("cancelled"),
            });
          });
        }
        if (
          maybeInterceptMethodSwitch(
            result.identityNumber,
            { type: "openid", logo: config.logo, name: config.name },
            preSnapshot[result.identityNumber.toString()],
          )
        ) {
          return;
        }
        await onSignIn(result.identityNumber);
        return;
      }
      if (mode === "signin" && onOpenIdNotConnected !== undefined) {
        pendingSsoRegistration = false;
        return await new Promise<void | "cancelled">((resolve) => {
          onOpenIdNotConnected({
            providerName: config.name,
            providerLogo: config.logo,
            userName: result.name,
            userEmail: result.email,
            resume: async () => {
              try {
                if (result.name !== undefined) {
                  await onSignUp(
                    await authFlow.completeOpenIdRegistration(result.name),
                  );
                } else {
                  authFlow.setupNewIdentity();
                }
                resolve();
              } catch (error) {
                onError(error);
                resolve();
              }
            },
            cancel: () => resolve("cancelled"),
          });
        });
      }
      if (result.name !== undefined) {
        await onSignUp(await authFlow.completeOpenIdRegistration(result.name));
      } else {
        pendingSsoRegistration = false;
        authFlow.setupNewIdentity();
      }
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        toaster.info({ title: $t`Sign-in was canceled` });
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
      const preSnapshot = { ...get(lastUsedIdentitiesStore).identities };
      const authResult = await authFlow.continueWithSso(result);
      if (authResult.type === "signIn") {
        if (
          maybeInterceptMethodSwitch(
            authResult.identityNumber,
            { type: "sso", name: result.name ?? result.domain },
            preSnapshot[authResult.identityNumber.toString()],
          )
        ) {
          return;
        }
        await onSignIn(authResult.identityNumber);
      } else if (authResult.name !== undefined) {
        await onSignUp(await authFlow.completeSsoRegistration(authResult.name));
      } else {
        // The SSO IdP didn't supply a name — drive the user to the
        // name-entry view and mark the deferred completion as SSO so
        // the callback dispatches to the SSO path.
        pendingSsoRegistration = true;
        authFlow.setupNewIdentity();
      }
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        toaster.info({ title: $t`Sign-in was canceled` });
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

{#if authFlow.captcha !== undefined}
  <SolveCaptcha {...authFlow.captcha} />
{:else if withinDialog && isContinueFromAnotherDeviceVisible}
  <ContinueOnAnotherDeviceView onRegistered={handleRegistered} {onError} />
{:else if withinDialog && isUpgrading}
  <MigrationWizard onSuccess={onUpgrade} {onError} />
{:else}
  {#if authFlow.view === "chooseMethod" || !withinDialog}
    {@render children?.()}
    <PickAuthenticationMethod
      setupOrUseExistingPasskey={mode === "signin"
        ? handleContinueWithExistingPasskey
        : mode === "signup"
          ? authFlow.setupNewPasskey
          : authFlow.setupOrUseExistingPasskey}
      continueWithOpenId={handleContinueWithOpenId}
      signInWithSso={authFlow.signInWithSso}
      {mode}
      {onSwitchMode}
      {withinDialog}
    />
  {/if}
  {#if authFlow.view !== "chooseMethod"}
    {#if !withinDialog}
      {#if !isContinueFromAnotherDeviceVisible && !isUpgrading}
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
      {/if}
    {:else}
      {@render dialogContent()}
    {/if}
  {/if}
{/if}

{#if !withinDialog && isContinueFromAnotherDeviceVisible}
  <Dialog onClose={() => (isContinueFromAnotherDeviceVisible = false)}>
    <ContinueOnAnotherDeviceView onRegistered={handleRegistered} {onError} />
  </Dialog>
{/if}
{#if !withinDialog && isUpgrading}
  <Dialog onClose={() => (isUpgrading = false)}>
    <MigrationWizard onSuccess={onUpgrade} {onError} />
  </Dialog>
{/if}

{#if authFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}
