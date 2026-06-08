<script lang="ts">
  import {
    AuthFlow,
    type AuthMode,
    type MethodTag,
  } from "$lib/flows/authFlow.svelte";
  import { type Snippet, untrack } from "svelte";
  import SolveCaptcha from "$lib/components/wizards/auth/views/SolveCaptcha.svelte";
  import PickAuthenticationMethod from "$lib/components/wizards/auth/views/PickAuthenticationMethod.svelte";
  import Dialog, { isInsideDialog } from "$lib/components/ui/Dialog.svelte";
  import SetupOrUseExistingPasskey from "$lib/components/wizards/auth/views/SetupOrUseExistingPasskey.svelte";
  import CreatePasskey from "$lib/components/wizards/auth/views/CreatePasskey.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { MigrationWizard } from "$lib/components/wizards/migration";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
  import { isOpenIdCancelError } from "$lib/utils/openID";
  import ContinueOnAnotherDeviceView from "$lib/components/wizards/auth/views/ContinueOnAnotherDeviceView.svelte";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import CreateIdentity from "$lib/components/wizards/auth/views/CreateIdentity.svelte";
  import SignInWithSso from "$lib/components/wizards/auth/views/SignInWithSso.svelte";
  import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import { get } from "svelte/store";
  import IdentityNotConnected from "$lib/components/wizards/auth/views/IdentityNotConnected.svelte";
  import IdentityAlreadyLinked from "$lib/components/wizards/auth/views/IdentityAlreadyLinked.svelte";
  import SwitchAccessMethod from "$lib/components/wizards/auth/views/SwitchAccessMethod.svelte";
  import { goto } from "$app/navigation";

  interface Props {
    onSignIn: (identityNumber: bigint) => Promise<void>;
    onSignUp: (identityNumber: bigint) => Promise<void>;
    onUpgrade: (identityNumber: bigint) => Promise<void>;
    onError: (error: unknown) => void;
    mode?: AuthMode;
    children?: Snippet<[boolean?]>;
  }

  let {
    onSignIn,
    onSignUp,
    onUpgrade,
    onError,
    mode = $bindable("both"),
    children,
  }: Props = $props();

  // Initial mode snapshot — restored when the wizard's own dialog closes
  // so the parent's inline-mount state doesn't drift into a mode it never
  // intended after a user self-elevated and toggled.
  const initialMode = untrack(() => mode);

  const authFlow = new AuthFlow();
  $effect(() => {
    authFlow.setMode(mode);
  });

  const inDialog: boolean = isInsideDialog();
  let isElevated = $state(false);
  let isContinueFromAnotherDeviceVisible = $state(false);
  let isAuthenticating = $state(false);
  let isUpgrading = $state(false);
  let pendingSsoRegistration = false;

  const methodType = (m: LastUsedIdentity["authMethod"]): MethodTag =>
    "passkey" in m ? "passkey" : "openid" in m ? "openid" : "sso";

  const maybeRequestMethodSwitch = (
    signedInIdentityNumber: bigint,
    newMethod: MethodTag,
    previousSnapshot: LastUsedIdentity | undefined,
    providerInfo?: {
      providerIssuer?: string;
      providerDomain?: string;
      providerName?: string;
    },
  ): boolean => {
    if (previousSnapshot === undefined) return false;
    if (methodType(previousSnapshot.authMethod) === newMethod) return false;
    authFlow.requestMethodSwitch({
      previousIdentityNumber: previousSnapshot.identityNumber,
      newMethod,
      signedInIdentityNumber,
      ...providerInfo,
    });
    return true;
  };

  // In signup mode the toggle's "Already have an identity? Sign in" CTA
  // is misleading when the user has none — hide it in that case. Signin
  // mode's "New to Internet Identity? Sign up" is always useful.
  const switchModeAvailable = $derived(
    mode === "signin" ||
      Object.keys($lastUsedIdentitiesStore.identities).length > 0,
  );

  const toggleMode = () => {
    mode = mode === "signin" ? "signup" : "signin";
    if (!inDialog) isElevated = true;
  };

  // Full reset — restores the wizard to its initial state. Fires when the
  // user closes the wizard's outermost dialog (either standalone or
  // self-elevated from an inline picker).
  const reset = () => {
    if (isAuthenticating) return;
    isElevated = false;
    pendingSsoRegistration = false;
    mode = initialMode;
    authFlow.chooseMethod();
  };

  // Sub-view cancel — clears the active sub-view but preserves mode and
  // elevation. Fires when the user closes a sub-view dialog (e.g. OIDC
  // disambiguation, method-switch confirmation) while the wizard sits
  // inside a parent dialog and the user wants to keep the picker open.
  const cancelSubView = () => {
    if (isAuthenticating) return;
    if (
      authFlow.view === "openIdNotConnected" ||
      authFlow.view === "openIdAlreadyLinked"
    ) {
      authFlow.cancelOpenIdDisambiguation();
      pendingSsoRegistration = false;
      return;
    }
    if (authFlow.view === "confirmMethodSwitch") {
      authFlow.cancelMethodSwitch();
      return;
    }
    authFlow.chooseMethod();
  };

  const handleContinueWithExistingPasskey = async (): Promise<
    void | "cancelled"
  > => {
    try {
      isAuthenticating = true;
      const preSnapshot = { ...get(lastUsedIdentitiesStore).identities };
      const identityNumber = await authFlow.continueWithExistingPasskey();
      if (
        maybeRequestMethodSwitch(
          identityNumber,
          "passkey",
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
      onError(error);
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
      onError(error);
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
      const result = await authFlow.continueWithOpenId(config, undefined, mode);
      if (result === undefined) {
        // Disambiguation took over — AuthFlow set the view, dialog
        // will render. Wizard exits the in-flight state.
        return;
      }
      if (result.type === "signIn") {
        if (
          maybeRequestMethodSwitch(
            result.identityNumber,
            "openid",
            preSnapshot[result.identityNumber.toString()],
            { providerIssuer: config.issuer, providerName: config.name },
          )
        ) {
          return;
        }
        await onSignIn(result.identityNumber);
        return;
      }
      if (result.name !== undefined) {
        await onSignUp(await authFlow.completeOpenIdRegistration(result.name));
      } else {
        pendingSsoRegistration = false;
        authFlow.setupNewIdentity();
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

  const handleContinueWithSso = async (
    ssoResult: SsoDiscoveryResult,
  ): Promise<void | "cancelled"> => {
    try {
      isAuthenticating = true;
      const preSnapshot = { ...get(lastUsedIdentitiesStore).identities };
      const authResult = await authFlow.continueWithSso(ssoResult, mode);
      if (authResult === undefined) {
        if (authFlow.view === "openIdNotConnected") {
          pendingSsoRegistration = true;
        }
        return;
      }
      if (authResult.type === "signIn") {
        if (
          maybeRequestMethodSwitch(
            authResult.identityNumber,
            "sso",
            preSnapshot[authResult.identityNumber.toString()],
            {
              providerDomain: ssoResult.domain,
              providerName: ssoResult.name ?? ssoResult.domain,
            },
          )
        ) {
          return;
        }
        await onSignIn(authResult.identityNumber);
        return;
      }
      if (authResult.name !== undefined) {
        await onSignUp(await authFlow.completeSsoRegistration(authResult.name));
      } else {
        pendingSsoRegistration = true;
        authFlow.setupNewIdentity();
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
      onError(error);
    } finally {
      isAuthenticating = false;
    }
  };

  const handleConfirmOpenIdSignUp = async (): Promise<void> => {
    try {
      isAuthenticating = true;
      const result = await authFlow.confirmOpenIdSignUp();
      if (result !== "needs-name") {
        await onSignUp(result);
      }
    } catch (error) {
      onError(error);
    } finally {
      isAuthenticating = false;
    }
  };

  const handleConfirmOpenIdSignIn = async (): Promise<void> => {
    try {
      isAuthenticating = true;
      const identityNumber = authFlow.confirmOpenIdSignIn();
      await onSignIn(identityNumber);
    } catch (error) {
      onError(error);
    } finally {
      isAuthenticating = false;
    }
  };

  const handleRecoverFromNotConnected = (): void => {
    authFlow.cancelOpenIdDisambiguation();
    void goto("/recovery");
  };

  const handleConfirmMethodSwitch = async (): Promise<void> => {
    try {
      isAuthenticating = true;
      const identityNumber = authFlow.confirmMethodSwitch();
      await onSignIn(identityNumber);
    } catch (error) {
      onError(error);
    } finally {
      isAuthenticating = false;
    }
  };

  const handleRegistered = async (identityNumber: bigint) => {
    await onSignIn(identityNumber);
  };
</script>

{#snippet activeView()}
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
  {:else if authFlow.view === "openIdNotConnected"}
    <IdentityNotConnected
      issuer={authFlow.configIssuer}
      providerName={authFlow.providerName}
      providerLogo={authFlow.providerLogo}
      userName={authFlow.userName}
      userEmail={authFlow.userEmail}
      onSignUp={handleConfirmOpenIdSignUp}
      onRecover={handleRecoverFromNotConnected}
      onCancel={cancelSubView}
      loading={isAuthenticating}
    />
  {:else if authFlow.view === "openIdAlreadyLinked"}
    <IdentityAlreadyLinked
      issuer={authFlow.configIssuer}
      providerName={authFlow.providerName}
      providerLogo={authFlow.providerLogo}
      userName={authFlow.userName}
      userEmail={authFlow.userEmail}
      onSignIn={handleConfirmOpenIdSignIn}
      onCancel={cancelSubView}
      loading={isAuthenticating}
    />
  {:else if authFlow.view === "confirmMethodSwitch" && authFlow.pendingMethodSwitch !== undefined}
    <SwitchAccessMethod
      previousIdentityNumber={authFlow.pendingMethodSwitch
        .previousIdentityNumber}
      newMethod={authFlow.pendingMethodSwitch.newMethod}
      providerIssuer={authFlow.pendingMethodSwitch.providerIssuer}
      providerName={authFlow.pendingMethodSwitch.providerName}
      onSwitch={handleConfirmMethodSwitch}
      onCancel={cancelSubView}
    />
  {/if}
{/snippet}

{#snippet pickerBlock()}
  {@render children?.(inDialog || isElevated)}
  <PickAuthenticationMethod
    setupOrUseExistingPasskey={mode === "signin"
      ? handleContinueWithExistingPasskey
      : mode === "signup"
        ? authFlow.setupNewPasskey
        : authFlow.setupOrUseExistingPasskey}
    continueWithOpenId={handleContinueWithOpenId}
    signInWithSso={authFlow.signInWithSso}
    {mode}
    onSwitchMode={switchModeAvailable ? toggleMode : undefined}
    withinDialog={inDialog || isElevated}
  />
{/snippet}

<!-- Single persistent Dialog instance — content swaps reactively as the
     wizard's view changes. Prevents remount races between captcha and
     post-captcha views when SvelteKit navigation interleaves with the
     Dialog's onNavigate outro-pause. -->
{#if authFlow.view === "chooseMethod" && !inDialog && !isElevated && authFlow.captcha === undefined && !isContinueFromAnotherDeviceVisible && !isUpgrading}
  {@render pickerBlock()}
{:else}
  {@const dialogOnClose =
    authFlow.captcha !== undefined
      ? undefined
      : isContinueFromAnotherDeviceVisible
        ? () => (isContinueFromAnotherDeviceVisible = false)
        : isUpgrading
          ? () => (isUpgrading = false)
          : reset}
  <!-- When the wizard is nested inside a parent Dialog, pass through
       and render content directly into the parent — avoids stacking
       two <dialog> elements (Safari renders both visibly, focus and
       top-layer cleanup race in CI). Sub-views that need a "go back
       to picker" affordance render their own in-content button which
       calls cancelSubView. -->
  <Dialog onClose={dialogOnClose} passthrough={inDialog}>
    {#if authFlow.captcha !== undefined}
      <SolveCaptcha {...authFlow.captcha} />
    {:else if isContinueFromAnotherDeviceVisible}
      <ContinueOnAnotherDeviceView onRegistered={handleRegistered} {onError} />
    {:else if isUpgrading}
      <MigrationWizard onSuccess={onUpgrade} {onError} />
    {:else if authFlow.view === "chooseMethod"}
      {@render pickerBlock()}
    {:else}
      {@render activeView()}
    {/if}
  </Dialog>
{/if}

{#if authFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}
