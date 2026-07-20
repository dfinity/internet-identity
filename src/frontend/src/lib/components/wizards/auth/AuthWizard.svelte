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
  import { isInsideAuthPanel } from "$lib/components/ui/AuthPanel.svelte";
  import SetupOrUseExistingPasskey from "$lib/components/wizards/auth/views/SetupOrUseExistingPasskey.svelte";
  import CreatePasskey from "$lib/components/wizards/auth/views/CreatePasskey.svelte";
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
  import { isOpenIdCancelError } from "$lib/utils/openID";
  import ContinueOnAnotherDeviceView from "$lib/components/wizards/auth/views/ContinueOnAnotherDeviceView.svelte";
  import type { OpenIdConfig } from "$lib/generated/internet_identity_types";
  import CreateIdentity from "$lib/components/wizards/auth/views/CreateIdentity.svelte";
  import SignInWithSso from "$lib/components/wizards/auth/views/SignInWithSso.svelte";
  import SsoNormalLoginRequired from "$lib/components/wizards/auth/views/SsoNormalLoginRequired.svelte";
  import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";
  import { SsoNormalLoginRequiredError } from "$lib/utils/authentication/jwt";
  import {
    lastUsedIdentitiesStore,
    type LastUsedIdentity,
  } from "$lib/stores/last-used-identities.store";
  import type { PendingLastUsedEntry } from "$lib/flows/authFlow.svelte";
  import { get } from "svelte/store";
  import IdentityNotConnected from "$lib/components/wizards/auth/views/IdentityNotConnected.svelte";
  import IdentityAlreadyLinked from "$lib/components/wizards/auth/views/IdentityAlreadyLinked.svelte";
  import SwitchAccessMethod from "$lib/components/wizards/auth/views/SwitchAccessMethod.svelte";
  import { goto } from "$app/navigation";
  import {
    shouldRequestMethodSwitch,
    type MethodDescriptor,
  } from "$lib/components/wizards/auth/AuthWizard.switch";

  interface Props {
    onSignIn: (identityNumber: bigint) => Promise<void>;
    onSignUp: (identityNumber: bigint) => Promise<void>;
    onError: (error: unknown) => void;
    mode?: AuthMode;
    // Override the primary passkey-button label in the picker. Forwarded
    // verbatim to PickAuthenticationMethod.
    passkeyLabel?: string;
    // Override the switch-mode CTA title + button action. Forwarded
    // verbatim to PickAuthenticationMethod.
    switchModeTitle?: string;
    switchModeAction?: string;
    // The target dapp origin; set only in the authorize (dapp sign-in) flow.
    ssoOrigin?: string;
    children?: Snippet<[boolean?]>;
  }

  let {
    onSignIn,
    onSignUp,
    onError,
    mode = $bindable("both"),
    passkeyLabel,
    switchModeTitle,
    switchModeAction,
    ssoOrigin,
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
  const inAuthPanel: boolean = isInsideAuthPanel();
  let isElevated = $state(false);
  let isContinueFromAnotherDeviceVisible = $state(false);
  let isAuthenticating = $state(false);
  let pendingSsoRegistration = false;

  // The SSO discovery result of the in-flight gated login, kept so the
  // normal-login dialog can bridge + replay it after a normal sign-in.
  let activeSsoResult = $state<SsoDiscoveryResult>();
  // Set when a gated non-`sub` login can't resolve directly; drives the
  // "First sign-in with X" dialog (SsoNormalLoginRequired).
  let ssoNormalLoginResult = $state<SsoDiscoveryResult>();

  const dismissSsoNormalLogin = () => {
    ssoNormalLoginResult = undefined;
    authFlow.chooseMethod();
  };

  // Dialog primary action: run the normal (primary-client) sign-in to bridge the
  // identity, then replay the stashed gated JWT (no fresh ceremony) to complete
  // the gated sign-in. See SsoNormalLoginRequired.svelte.
  const handleSsoNormalLoginContinue = async () => {
    if (activeSsoResult === undefined) {
      return;
    }
    try {
      isAuthenticating = true;
      // Force the org's primary client for the normal (un-gated) sign-in.
      const primaryResult: SsoDiscoveryResult = {
        ...activeSsoResult,
        resolvedClientId: activeSsoResult.clientId,
      };
      const identityNumber =
        await authFlow.completeSsoNormalLoginRecovery(primaryResult);
      if (identityNumber !== undefined) {
        ssoNormalLoginResult = undefined;
        await onSignIn(identityNumber);
      }
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        return;
      }
      onError(error);
    } finally {
      isAuthenticating = false;
    }
  };

  const methodDescriptor = (
    newMethod: MethodTag,
    providerIssuer: string | undefined,
    providerDomain: string | undefined,
  ): MethodDescriptor | undefined => {
    if (newMethod === "passkey") return { passkey: {} };
    if (newMethod === "openid")
      return providerIssuer === undefined
        ? undefined
        : { openid: { iss: providerIssuer } };
    return providerDomain === undefined
      ? undefined
      : { sso: { domain: providerDomain } };
  };

  const maybeRequestMethodSwitch = (
    signedInIdentityNumber: bigint,
    newMethod: MethodTag,
    previousSnapshot: LastUsedIdentity | undefined,
    pendingLastUsedEntry: PendingLastUsedEntry | undefined,
    providerInfo?: {
      providerIssuer?: string;
      providerDomain?: string;
      providerName?: string;
    },
  ): boolean => {
    if (previousSnapshot === undefined) return false;
    const next = methodDescriptor(
      newMethod,
      providerInfo?.providerIssuer,
      providerInfo?.providerDomain,
    );
    if (next === undefined) return false;
    if (!shouldRequestMethodSwitch(previousSnapshot.authMethod, next)) {
      return false;
    }
    authFlow.requestMethodSwitch({
      previousIdentity: previousSnapshot,
      newMethod,
      signedInIdentityNumber,
      pendingLastUsedEntry,
      ...providerInfo,
    });
    return true;
  };

  // Commits the deferred last-used entry once the user has cleared
  // (or skipped) the method-switch disambiguation. Mirrors the commit
  // step that `authFlow.confirmMethodSwitch` runs on the dialog path.
  const commitLastUsedEntry = (entry: PendingLastUsedEntry | undefined) => {
    if (entry !== undefined) {
      lastUsedIdentitiesStore.addLastUsedIdentity(entry);
    }
  };

  // In signup mode the toggle's "Already have an identity? Sign in" CTA
  // is misleading for fresh users with no identities. Hide it only in the
  // bare inline-picker case where that's a real concern — inside a parent
  // Dialog, self-elevated, or framed by an AuthPanel, the parent already
  // committed to showing both directions of the flow.
  const switchModeAvailable = $derived(
    mode === "signin" ||
      inDialog ||
      isElevated ||
      inAuthPanel ||
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
    // Dismissing a sub-view dialog via X/backdrop must run the same
    // cancel cleanup as the in-content "Use a different method" link
    // (e.g. restoring lastUsedIdentities for a cancelled method switch).
    if (
      authFlow.view === "openIdNotConnected" ||
      authFlow.view === "openIdAlreadyLinked" ||
      authFlow.view === "confirmMethodSwitch"
    ) {
      cancelSubView();
    }
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
      const { identityNumber, pendingLastUsedEntry } =
        await authFlow.continueWithExistingPasskey();
      if (
        maybeRequestMethodSwitch(
          identityNumber,
          "passkey",
          preSnapshot[identityNumber.toString()],
          pendingLastUsedEntry,
        )
      ) {
        return;
      }
      commitLastUsedEntry(pendingLastUsedEntry);
      await onSignIn(identityNumber);
    } catch (error) {
      if (isWebAuthnCancelError(error)) {
        return "cancelled";
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
            result.pendingLastUsedEntry,
            { providerIssuer: config.issuer, providerName: config.name },
          )
        ) {
          return;
        }
        commitLastUsedEntry(result.pendingLastUsedEntry);
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

  const applySsoAuthResult = async (
    authResult: Awaited<ReturnType<typeof authFlow.continueWithSso>>,
    ssoResult: SsoDiscoveryResult,
    preSnapshot: Record<string, LastUsedIdentity>,
  ): Promise<void> => {
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
          authResult.pendingLastUsedEntry,
          {
            providerDomain: ssoResult.domain,
            providerName: ssoResult.name ?? ssoResult.domain,
          },
        )
      ) {
        return;
      }
      commitLastUsedEntry(authResult.pendingLastUsedEntry);
      await onSignIn(authResult.identityNumber);
      return;
    }
    if (authResult.name !== undefined) {
      await onSignUp(await authFlow.completeSsoRegistration(authResult.name));
    } else {
      pendingSsoRegistration = true;
      authFlow.setupNewIdentity();
    }
  };

  const handleContinueWithSso = async (
    ssoResult: SsoDiscoveryResult,
  ): Promise<void | "cancelled"> => {
    activeSsoResult = ssoResult;
    try {
      isAuthenticating = true;
      const preSnapshot = { ...get(lastUsedIdentitiesStore).identities };
      const authResult = await authFlow.continueWithSso(
        ssoResult,
        mode,
        ssoOrigin,
      );
      await applySsoAuthResult(authResult, ssoResult, preSnapshot);
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        return "cancelled";
      }
      // Gated non-`sub` login can't resolve directly — show the normal-login
      // dialog instead of surfacing an error.
      if (
        error instanceof SsoNormalLoginRequiredError &&
        activeSsoResult !== undefined
      ) {
        ssoNormalLoginResult = activeSsoResult;
        return;
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
      handleSsoRegistrationError(error);
    } finally {
      isAuthenticating = false;
    }
  };

  // Route the gated non-`sub` fail-safe to the "sign in normally first" CTA
  // instead of a generic error. Falls through to onError for anything else, or if
  // there's no gated result to retry afterwards.
  const handleSsoRegistrationError = (error: unknown) => {
    if (
      error instanceof SsoNormalLoginRequiredError &&
      activeSsoResult !== undefined
    ) {
      ssoNormalLoginResult = activeSsoResult;
      return;
    }
    onError(error);
  };

  const handleConfirmOpenIdSignUp = async (): Promise<void> => {
    try {
      isAuthenticating = true;
      const result = await authFlow.confirmOpenIdSignUp();
      if (result !== "needs-name") {
        await onSignUp(result);
      }
    } catch (error) {
      handleSsoRegistrationError(error);
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
    const pending = authFlow.pendingMethodSwitch;
    if (pending === undefined) return;
    try {
      isAuthenticating = true;
      // Commit the deferred last-used entry BEFORE onSignIn navigates —
      // receiving routes read lastUsedIdentities on mount, so a post-
      // navigation write would lose the race.
      commitLastUsedEntry(pending.pendingLastUsedEntry);
      // Keep the SwitchAccessMethod view visible until onSignIn closes
      // the parent dialog — otherwise the picker briefly flashes back
      // in between the state reset and the dialog teardown.
      await onSignIn(pending.signedInIdentityNumber);
      authFlow.confirmMethodSwitch();
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
  {#if ssoNormalLoginResult !== undefined}
    <!-- Gated non-`sub` login couldn't resolve directly: prompt one normal
         sign-in, then bridge + replay the stashed gated JWT to continue. -->
    <SsoNormalLoginRequired
      name={ssoNormalLoginResult.name ?? ssoNormalLoginResult.domain}
      onContinue={handleSsoNormalLoginContinue}
      onCancel={dismissSsoNormalLogin}
      loading={isAuthenticating}
    />
  {:else if authFlow.view === "setupOrUseExistingPasskey"}
    <SetupOrUseExistingPasskey
      setupNew={authFlow.setupNewPasskey}
      useExisting={handleContinueWithExistingPasskey}
    />
  {:else if authFlow.view === "setupNewPasskey"}
    <CreatePasskey create={handleCreatePasskey} />
  {:else if authFlow.view === "setupNewIdentity"}
    <CreateIdentity create={handleCompleteOpenIdRegistration} />
  {:else if authFlow.view === "signInWithSso"}
    <SignInWithSso
      continueWithSso={handleContinueWithSso}
      goBack={authFlow.chooseMethod}
      origin={ssoOrigin}
    />
  {:else if authFlow.view === "openIdNotConnected"}
    <IdentityNotConnected
      issuer={authFlow.configIssuer}
      providerName={authFlow.providerName}
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
      userName={authFlow.userName}
      userEmail={authFlow.userEmail}
      onSignIn={handleConfirmOpenIdSignIn}
      onCancel={cancelSubView}
      loading={isAuthenticating}
    />
  {:else if authFlow.view === "confirmMethodSwitch" && authFlow.pendingMethodSwitch !== undefined}
    <SwitchAccessMethod
      previousIdentity={authFlow.pendingMethodSwitch.previousIdentity}
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
    continueOnAnotherDevice={mode === "signup"
      ? undefined
      : () => (isContinueFromAnotherDeviceVisible = true)}
    {mode}
    {passkeyLabel}
    {switchModeTitle}
    {switchModeAction}
    onSwitchMode={switchModeAvailable ? toggleMode : undefined}
    withinDialog={inDialog || isElevated || inAuthPanel}
  />
{/snippet}

<!-- Single persistent Dialog instance — content swaps reactively as the
     wizard's view changes. Prevents remount races between captcha and
     post-captcha views when SvelteKit navigation interleaves with the
     Dialog's onNavigate outro-pause. -->
{#if authFlow.view === "chooseMethod" && !inDialog && !isElevated && authFlow.captcha === undefined && ssoNormalLoginResult === undefined}
  {@render pickerBlock()}
{:else}
  {@const dialogOnClose = authFlow.captcha !== undefined ? undefined : reset}
  <!-- When the wizard is nested inside a parent Dialog, pass through
       and render content directly into the parent — avoids stacking
       two <dialog> elements (Safari renders both visibly, focus and
       top-layer cleanup race in CI). Sub-views that need a "go back
       to picker" affordance render their own in-content button which
       calls cancelSubView. -->
  <Dialog onClose={dialogOnClose} passthrough={inDialog}>
    {#if authFlow.captcha !== undefined}
      <SolveCaptcha {...authFlow.captcha} />
    {:else if authFlow.view === "chooseMethod" && ssoNormalLoginResult === undefined}
      {@render pickerBlock()}
    {:else}
      {@render activeView()}
    {/if}
  </Dialog>
{/if}

<!-- Cross-device pairing is layered on top of the picker as its own
     modal, leaving the picker mounted underneath. Closing it (X, Escape,
     or backdrop) only flips this flag back, returning the user to the
     "Add existing identity" picker rather than tearing down the whole
     (parent) dialog — which is what a shared passthrough close would do. -->
{#if isContinueFromAnotherDeviceVisible}
  <Dialog onClose={() => (isContinueFromAnotherDeviceVisible = false)}>
    <ContinueOnAnotherDeviceView onRegistered={handleRegistered} {onError} />
  </Dialog>
{/if}

{#if authFlow.systemOverlay}
  <SystemOverlayBackdrop />
{/if}
