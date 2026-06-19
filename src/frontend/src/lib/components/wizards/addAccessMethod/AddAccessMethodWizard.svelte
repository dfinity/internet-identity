<script lang="ts">
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { AddAccessMethodFlow } from "$lib/flows/addAccessMethodFlow.svelte.js";
  import type {
    AuthnMethodData,
    OpenIdCredential,
    OpenIdConfig,
  } from "$lib/generated/internet_identity_types";
  import AddAccessMethod from "$lib/components/wizards/addAccessMethod/views/AddAccessMethod.svelte";
  import SignInWithSso from "$lib/components/wizards/auth/views/SignInWithSso.svelte";
  import { ConfirmAccessMethodWizard } from "$lib/components/wizards/confirmAccessMethod";
  import { isOpenIdCancelError } from "$lib/utils/openID";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";
  import type { SsoDiscoveryResult } from "$lib/utils/ssoDiscovery";

  interface Props {
    onOpenIdLinked: (credential: OpenIdCredential) => void;
    onPasskeyRegistered: (credential: AuthnMethodData) => void;
    onOtherDeviceRegistered: () => void;
    onError: (error: unknown) => void;
    openIdCredentials?: OpenIdCredential[];
    maxPasskeysReached?: boolean;
    identityName?: string;
    // Credential ids of the identity's existing passkeys. Listed as
    // `excludeCredentials` on the create call so the OS shows the full
    // authenticator picker instead of silently reusing the just-used
    // Touch ID.
    excludePasskeyCredentialIds?: Uint8Array[];
  }

  const {
    onOpenIdLinked,
    onPasskeyRegistered,
    onOtherDeviceRegistered,
    onError,
    openIdCredentials,
    maxPasskeysReached,
    identityName,
    excludePasskeyCredentialIds,
  }: Props = $props();

  const addAccessMethodFlow = new AddAccessMethodFlow();

  let isContinueOnAnotherDeviceVisible = $state(false);

  const handleContinueWithOpenId = async (config: OpenIdConfig) => {
    try {
      onOpenIdLinked(await addAccessMethodFlow.linkOpenIdAccount(config));
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        return "cancelled";
      }
      onError(error); // Propagate unhandled errors to parent component
    }
  };
  const handleContinueWithSso = async (ssoResult: SsoDiscoveryResult) => {
    try {
      onOpenIdLinked(await addAccessMethodFlow.linkSsoAccount(ssoResult));
    } catch (error) {
      if (isOpenIdCancelError(error)) {
        return "cancelled";
      }
      onError(error);
    }
  };
  const handleCreatePasskey = async () => {
    try {
      onPasskeyRegistered(
        await addAccessMethodFlow.createPasskey(
          identityName,
          excludePasskeyCredentialIds,
        ),
      );
    } catch (error) {
      if (isWebAuthnCancelError(error)) {
        return "cancelled";
      }
      onError(error);
    }
  };
  const handleOtherDeviceRegistered = () => {
    onOtherDeviceRegistered();
  };
</script>

{#if isContinueOnAnotherDeviceVisible}
  <ConfirmAccessMethodWizard
    onConfirm={handleOtherDeviceRegistered}
    {onError}
  />
{:else if addAccessMethodFlow.view === "chooseMethod"}
  <AddAccessMethod
    createPasskey={handleCreatePasskey}
    continueOnAnotherDevice={() => (isContinueOnAnotherDeviceVisible = true)}
    linkOpenIdAccount={handleContinueWithOpenId}
    signInWithSso={addAccessMethodFlow.signInWithSso}
    {maxPasskeysReached}
    {openIdCredentials}
  />
{:else if addAccessMethodFlow.view === "signInWithSso"}
  <SignInWithSso
    continueWithSso={handleContinueWithSso}
    goBack={addAccessMethodFlow.chooseMethod}
    {openIdCredentials}
  />
{/if}

{#if addAccessMethodFlow.isSystemOverlayVisible}
  <SystemOverlayBackdrop />
{/if}
