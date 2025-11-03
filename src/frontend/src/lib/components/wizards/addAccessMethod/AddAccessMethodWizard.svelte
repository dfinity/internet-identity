<script lang="ts">
  import SystemOverlayBackdrop from "$lib/components/utils/SystemOverlayBackdrop.svelte";
  import { AddAccessMethodFlow } from "$lib/flows/addAccessMethodFlow.svelte.js";
  import type {
    AuthnMethodData,
    OpenIdCredential,
    OpenIdConfig,
  } from "$lib/generated/internet_identity_types";
  import AddAccessMethod from "$lib/components/wizards/addAccessMethod/views/AddAccessMethod.svelte";
  import AddPasskey from "$lib/components/wizards/addAccessMethod/views/AddPasskey.svelte";
  import { ConfirmAccessMethodWizard } from "$lib/components/wizards/confirmAccessMethod";
  import { isOpenIdCancelError } from "$lib/utils/openID";
  import { isWebAuthnCancelError } from "$lib/utils/webAuthnErrorUtils";

  interface Props {
    onOpenIdLinked: (credential: OpenIdCredential) => void;
    onPasskeyRegistered: (credential: AuthnMethodData) => void;
    onOtherDeviceRegistered: () => void;
    onError: (error: unknown) => void;
    isUsingPasskeys?: boolean;
    openIdCredentials?: OpenIdCredential[];
    maxPasskeysReached?: boolean;
  }

  const {
    onOpenIdLinked,
    onPasskeyRegistered,
    onOtherDeviceRegistered,
    onError,
    isUsingPasskeys,
    openIdCredentials,
    maxPasskeysReached,
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
  const handleCreatePasskey = async () => {
    try {
      onPasskeyRegistered(await addAccessMethodFlow.createPasskey());
    } catch (error) {
      if (isWebAuthnCancelError(error)) {
        return "cancelled";
      }
      onError(error);
    }
  };
  const handleOtherDeviceRegistered = async () => {
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
    continueWithPasskey={addAccessMethodFlow.continueWithPasskey}
    linkOpenIdAccount={handleContinueWithOpenId}
    {maxPasskeysReached}
    {openIdCredentials}
  />
{:else if addAccessMethodFlow.view === "addPasskey"}
  <AddPasskey
    createPasskey={handleCreatePasskey}
    continueOnAnotherDevice={() => (isContinueOnAnotherDeviceVisible = true)}
    {isUsingPasskeys}
  />
{/if}

{#if addAccessMethodFlow.isSystemOverlayVisible}
  <SystemOverlayBackdrop />
{/if}
