<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import { EditIcon, Link2OffIcon, PlusIcon, Trash2Icon } from "@lucide/svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import RemoveOpenIdCredential from "$lib/components/views/RemoveOpenIdCredential.svelte";
  import { invalidateAll } from "$app/navigation";
  import type {
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import RemovePasskeyDialog from "$lib/components/views/RemovePasskeyDialog.svelte";
  import RenamePasskeyDialog from "$lib/components/views/RenamePasskeyDialog.svelte";
  import { nonNullish } from "@dfinity/utils";
  import { handleError } from "$lib/components/utils/error";
  import {
    isLegacyAuthnMethod,
    isWebAuthnMetaData,
    haveMultipleOrigins,
    isSameAccessMethod,
  } from "$lib/utils/accessMethods";
  import { AddAccessMethodWizard } from "$lib/components/wizards/addAccessMethod";
  import { getAuthnMethodAlias } from "$lib/utils/webAuthn";
  import { toaster } from "$lib/components/utils/toaster";
  import { openIdLogo, openIdName } from "$lib/utils/openID";
  import Tooltip from "../ui/Tooltip.svelte";
  import { accessMethods } from "$lib/derived/accessMethods.derived.svelte";

  let isAddAccessMethodWizardOpen = $state(false);
  let removableAuthnMethod = $state<AuthnMethodData | null>(null);
  let removableOpenIdCredential = $state<OpenIdCredential | null>(null);
  let renamableAuthnMethod = $state<AuthnMethodData | null>(null);

  const openIdCredentials = $derived(identityInfo.openIdCredentials);
  const authnMethods = $derived(identityInfo.authnMethods);
  const isUsingPasskeys = $derived(authnMethods.length > 0);
  const isRemoveAccessMethodVisible = $derived(
    authnMethods.length + openIdCredentials.length > 1,
  );
  const isRemovableAuthnMethodCurrentAccessMethod = $derived(
    nonNullish(removableAuthnMethod) &&
      nonNullish(accessMethods.lastUsedAccessMethod) &&
      isSameAccessMethod(
        removableAuthnMethod,
        accessMethods.lastUsedAccessMethod,
      ),
  );
  const isRemovableOpenIdCredentialCurrentAccessMethod = $derived(
    nonNullish(removableOpenIdCredential) &&
      nonNullish(accessMethods.lastUsedAccessMethod) &&
      isSameAccessMethod(
        removableOpenIdCredential,
        accessMethods.lastUsedAccessMethod,
      ),
  );

  const handleOpenIDLinked = (credential: OpenIdCredential) => {
    openIdCredentials.push(credential);
    invalidateAll();
  };
  const handlePasskeyRegistered = (authnMethod: AuthnMethodData) => {
    authnMethods.push(authnMethod);
    invalidateAll();
  };
  const handleOtherDeviceRegistered = () => {
    toaster.success({
      title: "Passkey has been registered from another device.",
    });
    invalidateAll();
  };
  const handleRemoveOpenIdCredential = async () => {
    if (!removableOpenIdCredential) return;

    try {
      const credential = removableOpenIdCredential;
      const isCurrent = isRemovableOpenIdCredentialCurrentAccessMethod;
      // Optimistically remove the passkey
      removableOpenIdCredential = null;
      await identityInfo.removeGoogle({
        credential,
        isCurrent,
      });
    } catch (error) {
      handleError(error);
    }
  };
  const handleRemovePasskey = async () => {
    if (!removableAuthnMethod) return;

    try {
      const authnMethod = removableAuthnMethod;
      const isCurrent = isRemovableAuthnMethodCurrentAccessMethod;
      // Optimistically remove the passkey
      removableAuthnMethod = null;
      await identityInfo.removePasskey({
        authnMethod,
        isCurrent,
      });
    } catch (error) {
      handleError(error);
    }
  };

  const handleRenamePasskey = async (newName: string) => {
    if (!renamableAuthnMethod) return;

    try {
      await identityInfo.renamePasskey({
        authnMethod: renamableAuthnMethod,
        newName,
      });
      renamableAuthnMethod = null;
    } catch (error) {
      handleError(error);
    }
  };

  const isCurrentAccessMethod = (accessMethod: AuthnMethodData) => {
    return (
      nonNullish(accessMethods.lastUsedAccessMethod) &&
      isSameAccessMethod(accessMethod, accessMethods.lastUsedAccessMethod)
    );
  };

  const showPasskeyOrigin = $derived(haveMultipleOrigins(authnMethods));
</script>

<div class="flex flex-row justify-end py-6 max-sm:flex-col">
  <div>
    <Tooltip
      label="You have reached the maximum number of access methods"
      hidden={!accessMethods.accessMethodsMaxReached}
    >
      <Button
        onclick={() => (isAddAccessMethodWizardOpen = true)}
        variant="secondary"
        disabled={accessMethods.accessMethodsMaxReached}
        class="max-sm:w-full"
      >
        <span>Add</span>
        <PlusIcon class="size-5" />
      </Button>
    </Tooltip>
  </div>
</div>
<div
  class={`grid grid-cols-[min-content_1fr_min-content] grid-rows-[${identityInfo.totalAccessMethods}]`}
>
  {#each authnMethods as authnMethod}
    <div
      class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t py-4"
    >
      <div
        class={[
          "flex min-w-8 items-center justify-center px-4 pr-4",
          isLegacyAuthnMethod(authnMethod)
            ? "text-text-disabled"
            : "text-text-primary",
        ]}
      >
        <PasskeyIcon />
      </div>
      <!-- TODO: Create Design's ListItem -->
      <AccessMethod
        accessMethod={authnMethod}
        isDisabled={isLegacyAuthnMethod(authnMethod)}
        isCurrent={isCurrentAccessMethod(authnMethod)}
        showOrigin={showPasskeyOrigin}
      />
      {#if !isLegacyAuthnMethod(authnMethod)}
        <div class="flex items-center justify-end gap-2 px-4">
          <Button
            onclick={() => (renamableAuthnMethod = authnMethod)}
            variant="tertiary"
            iconOnly
            aria-label={`Rename ${isCurrentAccessMethod(authnMethod) ? "current" : ""} passkey`}
          >
            <EditIcon class="size-5" />
          </Button>
          {#if isRemoveAccessMethodVisible}
            <Button
              onclick={() => (removableAuthnMethod = authnMethod)}
              variant="tertiary"
              iconOnly
              aria-label={`Remove ${isCurrentAccessMethod(authnMethod) ? "current" : ""} passkey`}
              class="!text-fg-error-secondary"
            >
              <Trash2Icon class="size-5" />
            </Button>
          {/if}
        </div>
      {:else}
        <!-- Necessary to keep the same height as iconOnly buttons -->
        <!-- TODO: Align with design what should set the height? -->
        <div class="size-10"></div>
      {/if}
    </div>
  {/each}
  {#each openIdCredentials as credential}
    {@const logo = openIdLogo(credential.iss, credential.metadata)}
    <div
      class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t py-4"
    >
      <div
        class="text-text-primary flex min-w-8 items-center justify-center px-4 pr-4"
      >
        {#if nonNullish(logo)}
          <div class="size-6">
            {@html logo}
          </div>
        {:else}
          <GoogleIcon />
        {/if}
      </div>

      <AccessMethod
        accessMethod={credential}
        isCurrent={nonNullish(accessMethods.lastUsedAccessMethod) &&
          !isWebAuthnMetaData(accessMethods.lastUsedAccessMethod) &&
          accessMethods.lastUsedAccessMethod.sub === credential.sub}
      />

      <div class="flex items-center justify-end px-4">
        {#if isRemoveAccessMethodVisible}
          <Button
            onclick={() => (removableOpenIdCredential = credential)}
            variant="tertiary"
            iconOnly
            class="!text-fg-error-secondary"
          >
            <Link2OffIcon class="size-5" />
          </Button>
        {/if}
      </div>
    </div>
  {/each}
</div>

{#if removableOpenIdCredential}
  <RemoveOpenIdCredential
    onRemove={handleRemoveOpenIdCredential}
    onClose={() => (removableOpenIdCredential = null)}
    openIDName={openIdName(
      removableOpenIdCredential.iss,
      removableOpenIdCredential.metadata,
    ) ?? "Google"}
    isCurrentAccessMethod={isRemovableOpenIdCredentialCurrentAccessMethod}
  />
{/if}

{#if removableAuthnMethod}
  <RemovePasskeyDialog
    onRemove={handleRemovePasskey}
    onClose={() => (removableAuthnMethod = null)}
    isCurrentAccessMethod={isRemovableAuthnMethodCurrentAccessMethod}
  />
{/if}

{#if renamableAuthnMethod}
  <RenamePasskeyDialog
    currentName={getAuthnMethodAlias(renamableAuthnMethod)}
    onRename={handleRenamePasskey}
    onClose={() => (renamableAuthnMethod = null)}
  />
{/if}

{#if isAddAccessMethodWizardOpen}
  <AddAccessMethodWizard
    onOpenIDLinked={handleOpenIDLinked}
    onPasskeyRegistered={handlePasskeyRegistered}
    onOtherDeviceRegistered={handleOtherDeviceRegistered}
    onClose={() => (isAddAccessMethodWizardOpen = false)}
    maxPasskeysReached={accessMethods.isMaxPasskeysReached}
    {openIdCredentials}
    {isUsingPasskeys}
  />
{/if}
