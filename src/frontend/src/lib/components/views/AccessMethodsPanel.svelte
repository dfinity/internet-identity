<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import { EditIcon, Link2OffIcon, PlusIcon, Trash2Icon } from "@lucide/svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import RemoveOpenIdCredential from "$lib/components/views/RemoveOpenIdCredential.svelte";
  import AddOpenIdCredential from "$lib/components/views/AddOpenIdCredential.svelte";
  import { ADD_ACCESS_METHOD } from "$lib/state/featureFlags";
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
    getLastUsedAccessMethod,
    isWebAuthnMetaData,
  } from "$lib/utils/accessMethods";
  import { AddAccessMethodWizard } from "$lib/components/wizards/addAccessMethod";
  import { authnMethodEqual, getAuthnMethodAlias } from "$lib/utils/webAuthn";

  const MAX_PASSKEYS = 8;

  let isAddAccessMethodWizardOpen = $state(false);

  const lastUsedAccessMethod = $derived(
    getLastUsedAccessMethod(
      identityInfo.authnMethods,
      identityInfo.openIdCredentials,
    ),
  );
  const openIdCredentials = $derived(identityInfo.openIdCredentials);
  const authnMethods = $derived(identityInfo.authnMethods);
  const isMaxOpenIdCredentialsReached = $derived(
    identityInfo.openIdCredentials.length >= 1,
  );

  const isMaxPasskeysReached = $derived(
    identityInfo.authnMethods.length >= MAX_PASSKEYS,
  );
  const isUsingPasskeys = $derived(authnMethods.length > 0);
  const isAddAccessMethodVisible = $derived(
    $ADD_ACCESS_METHOD
      ? !isMaxOpenIdCredentialsReached || !isMaxPasskeysReached
      : !isMaxOpenIdCredentialsReached,
  );
  const isRemoveAccessMethodVisible = $derived(
    authnMethods.length + openIdCredentials.length > 1,
  );
  const isRemovableAuthnMethodCurrentAccessMethod = $derived(
    nonNullish(identityInfo.removableAuthnMethod) &&
      "WebAuthn" in identityInfo.removableAuthnMethod.authn_method &&
      identityInfo.isCurrentAccessMethod({
        passkey: {
          credentialId: new Uint8Array(
            identityInfo.removableAuthnMethod.authn_method.WebAuthn.credential_id,
          ),
        },
      }),
  );
  const isRemovableOpenIdCredentialCurrentAccessMethod = $derived(
    nonNullish(identityInfo.removableOpenIdCredential) &&
      identityInfo.isCurrentAccessMethod({
        openid: identityInfo.removableOpenIdCredential,
      }),
  );

  const handleGoogleLinked = (credential: OpenIdCredential) => {
    openIdCredentials.push(credential);
    invalidateAll();
  };
  const handlePasskeyRegistered = (authnMethod: AuthnMethodData) => {
    authnMethods.push(authnMethod);
    invalidateAll();
  };
  const handleRemoveOpenIdCredential = async () => {
    try {
      await identityInfo.removeGoogle();
    } catch (error) {
      handleError(error);
    }
  };
  const handleRemovePasskey = async () => {
    try {
      await identityInfo.removePasskey();
    } catch (error) {
      handleError(error);
    }
  };

  const handleRenamePasskey = async (newName: string) => {
    try {
      await identityInfo.renamePasskey(newName);
    } catch (error) {
      handleError(error);
    }
  };

  const isCurrentAccessMethod = (accessMethod: AuthnMethodData) => {
    return (
      nonNullish(lastUsedAccessMethod) &&
      isWebAuthnMetaData(lastUsedAccessMethod) &&
      authnMethodEqual(accessMethod, lastUsedAccessMethod)
    );
  };
</script>

<Panel>
  <div class="flex flex-col justify-between gap-5 p-4 pb-5 md:flex-row">
    <div>
      <h2 class="text-text-primary mb-2 text-lg font-semibold">
        Access methods
      </h2>
      <p class="text-text-tertiary text-sm">
        Manage your passkeys, security keys, and linked accounts.
      </p>
    </div>

    {#if isAddAccessMethodVisible}
      <div>
        <Button
          onclick={() => (isAddAccessMethodWizardOpen = true)}
          class="max-md:w-full"
        >
          <span>Add</span>
          <PlusIcon size="1.25rem" />
        </Button>
      </div>
    {/if}
  </div>
  <div
    class={`grid grid-cols-[min-content_1fr_min-content] grid-rows-[${identityInfo.totalAccessMethods}]`}
  >
    {#each authnMethods as authnMethod}
      <div
        class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t py-4"
      >
        <div
          class="text-text-primary flex min-w-8 items-center justify-center px-4 pr-4"
        >
          <PasskeyIcon />
        </div>
        <AccessMethod
          accessMethod={authnMethod}
          isCurrent={isCurrentAccessMethod(authnMethod)}
        />
        <div class="flex items-center justify-end gap-2 px-4">
          <Button
            onclick={() => (identityInfo.renamableAuthnMethod = authnMethod)}
            variant="tertiary"
            iconOnly
            aria-label={`Rename ${isCurrentAccessMethod(authnMethod) ? "current" : ""} passkey`}
          >
            <EditIcon size="1.25rem" />
          </Button>
          {#if isRemoveAccessMethodVisible}
            <Button
              onclick={() => (identityInfo.removableAuthnMethod = authnMethod)}
              variant="tertiary"
              iconOnly
              aria-label={`Remove ${isCurrentAccessMethod(authnMethod) ? "current" : ""} passkey`}
              class="!text-fg-error-secondary"
            >
              <Trash2Icon size="1.25rem" />
            </Button>
          {/if}
        </div>
      </div>
    {/each}
    {#each openIdCredentials as credential}
      <div
        class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t py-4"
      >
        <div
          class="text-text-primary flex min-w-8 items-center justify-center px-4 pr-4"
        >
          <GoogleIcon />
        </div>

        <AccessMethod
          accessMethod={credential}
          isCurrent={nonNullish(lastUsedAccessMethod) &&
            !isWebAuthnMetaData(lastUsedAccessMethod) &&
            lastUsedAccessMethod.sub === credential.sub}
        />

        <div class="flex items-center justify-end px-4">
          {#if isRemoveAccessMethodVisible}
            <Button
              onclick={() =>
                (identityInfo.removableOpenIdCredential = credential)}
              variant="tertiary"
              iconOnly
              class="!text-fg-error-secondary"
            >
              <Link2OffIcon size="1.25rem" />
            </Button>
          {/if}
        </div>
      </div>
    {/each}
  </div>
</Panel>

{#if identityInfo.removableOpenIdCredential}
  <RemoveOpenIdCredential
    onRemove={handleRemoveOpenIdCredential}
    onClose={() => (identityInfo.removableOpenIdCredential = null)}
    isCurrentAccessMethod={isRemovableOpenIdCredentialCurrentAccessMethod}
  />
{/if}

{#if identityInfo.removableAuthnMethod}
  <RemovePasskeyDialog
    onRemove={handleRemovePasskey}
    onClose={() => (identityInfo.removableAuthnMethod = null)}
    isCurrentAccessMethod={isRemovableAuthnMethodCurrentAccessMethod}
  />
{/if}

{#if identityInfo.renamableAuthnMethod}
  <RenamePasskeyDialog
    currentName={getAuthnMethodAlias(identityInfo.renamableAuthnMethod)}
    onRename={handleRenamePasskey}
    onClose={() => (identityInfo.renamableAuthnMethod = null)}
  />
{/if}

{#if isAddAccessMethodWizardOpen}
  {#if $ADD_ACCESS_METHOD}
    <AddAccessMethodWizard
      onGoogleLinked={handleGoogleLinked}
      onPasskeyRegistered={handlePasskeyRegistered}
      onOtherDeviceRegistered={invalidateAll}
      onClose={() => (isAddAccessMethodWizardOpen = false)}
      {isMaxOpenIdCredentialsReached}
      {isUsingPasskeys}
    />
  {:else}
    <AddOpenIdCredential
      onClose={() => (isAddAccessMethodWizardOpen = false)}
    />
  {/if}
{/if}
