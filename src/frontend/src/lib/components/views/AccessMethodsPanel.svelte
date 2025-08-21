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
    isLegacyAuthnMethod,
    isWebAuthnMetaData,
    haveMultipleOrigins,
    isSameAccessMethod,
  } from "$lib/utils/accessMethods";
  import { AddAccessMethodWizard } from "$lib/components/wizards/addAccessMethod";
  import { getAuthnMethodAlias } from "$lib/utils/webAuthn";
  import { toaster } from "$lib/components/utils/toaster";

  const MAX_PASSKEYS = 8;

  let isAddAccessMethodWizardOpen = $state(false);
  let removableAuthnMethod = $state<AuthnMethodData | null>(null);
  let removableOpenIdCredential = $state<OpenIdCredential | null>(null);
  let renamableAuthnMethod = $state<AuthnMethodData | null>(null);

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
    nonNullish(removableAuthnMethod) &&
      nonNullish(lastUsedAccessMethod) &&
      isSameAccessMethod(removableAuthnMethod, lastUsedAccessMethod),
  );
  const isRemovableOpenIdCredentialCurrentAccessMethod = $derived(
    nonNullish(removableOpenIdCredential) &&
      nonNullish(lastUsedAccessMethod) &&
      isSameAccessMethod(removableOpenIdCredential, lastUsedAccessMethod),
  );

  const handleGoogleLinked = (credential: OpenIdCredential) => {
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
      // Optimistically remove the passkey
      removableOpenIdCredential = null;
      await identityInfo.removeGoogle({
        credential,
        isCurrent: isRemovableOpenIdCredentialCurrentAccessMethod,
      });
    } catch (error) {
      handleError(error);
    }
  };
  const handleRemovePasskey = async () => {
    if (!removableAuthnMethod) return;

    try {
      const authnMethod = removableAuthnMethod;
      // Optimistically remove the passkey
      removableAuthnMethod = null;
      await identityInfo.removePasskey({
        authnMethod,
        isCurrent: isRemovableAuthnMethodCurrentAccessMethod,
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
      nonNullish(lastUsedAccessMethod) &&
      isSameAccessMethod(accessMethod, lastUsedAccessMethod)
    );
  };

  const showPasskeyOrigin = $derived(haveMultipleOrigins(authnMethods));
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
              <EditIcon size="1.25rem" />
            </Button>
            {#if isRemoveAccessMethodVisible}
              <Button
                onclick={() => (removableAuthnMethod = authnMethod)}
                variant="tertiary"
                iconOnly
                aria-label={`Remove ${isCurrentAccessMethod(authnMethod) ? "current" : ""} passkey`}
                class="!text-fg-error-secondary"
              >
                <Trash2Icon size="1.25rem" />
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
              onclick={() => (removableOpenIdCredential = credential)}
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

{#if removableOpenIdCredential}
  <RemoveOpenIdCredential
    onRemove={handleRemoveOpenIdCredential}
    onClose={() => (removableOpenIdCredential = null)}
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
  {#if $ADD_ACCESS_METHOD}
    <AddAccessMethodWizard
      onGoogleLinked={handleGoogleLinked}
      onPasskeyRegistered={handlePasskeyRegistered}
      onOtherDeviceRegistered={handleOtherDeviceRegistered}
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
