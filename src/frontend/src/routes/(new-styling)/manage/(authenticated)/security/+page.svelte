<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import { Link2OffIcon, PlusIcon, Trash2Icon } from "@lucide/svelte";
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
  import { nonNullish } from "@dfinity/utils";
  import { handleError } from "$lib/components/utils/error";
  import AddAccessMethodWizard from "$lib/components/wizards/AddAccessMethodWizard.svelte";

  const MAX_PASSKEYS = 8;

  let isAddAccessMethodWizardOpen = $state(false);

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
    authnMethods.length > 1 || openIdCredentials.length > 1,
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
</script>

<h1 class="text-text-primary mb-4 text-3xl font-semibold">Security</h1>
<p class="text-text-tertiary text-md mb-12">
  Settings and recommendations to keep your identity secure
</p>
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
        <AccessMethod accessMethod={authnMethod} />
        <div class="flex items-center justify-center pr-4">
          {#if isRemoveAccessMethodVisible}
            <Button
              onclick={() => (identityInfo.removableAuthnMethod = authnMethod)}
              variant="tertiary"
              iconOnly
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

        <AccessMethod accessMethod={credential} />

        <div class="flex items-center justify-center pr-4">
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
