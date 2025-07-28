<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import { PlusIcon } from "@lucide/svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import AddOpenIdCredential from "$lib/components/views/AddOpenIdCredential.svelte";
  import { ADD_ACCESS_METHOD } from "$lib/state/featureFlags";
  import { invalidateAll } from "$app/navigation";
  import type {
    AuthnMethodData,
    OpenIdCredential,
  } from "$lib/generated/internet_identity_types";
  import { getLastUsedAccessMethod } from "$lib/utils/accessMethods";
  import { AddAccessMethodWizard } from "$lib/components/wizards/addAccessMethod";
  import AccessMethodsList from "$lib/components/views/AccessMethodsList.svelte";

  const MAX_PASSKEYS = 8;

  let isAddAccessMethodWizardOpen = $state(false);

  const lastUsedAccessMethod = $derived(
    getLastUsedAccessMethod(
      identityInfo.authnMethods,
      identityInfo.openIdCredentials,
    ),
  );
  const openIdCredentials = $derived(identityInfo.openIdCredentials);
  const authnMethods = $derived(identityInfo.newAuthnMethods);
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

  const handleGoogleLinked = (credential: OpenIdCredential) => {
    openIdCredentials.push(credential);
    invalidateAll();
  };
  const handlePasskeyRegistered = (authnMethod: AuthnMethodData) => {
    authnMethods.push(authnMethod);
    invalidateAll();
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
  <AccessMethodsList
    {authnMethods}
    {openIdCredentials}
    {lastUsedAccessMethod}
    {isRemoveAccessMethodVisible}
  />
</Panel>

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
