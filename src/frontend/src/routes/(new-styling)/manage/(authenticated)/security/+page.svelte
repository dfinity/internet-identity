<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import { Link2Off, Plus } from "@lucide/svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import RemoveOpenIdCredential from "$lib/components/views/RemoveOpenIdCredential.svelte";
  import AddOpenIdCredential from "$lib/components/views/AddOpenIdCredential.svelte";
  import { ADD_ACCESS_METHOD } from "$lib/state/featureFlags";
  import AddAccessMethod from "$lib/components/views/AddAccessMethod.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";

  const MAX_PASSKEYS = 8;

  let isAddAccessMethodDialogOpen = $state(false);

  const isMaxOpenIdCredentialsReached = $derived(
    identityInfo.openIdCredentials.length >= 1,
  );
  const isMaxPasskeysReached = $derived(
    identityInfo.authnMethods.length >= MAX_PASSKEYS,
  );
  const isAddAccessMethodVisible = $derived(
    $ADD_ACCESS_METHOD
      ? !isMaxOpenIdCredentialsReached || !isMaxPasskeysReached
      : !isMaxOpenIdCredentialsReached,
  );
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
          onclick={() => (isAddAccessMethodDialogOpen = true)}
          class="max-md:w-full"
        >
          <span>Add</span>
          <Plus size="1.25rem" />
        </Button>
      </div>
    {/if}
  </div>
  <div
    class={`grid grid-cols-[min-content_1fr_min-content] grid-rows-[${identityInfo.totalAccessMethods}]`}
  >
    {#each identityInfo.authnMethods as authnMethod}
      <div
        class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t py-4"
      >
        <div
          class="text-text-primary flex min-w-8 items-center justify-center px-4 pr-4"
        >
          <PasskeyIcon />
        </div>
        <AccessMethod accessMethod={authnMethod} />
        <!-- for layout consistency -->
        <!-- TODO: this is where we would add interactions like removal -->
        <div class="min-h-10 min-w-[52px]"></div>
      </div>
    {/each}
    {#each identityInfo.openIdCredentials as credential}
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
          {#if identityInfo.totalAccessMethods > 1}
            <Button
              variant="tertiary"
              iconOnly={true}
              onclick={() =>
                (identityInfo.removableOpenIdCredential = credential)}
            >
              <Link2Off class="stroke-fg-error-secondary" />
            </Button>
          {/if}
        </div>
      </div>
    {/each}
  </div>
</Panel>

{#if identityInfo.removableOpenIdCredential}
  <RemoveOpenIdCredential
    credentialToBeRemoved={identityInfo.removableOpenIdCredential}
    onClose={() => (identityInfo.removableOpenIdCredential = null)}
  />
{/if}

{#if isAddAccessMethodDialogOpen}
  {#if $ADD_ACCESS_METHOD}
    <Dialog onClose={() => (isAddAccessMethodDialogOpen = false)}>
      <AddAccessMethod continueWithGoogle={() => Promise.resolve()} />
    </Dialog>
  {:else}
    <AddOpenIdCredential
      onClose={() => (isAddAccessMethodDialogOpen = false)}
    />
  {/if}
{/if}
