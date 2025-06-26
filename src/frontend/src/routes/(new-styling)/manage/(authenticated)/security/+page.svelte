<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";
  import { InfoIcon, Plus, TriangleAlertIcon, Unlink } from "@lucide/svelte";
  import ListItem from "$lib/components/ui/ListItem.svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import RemoveOpenIdCredential from "$lib/components/views/RemoveOpenIdCredential.svelte";
  import AddOpenIdCredential from "$lib/components/views/AddOpenIdCredential.svelte";

  let displayAddCredentialDialog = $state(false);

  const totalAccessMethods = $derived<number>(
    identityInfo.authnMethods.length + identityInfo.openIdCredentials.length,
  );
</script>

<div class="text-text-primary">
  <h1 class="mb-4 text-4xl">Security</h1>
  <h2 class="mb-12 text-lg">
    Settings and recommendations to keep your identity secure
  </h2>
  <Panel>
    <div class="flex flex-col justify-between gap-5 p-4 md:flex-row">
      <div>
        <h3 class="mb-2 text-lg font-semibold">Access methods</h3>
        <h4>Manage your passkeys, security keys, and linked accounts.</h4>
      </div>

      <div>
        <Button
          onclick={() => {
            displayAddCredentialDialog = true;
          }}
          class="bg-bg-brand-solid text-text-primary-inversed text-[] top-0 flex w-full items-center justify-center gap-1 rounded-sm px-3.5 py-2 font-semibold md:max-w-fit"
          >Add <Plus size="1.25rem" /></Button
        >
      </div>
    </div>
    <ul>
      {#each identityInfo.authnMethods as authnMethod}
        <ListItem>
          <div class="min-w-8">
            <PasskeyIcon />
          </div>
          <div class="flex-1">
            <AccessMethod accessMethod={authnMethod} />
          </div>
          <!-- for layout consistency -->
          <!-- TODO: this is where we would add interactions like removal -->
          <div class="w-[52px]"></div>
        </ListItem>
      {/each}
      {#each identityInfo.openIdCredentials as credential}
        <ListItem class="!py-0">
          <div class="min-w-8 py-4">
            <GoogleIcon />
          </div>
          <div class="flex-1 py-4">
            <AccessMethod accessMethod={credential} />
          </div>

          {#if totalAccessMethods > 1}
            <Button
              variant="tertiary"
              iconOnly={true}
              onclick={() =>
                (identityInfo.removableOpenIdCredential = credential)}
            >
              <Unlink class="stroke-fg-error-secondary" />
            </Button>
          {/if}
        </ListItem>
      {/each}
    </ul>
  </Panel>
</div>

{#if identityInfo.removableOpenIdCredential}
  <RemoveOpenIdCredential
    onClose={() => (identityInfo.removableOpenIdCredential = null)}
  />
{/if}

{#if displayAddCredentialDialog}
  <AddOpenIdCredential onClose={() => (displayAddCredentialDialog = false)} />
{/if}
