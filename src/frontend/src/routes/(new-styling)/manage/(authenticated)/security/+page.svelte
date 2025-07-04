<script lang="ts">
  import Button from "$lib/components/ui/Button.svelte";
  import Panel from "$lib/components/ui/Panel.svelte";
  import { Link2Off, Plus } from "@lucide/svelte";
  import GoogleIcon from "$lib/components/icons/GoogleIcon.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";
  import PasskeyIcon from "$lib/components/icons/PasskeyIcon.svelte";
  import RemoveOpenIdCredential from "$lib/components/views/RemoveOpenIdCredential.svelte";
  import AddAccessMethodDialog from "$lib/components/views/AddAccessMethodDialog.svelte";
  import { CROSS_DEVICE_PASSKEYS } from "$lib/state/featureFlags";

  let displayAddAccessMethod = $state(false);
</script>

<div class="text-text-primary">
  <h1 class="mb-4 text-3xl font-semibold">Security</h1>
  <h2 class="text-text-tertiary mb-12 text-lg">
    Settings and recommendations to keep your identity secure
  </h2>
  <Panel>
    <div class="flex flex-col justify-between gap-5 p-4 pb-5 md:flex-row">
      <div>
        <h3 class="mb-2 text-lg font-semibold">Access methods</h3>
        <h4 class="text-text-tertiary text-sm">
          Manage your passkeys, security keys, and linked accounts.
        </h4>
      </div>

      {#if identityInfo.openIdCredentials.length === 0 || (identityInfo.authnMethods.length <= 8 && $CROSS_DEVICE_PASSKEYS)}
        <div>
          <Button
            onclick={() => {
              displayAddAccessMethod = true;
            }}
            class="bg-bg-brand-solid text-text-primary-inversed text-[] top-0 flex w-full items-center justify-center gap-1 rounded-sm px-3.5 py-2 font-semibold md:max-w-fit"
            >Add <Plus size="1.25rem" /></Button
          >
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
          <div class="flex min-w-8 items-center justify-center px-4 pr-4">
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
          <div class="flex min-w-8 items-center justify-center px-4 pr-4">
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
</div>

{#if identityInfo.removableOpenIdCredential}
  <RemoveOpenIdCredential
    credentialToBeRemoved={identityInfo.removableOpenIdCredential}
    onClose={() => (identityInfo.removableOpenIdCredential = null)}
  />
{/if}

{#if displayAddAccessMethod}
  <AddAccessMethodDialog onClose={() => (displayAddAccessMethod = false)} />
{/if}
