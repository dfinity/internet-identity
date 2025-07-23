<script lang="ts">
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import PlaceHolder from "$lib/components/ui/PlaceHolder.svelte";
  import AccessMethods from "$lib/components/views/AccessMethodsPanel.svelte";
  import IdentityInfoPanel from "$lib/components/views/IdentityInfoPanel.svelte";
  import { fade } from "svelte/transition";
  import type { PageProps } from "./$types";
  import { afterNavigate, invalidateAll, replaceState } from "$app/navigation";
  import { page } from "$app/state";
  import { CONTINUE_FROM_ANOTHER_DEVICE } from "$lib/state/featureFlags";
  import { nonNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { ConfirmAccessMethodWizard } from "$lib/components/wizards/confirmAccessMethod";
  import { handleError } from "$lib/components/utils/error";

  const { data }: PageProps = $props();

  let pendingRegistrationId = $state(data.pendingRegistrationId);

  // Remove registration id from URL bar after assigning it to state
  afterNavigate(() => {
    page.url.searchParams;
    if (page.url.searchParams.has("activate")) {
      replaceState("/manage", {});
    }
  });
</script>

<div>
  <div class="mh-9 mb-4">
    <h1 class="text-text-primary text-3xl font-semibold">
      Welcome,
      {#if !identityInfo.name}
        <PlaceHolder class="mt-0.5 inline-block h-6 w-40 md:w-64" />
      {:else}
        <span transition:fade={{ delay: 30 }}>
          {identityInfo.name}!
        </span>
      {/if}
    </h1>
  </div>
  <h2 class="text-text-tertiary mb-12">
    Manage your identity and access methods below.
  </h2>

  <div class="flex flex-col gap-6">
    <IdentityInfoPanel />

    <AccessMethods />
  </div>
</div>

{#if $CONTINUE_FROM_ANOTHER_DEVICE && nonNullish(pendingRegistrationId)}
  <Dialog onClose={() => (pendingRegistrationId = null)}>
    <ConfirmAccessMethodWizard
      registrationId={pendingRegistrationId}
      onConfirm={() => {
        invalidateAll();
        pendingRegistrationId = null;
      }}
      onError={(error) => {
        handleError(error);
        pendingRegistrationId = null;
      }}
    />
  </Dialog>
{/if}
