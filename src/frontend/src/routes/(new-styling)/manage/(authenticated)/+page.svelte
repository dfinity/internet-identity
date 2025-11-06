<script lang="ts">
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import PlaceHolder from "$lib/components/ui/PlaceHolder.svelte";
  import IdentityInfoPanel from "$lib/components/views/IdentityInfoPanel.svelte";
  import { fade } from "svelte/transition";
  import type { PageProps } from "./$types";
  import { afterNavigate, invalidateAll, replaceState } from "$app/navigation";
  import { page } from "$app/state";
  import { nonNullish } from "@dfinity/utils";
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import { ConfirmAccessMethodWizard } from "$lib/components/wizards/confirmAccessMethod";
  import { handleError } from "$lib/components/utils/error";
  import { toaster } from "$lib/components/utils/toaster";
  import AccessMethodsPanel from "$lib/components/views/AccessMethodsPanel.svelte";
  import { toAccessMethods } from "./access/utils";

  const { data }: PageProps = $props();

  let pendingRegistrationId = $state(data.pendingRegistrationId);

  const name = $derived(
    data.identityInfo.name[0] ?? String(data.identityNumber),
  );
  const totalAccessMethods = $derived(
    toAccessMethods(data.identityInfo).length,
  );

  const handleConfirm = () => {
    toaster.success({
      title: "Passkey has been registered from another device.",
    });
    invalidateAll();
  };

  // Remove registration id from URL bar after assigning it to state
  afterNavigate(() => {
    if (page.url.searchParams.has("activate")) {
      replaceState("/manage", {});
    }
  });
</script>

<div>
  <div class="mh-9 mb-3">
    <h1 class="text-text-primary text-3xl font-medium">
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
  <h2 class="text-text-tertiary mb-12 text-base">
    Manage your identity and access methods.
  </h2>

  <div class="flex flex-col gap-6 lg:flex-row">
    <div class="flex-1">
      <IdentityInfoPanel {name} />
    </div>
    <div class="flex-1">
      <AccessMethodsPanel {totalAccessMethods} />
    </div>
  </div>
</div>

{#if nonNullish(pendingRegistrationId)}
  <Dialog onClose={() => (pendingRegistrationId = null)}>
    <ConfirmAccessMethodWizard
      registrationId={pendingRegistrationId}
      onConfirm={() => {
        handleConfirm();
        pendingRegistrationId = null;
      }}
      onError={(error) => {
        handleError(error);
        pendingRegistrationId = null;
      }}
    />
  </Dialog>
{/if}
