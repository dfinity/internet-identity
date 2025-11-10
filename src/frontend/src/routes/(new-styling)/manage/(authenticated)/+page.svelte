<script lang="ts">
  import IdentityInfoPanel from "$lib/components/views/IdentityInfoPanel.svelte";
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
  import { t } from "$lib/stores/locale.store";
  import { Trans } from "$lib/components/locale";

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

<header class="flex flex-col gap-3">
  <h1 class="text-text-primary text-3xl font-medium">
    {$t`Welcome, ${name}!`}
  </h1>
  <p class="text-text-tertiary text-base">
    <Trans>Manage your identity and access methods.</Trans>
  </p>
</header>

<div
  class="mt-10 grid grid-cols-[repeat(auto-fill,minmax(24rem,1fr))] gap-5 max-sm:grid-cols-1"
>
  <IdentityInfoPanel {name} />
  <AccessMethodsPanel {totalAccessMethods} />
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
