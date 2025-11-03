<script lang="ts">
  import IdentityInfoPanel from "$lib/components/views/IdentityInfoPanel.svelte";
  import type { PageProps } from "./$types";
  import AccessMethodsPanel from "$lib/components/views/AccessMethodsPanel.svelte";
  import { authenticatedStore } from "$lib/stores/authentication.store";

  const { data }: PageProps = $props();

  const name = $derived(
    data.identityInfo.name[0] ??
      $authenticatedStore.identityNumber.toString(10),
  );
  const totalAccessMethods = $derived(
    data.identityInfo.authn_methods.length +
      (data.identityInfo.openid_credentials[0]?.length ?? 0),
  );
</script>

<div>
  <div class="mh-9 mb-3">
    <h1 class="text-text-primary text-3xl font-medium">
      Welcome, {name}!
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
