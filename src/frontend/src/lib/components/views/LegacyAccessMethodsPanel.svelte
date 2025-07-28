<script lang="ts">
  import Panel from "$lib/components/ui/Panel.svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import { getLastUsedAccessMethod } from "$lib/utils/accessMethods";
  import AccessMethodsList from "$lib/components/views/AccessMethodsList.svelte";

  const lastUsedAccessMethod = $derived(
    getLastUsedAccessMethod(
      identityInfo.authnMethods,
      identityInfo.openIdCredentials,
    ),
  );
  const authnMethods = $derived(identityInfo.legacyAuthnMethods);
  const isRemoveAccessMethodVisible = $derived(authnMethods.length > 1);
</script>

<Panel>
  <div class="flex flex-col justify-between gap-5 p-4 pb-5 md:flex-row">
    <div>
      <h2 class="text-text-primary mb-2 text-lg font-semibold">
        Legacy Access methods
      </h2>
      <p class="text-text-tertiary text-sm">
        Manage your legacy passkeys, and security keys.
      </p>
    </div>
  </div>
  <AccessMethodsList
    {authnMethods}
    {lastUsedAccessMethod}
    {isRemoveAccessMethodVisible}
  />
</Panel>
