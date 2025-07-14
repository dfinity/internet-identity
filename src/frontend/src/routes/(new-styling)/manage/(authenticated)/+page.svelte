<script lang="ts">
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import PlaceHolder from "$lib/components/ui/PlaceHolder.svelte";
  import AccessMethods from "$lib/components/views/AccessMethodsPanel.svelte";
  import IdentityInfoPanel from "$lib/components/views/IdentityInfoPanel.svelte";
  import { fade } from "svelte/transition";
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
