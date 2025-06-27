<script lang="ts">
  import Panel from "$lib/components/ui/Panel.svelte";
  import Tooltip from "$lib/components/ui/Tooltip.svelte";
  import { ChevronRight, InfoIcon } from "@lucide/svelte";
  import identityInfo from "$lib/stores/identity-info.state.svelte";
  import PlaceHolder from "$lib/components/ui/PlaceHolder.svelte";
  import { fade } from "svelte/transition";
  import AccessMethod from "$lib/components/ui/AccessMethod.svelte";
  import { getLastUsedAccessMethod } from "$lib/utils/accessMethods";

  const lastUsedAccessMethod = $derived(
    getLastUsedAccessMethod(
      identityInfo.authnMethods,
      identityInfo.openIdCredentials,
    ),
  );
</script>

<div>
  <div class="mh-9 mb-4">
    <h1 class="text-text-primary text-3xl font-semibold">
      Welcome,
      {#if !identityInfo.name}
        <PlaceHolder class="mt-0.5 inline-block h-6 w-64" />
      {:else}
        <span transition:fade={{ delay: 30 }}>
          {identityInfo.name}!
        </span>
      {/if}
    </h1>
  </div>
  <h2 class="text-text-tertiary mb-12">
    Manage your identity and passkeys below.
  </h2>

  <Panel>
    <div class="p-4">
      <h3 class="text-text-primary mb-2 text-lg font-semibold">My Identity</h3>
      <h4 class="text-text-tertiary text-sm">
        Internet Identity is used to sign in securely and connect to apps with
        passkeys.
      </h4>
    </div>
    <div class="grid grid-cols-[1fr_2fr_min-content] grid-rows-2">
      <div
        class="border-border-tertiary col-span-3 grid grid-cols-subgrid border-t px-4 py-4"
      >
        <h5 class="text-text-tertiary flex min-w-30 items-center pr-4 text-sm">
          Identity Name
        </h5>
        <div class="flex items-center">
          {#if identityInfo.name}
            <h5
              class="text-text-primary text-sm font-semibold nth-[2]:hidden"
              transition:fade={{ delay: 30 }}
            >
              {identityInfo.name}
            </h5>
          {:else}
            <PlaceHolder class="mr-8 h-4 !rounded-sm" />
          {/if}
        </div>
        <div class="flex items-center justify-center">
          <Tooltip
            label="Your Identity name is currently not editable. It is only ever visible to you."
            ><InfoIcon
              size="20"
              class="text-text-primary stroke-fg-tertiary"
            /></Tooltip
          >
        </div>
      </div>
      <a
        class="border-border-tertiary not-disabled:hover:bg-bg-primary_hover col-span-3 grid grid-cols-subgrid rounded-b-2xl border-t px-4 py-4"
        href="manage/security"
        aria-label="Go to Security"
      >
        <h5
          class="text-text-tertiary flex min-w-30 items-center pr-4 text-sm nth-[2]:hidden"
        >
          Access Methods
        </h5>

        <AccessMethod accessMethod={lastUsedAccessMethod} />

        <div class="flex items-center justify-center">
          <ChevronRight class="text-text-primary" />
        </div>
      </a>
    </div>
  </Panel>
</div>
