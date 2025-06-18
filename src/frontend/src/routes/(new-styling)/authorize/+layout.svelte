<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { onMount } from "svelte";
  import CenterLayout from "$lib/components/layout/CenterLayout.svelte";
  import {
    authorizationStore,
    authorizationStatusStore,
  } from "$lib/stores/authorization.store";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import ProgressRing from "$lib/components/ui/ProgressRing.svelte";
  import { fly, scale } from "svelte/transition";
  import { nonNullish } from "@dfinity/utils";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";

  const { children }: LayoutProps = $props();

  const status = $derived($authorizationStatusStore);
  let divRef = $state<HTMLElement>();

  onMount(() => {
    authorizationStore.init();
  });
</script>

<CenterLayout data-page="new-authorize-view">
  {#if status === "authenticating"}
    <div
      class="grid w-full flex-1 items-center max-sm:items-stretch sm:max-w-100"
    >
      {#if nonNullish($lastUsedIdentitiesStore.selected)}
        {#key $lastUsedIdentitiesStore.selected.identityNumber}
          <div
            bind:this={divRef}
            class="col-start-1 row-start-1 flex flex-col"
            in:fly={{ duration: 300, y: 60, delay: 200 }}
            out:scale={{ duration: 500, start: 0.9 }}
            onoutrostart={() => divRef?.setAttribute("aria-hidden", "true")}
          >
            <AuthPanel>
              {@render children()}
            </AuthPanel>
          </div>
        {/key}
      {:else}
        <div class="col-start-1 row-start-1 flex flex-col">
          <AuthPanel>
            {@render children()}
          </AuthPanel>
        </div>
      {/if}
    </div>
  {:else if status === "authorizing"}
    <!-- Spinner is not shown for other statuses to avoid flicker -->
    <div class="flex flex-col items-center justify-center gap-4">
      <ProgressRing class="text-fg-primary size-14" />
      <p class="text-text-secondary text-lg">Redirecting to the app</p>
    </div>
  {:else if status === "orphan" || status === "closed" || status === "invalid" || status === "failure"}
    <div>Error</div>
  {:else if status === "success"}
    <div>Success</div>
  {/if}
</CenterLayout>
