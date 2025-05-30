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

  const { children }: LayoutProps = $props();

  const status = $derived($authorizationStatusStore);

  onMount(() => {
    authorizationStore.init();
  });
</script>

<CenterLayout data-page="new-authorize-view">
  {#if status === "authenticating"}
    <AuthPanel>
      {@render children()}
    </AuthPanel>
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
