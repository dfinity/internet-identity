<script lang="ts">
  import type { LayoutProps } from "./$types";
  import { onMount } from "svelte";
  import { ProgressRing } from "@skeletonlabs/skeleton-svelte";
  import CenterCard from "$lib/components/UI/CenterCard.svelte";
  import CenterContainer from "$lib/components/UI/CenterContainer.svelte";
  import { canisterConfig, canisterId } from "$lib/globals";
  import { authorizationStore } from "$lib/stores/authorization.store";

  const { children }: LayoutProps = $props();

  const { status } = authorizationStore;

  onMount(() => {
    authorizationStore.init({ canisterId, canisterConfig });
  });
</script>

<CenterContainer data-page="new-authorize-view">
  <CenterCard>
    {#if $status === "init" || $status === "waiting" || $status === "validating"}
      <div class="flex flex-col items-center justify-center gap-2">
        <ProgressRing
          value={null}
          size="size-14"
          meterStroke="stroke-surface-900-100"
        />
        <p class="opacity-60">Loading</p>
      </div>
    {:else if $status === "authenticating"}
      <div class="mb-8 flex flex-col gap-1">
        <h1 class="h1">Sign in</h1>
        <p class="p font-medium">
          to continue with <span class="font-bold">Example dapp</span>
        </p>
      </div>
      {@render children()}
    {:else if $status === "orphan" || $status === "closed" || $status === "invalid" || $status === "failure"}
      <div>Error</div>
    {:else if $status === "success"}
      <div>Success</div>
    {/if}
  </CenterCard>
</CenterContainer>
