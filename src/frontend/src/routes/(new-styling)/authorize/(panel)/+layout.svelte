<script lang="ts">
  import type { LayoutProps } from "../$types";
  import AuthPanel from "$lib/components/layout/AuthPanel.svelte";
  import { fly, scale } from "svelte/transition";
  import { nonNullish } from "@dfinity/utils";
  import { lastUsedIdentitiesStore } from "$lib/stores/last-used-identities.store";

  const { children }: LayoutProps = $props();

  const selectedIdentity = $derived($lastUsedIdentitiesStore.selected);

  let animationWrapperRef = $state<HTMLElement>();
</script>

<div class="grid w-full flex-1 items-center max-sm:items-stretch sm:max-w-100">
  {#if nonNullish(selectedIdentity)}
    {#key selectedIdentity.identityNumber}
      <div
        bind:this={animationWrapperRef}
        class="col-start-1 row-start-1 flex flex-col"
        in:fly={{ duration: 300, y: 60, delay: 200 }}
        out:scale={{ duration: 500, start: 0.9 }}
        onoutrostart={() =>
          animationWrapperRef?.setAttribute("aria-hidden", "true")}
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
