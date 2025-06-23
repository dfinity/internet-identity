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
  import Dialog from "$lib/components/ui/Dialog.svelte";
  import Button from "$lib/components/ui/Button.svelte";
  import { RotateCcwIcon, CircleAlertIcon } from "@lucide/svelte";
  import FeaturedIcon from "$lib/components/ui/FeaturedIcon.svelte";

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
    {@const title = {
      orphan: "Missing request",
      closed: "Connection closed",
      invalid: "Invalid request",
      failure: "Something went wrong",
    }[status]}
    {@const description = {
      orphan:
        "There was an issue connecting with the application. Try a different browser; if the issue persists, contact the developer.",
      closed:
        "It seems like the connection with the service could not be established.",
      invalid: "It seems like an invalid authentication request was received.",
      failure:
        "Something went wrong during authentication. Authenticating service was notified and you may close this page.",
    }[status]}
    <Dialog>
      <FeaturedIcon size="lg" variant="error" class="mb-4 self-start">
        <CircleAlertIcon size="1.5rem" />
      </FeaturedIcon>
      <h1 class="text-text-primary mb-3 text-2xl font-medium">{title}</h1>
      <p class="text-md text-text-tertiary mb-6 font-medium">{description}</p>
      <Button onclick={() => window.close()} variant="secondary">
        <RotateCcwIcon size="1rem" />
        Return to app
      </Button>
    </Dialog>
  {/if}
</CenterLayout>
